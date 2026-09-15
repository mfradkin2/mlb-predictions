"""Persistent memory: the part of the system that gets better on its own.

Three files per league, all committed back to the repository so that every
scheduled run starts from everything the previous runs learned:

``history/<league>_games.csv``   append-only archive of completed games.
    The ingest scripts only see a rolling window of the schedule. Merging each
    window into an archive means Elo and the form features keep growing a
    longer memory instead of restarting from 1500 every hour.

``history/<league>_ledger.csv``  every prediction, written *before* the game.
    This is the only genuinely leak-free scoreboard the system has. Season
    standings describe how a season turned out, so scoring a standings model
    against past games flatters it. A prediction recorded at 9am and graded at
    11pm cannot flatter anything.

``model_state/<league>.json``    tuned parameters, calibration and a run log.
    New parameters have to beat the incumbent on validation before they are
    adopted, so a bad hour cannot degrade the model.
"""
from __future__ import annotations

import os

from . import config
from .glm import PlattCalibrator, score
from .util import (clamp, logistic, logit, now_iso, num, parse_date, read_csv,
                   read_json, write_csv, write_json)

MODEL_VERSION = 3
LEDGER_MATURITY = 150      # graded pre-game predictions before the ledger rules
ARCHIVE_FIELDS = ['game_id', 'game_date', 'away_team', 'home_team',
                  'away_score', 'home_score', 'winner', 'first_seen']
LEDGER_FIELDS = ['game_id', 'game_date', 'away_team', 'home_team', 'predicted_at',
                 'model_version', 'p_final', 'p_elo', 'p_glm', 'p_prior',
                 'favored_team', 'away_score', 'home_score', 'winner',
                 'graded', 'correct']


def _key(row):
    gid = (row.get('game_id') or '').strip()
    if gid:
        return gid
    return '|'.join([(row.get('game_date') or '')[:10],
                     (row.get('away_team') or '').strip(),
                     (row.get('home_team') or '').strip()])


class LeagueMemory:
    def __init__(self, league_key, history_dir=None, state_dir=None):
        # Resolved on construction rather than bound as import-time defaults,
        # so the locations stay overridable (tests, alternate checkouts).
        history_dir = history_dir or config.HISTORY_DIR
        state_dir = state_dir or config.STATE_DIR
        self.league = league_key
        self.archive_path = os.path.join(history_dir, f'{league_key}_games.csv')
        self.ledger_path = os.path.join(history_dir, f'{league_key}_ledger.csv')
        self.state_path = os.path.join(state_dir, f'{league_key}.json')
        self.archive = {r_k: r for r_k, r in
                        ((_key(r), r) for r in read_csv(self.archive_path))}
        self.ledger = {r_k: r for r_k, r in
                       ((_key(r), r) for r in read_csv(self.ledger_path))}
        self.state = read_json(self.state_path, {})

    # ── archive ─────────────────────────────────────────────────────────────
    def merge_archive(self, rows):
        """Fold this run's completed games into the permanent archive."""
        added = 0
        for r in rows:
            if (r.get('status') or '') != 'Final':
                continue
            winner = (r.get('winner') or '').strip()
            hs, as_ = num(r.get('home_score')), num(r.get('away_score'))
            if not winner or hs is None or as_ is None:
                continue
            if hs == 0 and as_ == 0:          # ESPN placeholder for a no-show
                continue
            k = _key(r)
            if k in self.archive:
                continue
            self.archive[k] = {
                'game_id': (r.get('game_id') or '').strip(),
                'game_date': (r.get('game_date') or '')[:10],
                'away_team': (r.get('away_team') or '').strip(),
                'home_team': (r.get('home_team') or '').strip(),
                'away_score': int(as_), 'home_score': int(hs),
                'winner': winner, 'first_seen': now_iso()[:10],
            }
            added += 1
        return added

    def archive_rows(self):
        """Archive entries shaped like enriched-CSV rows, oldest first."""
        out = []
        for r in self.archive.values():
            out.append({
                'game_id': r.get('game_id', ''),
                'game_date': r.get('game_date', ''),
                'away_team': r.get('away_team', ''),
                'home_team': r.get('home_team', ''),
                'away_score': r.get('away_score', ''),
                'home_score': r.get('home_score', ''),
                'winner': r.get('winner', ''),
                'status': 'Final',
            })
        out.sort(key=lambda r: (r['game_date'], r['game_id']))
        return out

    def save_archive(self):
        rows = sorted(self.archive.values(),
                      key=lambda r: (r.get('game_date', ''), r.get('game_id', '')))
        write_csv(self.archive_path, rows, ARCHIVE_FIELDS)

    # ── ledger ──────────────────────────────────────────────────────────────
    def record(self, game, parts, favored):
        """Log a pre-game prediction. Existing entries are never overwritten:
        the first forecast we published is the one we are held to."""
        if game['final']:
            return
        k = _key({'game_id': game['game_id'], 'game_date': str(game['date']),
                  'away_team': game['away'], 'home_team': game['home']})
        if k in self.ledger and (self.ledger[k].get('p_final') or '') != '':
            return
        self.ledger[k] = {
            'game_id': game['game_id'],
            'game_date': str(game['date']),
            'away_team': game['away'],
            'home_team': game['home'],
            'predicted_at': now_iso(),
            'model_version': MODEL_VERSION,
            'p_final': round(parts['prob'], 4),
            'p_elo': round(parts['elo_prob'], 4) if parts.get('elo_prob') is not None else '',
            'p_glm': round(parts['glm_prob'], 4) if parts.get('glm_prob') is not None else '',
            'p_prior': round(parts['prior_prob'], 4) if parts.get('prior_prob') is not None else '',
            'favored_team': favored,
            'away_score': '', 'home_score': '', 'winner': '',
            'graded': '0', 'correct': '',
        }

    def grade(self):
        """Attach results to ledger entries whose games have since finished."""
        graded = 0
        for k, entry in self.ledger.items():
            if entry.get('graded') == '1':
                continue
            res = self.archive.get(k)
            if not res:
                continue
            winner = res.get('winner', '')
            if not winner:
                continue
            p = num(entry.get('p_final'))
            fav = entry.get('favored_team', '')
            entry['away_score'] = res.get('away_score', '')
            entry['home_score'] = res.get('home_score', '')
            entry['winner'] = winner
            entry['graded'] = '1'
            entry['correct'] = '1' if (fav and fav == winner) else '0'
            if p is None:
                entry['correct'] = ''
            graded += 1
        return graded

    def graded_rows(self):
        return [e for e in self.ledger.values() if e.get('graded') == '1'
                and e.get('winner')]

    def save_ledger(self):
        rows = sorted(self.ledger.values(),
                      key=lambda r: (r.get('game_date', ''), r.get('game_id', '')))
        write_csv(self.ledger_path, rows, LEDGER_FIELDS)

    # ── leak-free scoring of each component ─────────────────────────────────
    def component_scores(self):
        rows = self.graded_rows()
        if not rows:
            return {}, 0
        out = {}
        ys = [1 if r['winner'] == r['home_team'] else 0 for r in rows]
        for name in ('p_final', 'p_elo', 'p_glm', 'p_prior'):
            pairs = [(num(r.get(name)), y) for r, y in zip(rows, ys)]
            probs = [p for p, _ in pairs if p is not None]
            outs = [y for p, y in pairs if p is not None]
            if len(probs) >= 20:
                out[name[2:]] = score(probs, outs)
        return out, len(rows)

    def tune_trust_from_ledger(self):
        """Choose how far to lean on the learned model rather than the
        standings prior, using only predictions recorded before kickoff."""
        rows = self.graded_rows()
        usable = [r for r in rows
                  if num(r.get('p_elo')) is not None and num(r.get('p_prior')) is not None]
        if len(usable) < LEDGER_MATURITY:
            return None
        ys = [1 if r['winner'] == r['home_team'] else 0 for r in usable]
        model_p = []
        for r in usable:
            glm = num(r.get('p_glm'))
            elo = num(r.get('p_elo'))
            model_p.append(glm if glm is not None else elo)
        prior_p = [num(r.get('p_prior')) for r in usable]

        best_w, best_ll = None, None
        for step in range(21):
            w = step / 20.0
            probs = [logistic(w * logit(m) + (1 - w) * logit(p))
                     for m, p in zip(model_p, prior_p)]
            ll = score(probs, ys)['logloss']
            if ll is not None and (best_ll is None or ll < best_ll):
                best_ll, best_w = ll, w
        return {'trust': best_w, 'logloss': best_ll, 'n': len(usable)}

    def ledger_calibrator(self):
        """Recalibrate on graded pre-game predictions once there are enough."""
        rows = self.graded_rows()
        probs, ys = [], []
        for r in rows:
            p = num(r.get('p_final'))
            if p is None:
                continue
            probs.append(p)
            ys.append(1 if r['winner'] == r['home_team'] else 0)
        if len(probs) < LEDGER_MATURITY:
            return None
        cal = PlattCalibrator().fit(probs, ys)
        before = score(probs, ys)['logloss']
        after = score([cal.apply(p) for p in probs], ys)['logloss']
        if after is None or before is None or after >= before - 1e-4:
            return None
        return cal

    # ── state ───────────────────────────────────────────────────────────────
    def previous_best(self):
        return self.state.get('best') or {}

    def adopt(self, candidate, metrics, notes=''):
        """Champion/challenger: keep new parameters only when they validate
        better than the incumbent, so a noisy hour cannot make the model worse."""
        prev = self.previous_best()
        prev_ll = (prev.get('metrics') or {}).get('logloss')
        new_ll = (metrics or {}).get('logloss')
        prev_n = (prev.get('metrics') or {}).get('n') or 0
        new_n = (metrics or {}).get('n') or 0

        adopted = True
        reason = 'first fit'
        if prev_ll is not None and new_ll is not None:
            # Require a real improvement, or a materially bigger sample.
            if new_ll <= prev_ll - 1e-4:
                reason = f'log loss {prev_ll:.4f} -> {new_ll:.4f}'
            elif new_n >= prev_n * 1.15:
                reason = f'more evidence ({prev_n} -> {new_n} games)'
            else:
                adopted = False
                reason = f'kept incumbent (challenger {new_ll:.4f} vs {prev_ll:.4f})'
        elif new_ll is None:
            adopted = False
            reason = 'challenger produced no validation score'

        if adopted:
            self.state['best'] = {'params': candidate, 'metrics': metrics,
                                  'adopted_at': now_iso(), 'reason': reason}
        log = self.state.setdefault('runs', [])
        log.append({'at': now_iso(), 'adopted': adopted, 'reason': reason,
                    'metrics': metrics, 'notes': notes,
                    'model_version': MODEL_VERSION})
        self.state['runs'] = log[-60:]          # keep the trail bounded
        return adopted, reason

    def save_state(self, extra=None):
        self.state['league'] = self.league
        self.state['updated'] = now_iso()
        self.state['model_version'] = MODEL_VERSION
        if extra:
            self.state.update(extra)
        write_json(self.state_path, self.state)

    def learning_curve(self):
        """Rolling accuracy of graded pre-game picks — the 'is it improving?'
        chart on the site."""
        rows = sorted(self.graded_rows(), key=lambda r: r.get('game_date', ''))
        by_day = {}
        for r in rows:
            if r.get('correct') not in ('0', '1'):
                continue
            d = r.get('game_date', '')[:10]
            slot = by_day.setdefault(d, [0, 0])
            slot[1] += 1
            slot[0] += int(r['correct'])
        points, run_c, run_n = [], 0, 0
        for d in sorted(by_day):
            c, n = by_day[d]
            run_c += c
            run_n += n
            points.append({'date': d, 'correct': c, 'n': n,
                           'cum_acc': round(run_c / run_n, 4)})
        return points
