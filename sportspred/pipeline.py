"""Run one league end to end: ingest -> learn -> predict -> props -> payload."""
from __future__ import annotations

import os
from datetime import date, datetime, timedelta, timezone

from . import espn, model, props as props_mod
from . import config
from .config import LEAGUES
from .glm import score
from .learn import MODEL_VERSION, LeagueMemory
from .model import FEATURE_LABELS
from .util import (Http, clamp, format_eastern, now_iso, num, parse_iso, read_csv,
                   read_json, short_name, today_utc, write_json)

RECENT_DAYS = 21        # how far back the Results view can reach
LIVE_RECENT_DAYS = 4    # finished games kept in the first-paint payload
UPCOMING_DAYS = 14


def started(game, now=None):
    """Has this game's scheduled start already passed?"""
    now = now or datetime.now(timezone.utc)
    start = parse_iso((game.get('row') or {}).get('game_start_utc'))
    if start is not None:
        return start <= now
    # No timestamp: fall back to the calendar date, which is only wrong for
    # games still to be played later today.
    return game['date'] < now.date()


def is_preseason(game):
    return bool(game.get('preseason'))


def merge_sources(csv_rows, archive_rows):
    """Current window (authoritative, carries team stats) + everything older."""
    seen = set()
    merged = []
    for r in csv_rows:
        key = (r.get('game_id') or '').strip() or '|'.join([
            (r.get('game_date') or '')[:10], r.get('away_team', ''), r.get('home_team', '')])
        seen.add(key)
        merged.append(r)
    for r in archive_rows:
        key = (r.get('game_id') or '').strip() or '|'.join([
            (r.get('game_date') or '')[:10], r.get('away_team', ''), r.get('home_team', '')])
        if key not in seen:
            merged.append(r)
    merged.sort(key=lambda r: ((r.get('game_date') or '')[:10], r.get('game_id') or ''))
    return merged


def run(league_key, fetch_props=True, http=None, tune=True):
    cfg = LEAGUES[league_key]
    csv_path = os.path.join(os.path.dirname(config.DATA_DIR), cfg['csv_file'])
    csv_rows = read_csv(csv_path)

    memory = LeagueMemory(league_key)
    added = memory.merge_archive(csv_rows, league_key)
    rows = merge_sources(csv_rows, memory.archive_rows())

    # ── learn ───────────────────────────────────────────────────────────────
    prev = memory.previous_best()
    prev_elo = (prev.get('params') or {}).get('elo_params')
    trained = model.train(league_key, rows, cfg,
                          elo_params=prev_elo if not tune else None, tune=tune)

    candidate = {'elo_params': trained['elo_params'], 'l2': trained['l2'],
                 'blend_w': trained['blend_w'], 'calibration': trained['calibration']}
    adopted, reason = memory.adopt(candidate, trained.get('metrics') or {},
                                   notes=f'{trained["n_final"]} completed games')
    if not adopted and prev.get('params'):
        # Roll back to the incumbent, then rebuild with its parameters.
        keep = prev['params']
        trained = model.train(league_key, rows, cfg,
                              elo_params=keep.get('elo_params'), tune=False)
        trained['l2'] = keep.get('l2', trained['l2'])
        trained['blend_w'] = keep.get('blend_w', trained['blend_w'])
        trained['calibration'] = keep.get('calibration', trained['calibration'])

    # ── leak-free re-weighting from the prediction ledger ───────────────────
    memory.grade()
    ledger_trust = memory.tune_trust_from_ledger()
    trust_override = ledger_trust['trust'] if ledger_trust else None
    ledger_cal = memory.ledger_calibrator()
    if ledger_cal:
        trained['calibration'] = ledger_cal.to_dict()

    # ── predict every game, then freeze the answer ──────────────────────────
    #
    # The model is refit every hour, and the standings model behind it is
    # recomputed from *current* standings. Left alone, that means a finished
    # game's probability keeps moving, and the team it names as the favourite
    # can flip once the result is in the standings — the model reading back its
    # own answer. So the first forecast published for a game is written to the
    # ledger and is what the site shows from then on.
    records = trained['records']
    now = datetime.now(timezone.utc)
    for rec in records:
        game = rec['game']
        live = model.predict(rec, trained, cfg, league_key, trust_override)
        live = {k: (round(v, 4) if isinstance(v, float) else v) for k, v in live.items()}
        rec['live_prediction'] = live

        stored = memory.entry_for(game) if game['game_id'] else None
        if stored and num(stored.get('p_final')) is not None:
            prob = clamp(num(stored['p_final']), 0.0, 1.0)
            favored = stored.get('favored_team') or (
                game['home'] if prob >= 0.5 else game['away'])
            rec['prediction'] = {
                'prob': prob,
                'elo_prob': num(stored.get('p_elo'), live['elo_prob']),
                'glm_prob': num(stored.get('p_glm'), live.get('glm_prob')),
                'prior_prob': num(stored.get('p_prior'), live.get('prior_prob')),
                'trust': live.get('trust', 0.0),
            }
            rec['favored'] = favored
            rec['locked'] = True
            rec['pregame'] = stored.get('pregame') == '1'
            rec['locked_at'] = stored.get('predicted_at', '')
        else:
            favored = game['home'] if live['prob'] >= 0.5 else game['away']
            rec['prediction'] = live
            rec['favored'] = favored
            # Anything we are seeing for the first time after it kicked off is
            # marked as such, so it never counts toward the model's record.
            pregame = not game['final'] and not started(game, now)
            rec['locked'] = False
            rec['pregame'] = pregame
            rec['locked_at'] = ''
            if game['game_id']:
                memory.record(game, live, favored, pregame=pregame,
                              preseason=is_preseason(game))

    memory.grade()
    components, n_graded = memory.component_scores()

    # ── player props ────────────────────────────────────────────────────────
    prop_board, prop_status = {}, 'off'
    if fetch_props:
        prop_board, prop_status = build_props(league_key, cfg, records, trained,
                                              http or Http(budget_s=180))

    payload = build_payload(league_key, cfg, trained, memory, components,
                            n_graded, ledger_trust, prop_board, prop_status)

    memory.save_archive()
    memory.save_ledger()
    memory.save_state({
        'elo': trained['engine'].snapshot(),
        'last_metrics': trained.get('metrics'),
        'ledger': {'graded': n_graded, 'components': components,
                   'trust': ledger_trust},
        'archive_size': len(memory.archive),
        'new_games_this_run': added,
    })
    return payload, trained, memory


# ─────────────────────────────────────────────────────────────────────────────
#  Props
# ─────────────────────────────────────────────────────────────────────────────
def build_props(league_key, cfg, records, trained, http):
    """Fetch the player pool and price props for today's and upcoming games.

    Falls back to the previous run's cached pool whenever the feed is
    unavailable, so a blip upstream does not blank the prop board.
    """
    sport, league = cfg['espn_path'].split('/')
    cache_path = os.path.join(config.DATA_DIR, f'{league_key}_players.json')
    status = 'live'

    pool = {}
    try:
        pool = espn.fetch_athlete_stats(http, sport, league)
    except Exception:                        # noqa: BLE001
        pool = {}
    if not pool:
        try:
            index = espn.fetch_team_index(http, sport, league)
            pool = espn.fetch_team_rosters(http, sport, league, index.keys())
            status = 'roster'
        except Exception:                    # noqa: BLE001
            pool = {}
    if not pool:
        cached = read_json(cache_path, {})
        pool = cached.get('pool') or {}
        status = 'cached' if pool else 'unavailable'
    else:
        write_json(cache_path, {'updated': now_iso(), 'pool': pool}, indent=None)

    if not pool:
        return {}, status

    today = today_utc()
    horizon = today + timedelta(days=UPCOMING_DAYS)
    targets = [r for r in records
               if not r['game']['final']
               and today <= r['game']['date'] <= horizon
               and r['game']['game_id']]
    if not targets:
        return {}, status

    starters = {}
    if sport == 'baseball':
        for offset in range(0, 3):
            stamp = (today + timedelta(days=offset)).strftime('%Y%m%d')
            board = espn.fetch_scoreboard(http, sport, league, stamp)
            if board:
                starters.update(espn.probable_starters(board))

    env = props_mod.team_environment([r['game'] for r in records], cfg)
    out = {}
    for rec in targets:
        game = rec['game']
        try:
            out[game['game_id']] = props_mod.build_for_game(
                game, pool, env, cfg, sport, rec['prediction']['prob'],
                starters=starters.get(game['game_id']))
        except Exception:                    # noqa: BLE001
            continue
    return out, status


# ─────────────────────────────────────────────────────────────────────────────
#  Payload
# ─────────────────────────────────────────────────────────────────────────────
def build_payload(league_key, cfg, trained, memory, components, n_graded,
                  ledger_trust, prop_board, prop_status):
    today = today_utc()
    recent_cut = today - timedelta(days=RECENT_DAYS)
    horizon = today + timedelta(days=UPCOMING_DAYS)

    # The live board keeps a short tail of finished games; everything older
    # goes to a history file the Results view loads on demand, so the first
    # paint does not pay for a month of box scores.
    live_cut = today - timedelta(days=LIVE_RECENT_DAYS)
    games, history = [], []
    for rec in trained['records']:
        g = rec['game']
        if g['date'] > horizon:
            continue
        if g['date'] >= live_cut:
            games.append(game_json(rec, cfg, league_key,
                                   prop_board.get(g['game_id']), trained))
        elif g['final'] and not g.get('preseason') and g['date'] >= recent_cut:
            history.append(game_json(rec, cfg, league_key, None, trained))
    games.sort(key=lambda g: (g['date'], g['time'] or '', g['home']))
    history.sort(key=lambda g: g['date'], reverse=True)

    metrics = trained.get('metrics') or {}
    return {
        'league': league_key,
        'name': cfg['name'],
        'emoji': cfg['emoji'],
        'accent': cfg['accent'],
        'season': cfg['season_label'],
        'espn_path': cfg['espn_path'],
        'generated': now_iso(),
        'today': str(today),
        'model': {
            'version': MODEL_VERSION,
            'stage': trained.get('stage', 'empty'),
            'min_train': model.MIN_TRAIN + 20,
            'elo': trained['elo_params'],
            'blend_w': trained.get('blend_w'),
            'l2': trained.get('l2'),
            'trust': (ledger_trust or {}).get('trust'),
            'trust_source': 'prediction ledger' if ledger_trust else 'maturity schedule',
            'n_train': trained.get('n_final', 0),
            'archive': len(memory.archive),
            'validation': metrics,
            'reliability': trained.get('reliability') or [],
            'importance': [
                {**i, 'label': FEATURE_LABELS.get(i['feature'], i['feature'])}
                for i in (trained.get('importance') or [])
            ],
            'ledger': {'graded': n_graded, 'components': components},
            'runs': (memory.state.get('runs') or [])[-12:],
            'curve': memory.learning_curve()[-90:],
        },
        'stats': cfg['stats'],
        'props_status': prop_status,
        'preseason_excluded': sum(
            1 for r in trained['records']
            if r['game'].get('preseason') and r['game']['final']),
        'feature_labels': FEATURE_LABELS,
        'games': games,
        'history': history,
        'accuracy': accuracy_block(trained),
    }


def game_json(rec, cfg, league_key, props, trained):
    g = rec['game']
    row = g['row']
    pred = rec.get('prediction') or {}
    prob = pred.get('prob', 0.5)
    ctx = rec['context']

    # A scheduled game carries 0-0 in the feed; that is not a score.
    away_score = g['away_score'] if g['final'] else None
    home_score = g['home_score'] if g['final'] else None
    correct = None
    if g['final'] and g['winner']:
        correct = (rec.get('favored') == g['winner'])

    # Values only, in the order of payload['stats']; the labels and formats
    # live once at the league level rather than on all few-hundred games.
    team_stats = []
    for s in cfg['stats']:
        av = num(row.get(f'away_{s["key"]}'))
        hv = num(row.get(f'home_{s["key"]}'))
        team_stats.append([av, hv])
    if all(a is None and h is None for a, h in team_stats):
        team_stats = []

    # Feature key and signed contribution only — the human-readable labels
    # ship once per payload rather than on every game.
    drivers = [[d['feature'], round(d['contribution'], 3)]
               for d in model.edge_drivers(rec, trained)]

    out = {
        'id': g['game_id'],
        'date': str(g['date']),
        'preseason': bool(g.get('preseason')),
        'locked': bool(rec.get('locked')),
        'pregame': bool(rec.get('pregame')),
        'counted': bool(rec.get('pregame')) and not g.get('preseason'),
        'time': format_eastern(row.get('game_start_utc') or row.get('game_time'),
                               str(g['date'])),
        'away': g['away'],
        'home': g['home'],
        'away_s': short_name(g['away']),
        'home_s': short_name(g['home']),
        'away_rec': (row.get('away_record') or '').strip(),
        'home_rec': (row.get('home_record') or '').strip(),
        'final': g['final'],
        'winner': g['winner'],
        'away_score': int(away_score) if away_score is not None else None,
        'home_score': int(home_score) if home_score is not None else None,
        'home_prob': round(prob, 4),
        'away_prob': round(1 - prob, 4),
        'favored': rec.get('favored', ''),
        'pick_prob': round(max(prob, 1 - prob), 4),
        'conf': conf_tier(prob),
        'correct': correct,
        'components': {
            'elo': round(pred.get('elo_prob', 0.5), 4),
            'model': round(pred['glm_prob'], 4) if pred.get('glm_prob') is not None else None,
            'standings': round(pred['prior_prob'], 4) if pred.get('prior_prob') is not None else None,
            'trust': round(pred.get('trust', 0.0), 2),
        },
        'context': ctx,
        'stats': team_stats,
        'drivers': drivers,
    }
    if props:
        out['props'] = props
    return out


def conf_tier(prob):
    p = max(prob, 1 - prob)
    if p >= 0.66:
        return 'high'
    if p >= 0.57:
        return 'med'
    return 'low'


def accuracy_block(trained):
    """Two separate records, because they mean very different things.

    ``verified`` counts only games whose forecast was published before kickoff.
    It is the real track record, and it starts empty on a fresh install.

    ``backtest`` is the walk-forward validation score: the model applied to
    historical games using only what was known at the time. It is available
    immediately but it is a simulation, not a record.

    What is *not* reported is the model's hit rate on games it first saw after
    they had finished. Those picks are made with standings that already contain
    the result, so they score near-perfectly and mean nothing.
    """
    tiers = {'high': [0, 0], 'med': [0, 0], 'low': [0, 0]}
    rows, verified_correct, verified_total = [], 0, 0
    backfilled = 0

    for rec in trained['records']:
        g = rec['game']
        if not g['final'] or not g['winner'] or 'prediction' not in rec:
            continue
        if g.get('preseason'):
            continue
        ok = rec['favored'] == g['winner']
        if not rec.get('pregame'):
            backfilled += 1
            continue
        tier = conf_tier(rec['prediction']['prob'])
        tiers[tier][1] += 1
        tiers[tier][0] += int(ok)
        verified_total += 1
        verified_correct += int(ok)
        rows.append({'away': g['away'], 'home': g['home'], 'ok': ok})

    teams = {}
    for r in rows:
        for t in (r['away'], r['home']):
            slot = teams.setdefault(t, {'n': 0, 'ok': 0})
            slot['n'] += 1
            slot['ok'] += int(r['ok'])
    team_rows = sorted(
        ({'team': t, 'n': v['n'], 'ok': v['ok'],
          'acc': round(v['ok'] / v['n'], 4) if v['n'] else 0}
         for t, v in teams.items()),
        key=lambda t: (-t['n'], -t['acc']))

    backtest = None
    oos = trained.get('oos')
    if trained.get('stage') == 'trained' and oos:
        bt_tiers = {'high': [0, 0], 'med': [0, 0], 'low': [0, 0]}
        bt_ok = 0
        for prob, y in zip(oos['probs'], oos['ys']):
            hit = int((prob >= 0.5) == (y == 1))
            tier = conf_tier(prob)
            bt_tiers[tier][1] += 1
            bt_tiers[tier][0] += hit
            bt_ok += hit
        n = len(oos['ys'])
        backtest = {
            'total': n,
            'correct': bt_ok,
            'pct': round(bt_ok / n, 4) if n else None,
            'buckets': {k: {'n': v[1], 'ok': v[0],
                            'pct': round(v[0] / v[1], 4) if v[1] else None}
                        for k, v in bt_tiers.items()},
        }

    return {
        'verified': {
            'total': verified_total,
            'correct': verified_correct,
            'pct': round(verified_correct / verified_total, 4) if verified_total else None,
            'buckets': {k: {'n': v[1], 'ok': v[0],
                            'pct': round(v[0] / v[1], 4) if v[1] else None}
                        for k, v in tiers.items()},
        },
        'backtest': backtest,
        'backfilled': backfilled,
        'teams': team_rows,
    }
