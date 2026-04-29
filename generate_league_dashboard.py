#!/usr/bin/env python3
"""
Unified League Dashboard Generator — MLB / NHL / NBA / NFL
• List-format rows with collapsible live boxscore + pre-game stats
• Working filters on every tab including team breakdown in Accuracy
• Live polling with correct ET time display + in-game stats
"""
import csv, os, sys
from datetime import date, timedelta

_BASE = os.path.dirname(os.path.abspath(__file__))

# ══════════════════════════════════════════════════════════════════════════════
#  LEAGUE CONFIG
# ══════════════════════════════════════════════════════════════════════════════
LEAGUES = {
    'mlb': {
        'name': 'MLB', 'emoji': '⚾', 'accent': '#3b82f6',
        'espn_path': 'baseball/mlb',
        'script_dir': _BASE, 'csv_file': 'mlb_predictions.csv',
        'output_file': 'mlb.html', 'season_label': '2026 Season',
        'stats': [
            {'away': 'away_win_pct',     'home': 'home_win_pct',     'label': 'Win%',       'lower_better': False},
            {'away': 'away_pyth_pct',    'home': 'home_pyth_pct',    'label': 'Pyth Win%',  'lower_better': False},
            {'away': 'away_run_diff_pg', 'home': 'home_run_diff_pg', 'label': 'Run Diff/G', 'lower_better': False},
            {'away': 'away_rs_pg',       'home': 'home_rs_pg',       'label': 'RS/G',       'lower_better': False},
            {'away': 'away_era',         'home': 'home_era',         'label': 'Team ERA',   'lower_better': True},
        ],
    },
    'nhl': {
        'name': 'NHL', 'emoji': '🏒', 'accent': '#0ea5e9',
        'espn_path': 'hockey/nhl',
        'script_dir': _BASE, 'csv_file': 'nhl_2026_schedule_enriched.csv',
        'output_file': 'nhl.html', 'season_label': '2025–26 Season',
        'stats': [
            {'away': 'away_pts_pct',  'home': 'home_pts_pct',  'label': 'Points%', 'lower_better': False, 'suffix': '%'},
            {'away': 'away_gf_pg',    'home': 'home_gf_pg',    'label': 'GF/G',    'lower_better': False},
            {'away': 'away_ga_pg',    'home': 'home_ga_pg',    'label': 'GA/G',    'lower_better': True},
            {'away': 'away_pp_pct',   'home': 'home_pp_pct',   'label': 'PP%',     'lower_better': False, 'suffix': '%'},
            {'away': 'away_save_pct', 'home': 'home_save_pct', 'label': 'Save%',   'lower_better': False},
        ],
    },
    'nba': {
        'name': 'NBA', 'emoji': '🏀', 'accent': '#ef4444',
        'espn_path': 'basketball/nba',
        'script_dir': _BASE, 'csv_file': 'nba_2026_schedule_enriched.csv',
        'output_file': 'nba.html', 'season_label': '2025–26 Season',
        'stats': [
            {'away': 'away_win_pct',  'home': 'home_win_pct',  'label': 'Win%',    'lower_better': False},
            {'away': 'away_net_rtg',  'home': 'home_net_rtg',  'label': 'Net Rtg', 'lower_better': False},
            {'away': 'away_ppg',      'home': 'home_ppg',      'label': 'PPG',     'lower_better': False},
            {'away': 'away_opp_ppg',  'home': 'home_opp_ppg',  'label': 'Opp PPG', 'lower_better': True},
            {'away': 'away_pace',     'home': 'home_pace',     'label': 'Pace',    'lower_better': False},
        ],
    },
    'nfl': {
        'name': 'NFL', 'emoji': '🏈', 'accent': '#8b5cf6',
        'espn_path': 'football/nfl',
        'script_dir': _BASE, 'csv_file': 'nfl_2026_schedule_enriched.csv',
        'output_file': 'nfl.html', 'season_label': '2026 Season',
        'stats': [
            {'away': 'away_win_pct',    'home': 'home_win_pct',    'label': 'Win%',      'lower_better': False},
            {'away': 'away_pt_diff_pg', 'home': 'home_pt_diff_pg', 'label': 'Pt Diff/G', 'lower_better': False},
            {'away': 'away_ypg_off',    'home': 'home_ypg_off',    'label': 'YPG Off',   'lower_better': False},
            {'away': 'away_ypg_def',    'home': 'home_ypg_def',    'label': 'YPG Def',   'lower_better': True},
            {'away': 'away_to_margin',  'home': 'home_to_margin',  'label': 'TO Margin', 'lower_better': False},
        ],
    },
}

# ══════════════════════════════════════════════════════════════════════════════
#  HELPERS
# ══════════════════════════════════════════════════════════════════════════════
def safe_float(v, default=''):
    try:    return float(v)
    except: return default

def conf_info(prob):
    p = max(float(prob), 1 - float(prob)) if prob != '' else 0.5
    if p >= 0.68: return 'HIGH CONF', 'conf-high', 'high'
    if p >= 0.58: return 'MED CONF',  'conf-med',  'med'
    return              'LOW CONF',   'conf-low',  'low'

def fmt_stat(val, suffix=''):
    if val == '' or val is None: return '—'
    try:
        f = float(val)
        s = f'{f:.3f}' if suffix == '' and 0 < abs(f) < 1 else f'{f:.1f}'
        return s + suffix
    except:
        return str(val)

def fmt_date_short(d):
    try:
        from datetime import datetime
        return datetime.strptime(d[:10], '%Y-%m-%d').strftime('%b %-d')
    except:
        return d[:10]

# ══════════════════════════════════════════════════════════════════════════════
#  ROW RENDERER
# ══════════════════════════════════════════════════════════════════════════════
def render_row(row, cfg, tab_id, row_idx):
    away       = row.get('away_team', '')
    home       = row.get('home_team', '')
    away_p     = safe_float(row.get('away_win_probability', 0.5), 0.5)
    home_p     = safe_float(row.get('home_win_probability', 0.5), 0.5)
    is_final   = row.get('status', '') == 'Final'
    winner     = row.get('winner', '')
    away_score = row.get('away_score', '')
    home_score = row.get('home_score', '')
    away_rec   = row.get('away_record', '')
    home_rec   = row.get('home_record', '')
    game_time  = row.get('game_time', '')
    game_id    = row.get('game_id', '')
    game_date  = row.get('game_date', '')

    conf_label, conf_cls, conf_key = conf_info(home_p)
    away_pct  = round(away_p * 100, 1)
    home_pct  = round(home_p * 100, 1)
    pred_side = 'home' if home_p >= 0.5 else 'away'
    pred_name = home   if pred_side == 'home' else away
    pred_prob = max(home_p, away_p)

    result_key  = ''
    result_pill = ''
    if is_final and away_score and home_score:
        try:
            aw, hw  = int(float(away_score)), int(float(home_score))
            actual  = 'away' if aw > hw else 'home'
            correct = actual == pred_side
            result_key  = 'correct' if correct else 'wrong'
            result_pill = f'<span class="pill {"pill-correct" if correct else "pill-wrong"}">{"✓" if correct else "✗"}</span>'
        except: pass

    if is_final:
        center_html = (f'<div class="row-score" id="ctr-{game_id}">'
                       f'<span class="sc">{away_score}–{home_score}</span>'
                       f'<span class="sc-lbl">FINAL</span></div>')
        status_key = 'final'
    else:
        center_html = (f'<div class="row-time" id="ctr-{game_id}">'
                       f'{game_time or "TBD"}</div>')
        status_key = 'scheduled'

    away_cls = ' winner' if (is_final and winner == away) else (' fav' if pred_side == 'away' else '')
    home_cls = ' winner' if (is_final and winner == home) else (' fav' if pred_side == 'home' else '')

    prob_bar = (f'<div class="row-bar">'
                f'<div class="rb-away" style="width:{away_pct}%"></div>'
                f'<div class="rb-home" style="width:{home_pct}%"></div>'
                f'</div>')

    # Pre-game stats table (shown when no live boxscore available)
    stat_rows = ''
    for s in cfg['stats']:
        av, hv = row.get(s['away'], ''), row.get(s['home'], '')
        suffix = s.get('suffix', '')
        lb     = s.get('lower_better', False)
        af, hf = safe_float(av, None), safe_float(hv, None)
        a_cls = h_cls = ''
        if af is not None and hf is not None and af != hf:
            a_better = (af < hf) if lb else (af > hf)
            a_cls = 'better' if a_better     else 'worse'
            h_cls = 'better' if not a_better else 'worse'
        stat_rows += (f'<tr>'
                      f'<td class="ds-away {a_cls}">{fmt_stat(av,suffix)}</td>'
                      f'<td class="ds-lbl">{s["label"]}</td>'
                      f'<td class="ds-home {h_cls}">{fmt_stat(hv,suffix)}</td>'
                      f'</tr>')

    pregame_table = f'''<table class="detail-stats pregame-stats">
      <thead><tr>
        <th class="ds-away">{away}</th>
        <th class="ds-lbl">SEASON STATS</th>
        <th class="ds-home">{home}</th>
      </tr></thead>
      <tbody>{stat_rows}</tbody>
    </table>'''

    detail_id = f'det-{tab_id}-{row_idx}'

    return f'''\
<div class="game-row"
     data-conf="{conf_key}"
     data-pick="{pred_side}"
     data-result="{result_key}"
     data-date="{game_date[:10]}"
     data-status="{status_key}"
     data-game-id="{game_id}"
     data-away="{away}"
     data-home="{home}"
     data-pred="{pred_side}"
     data-espn-path="{cfg.get('espn_path','')}">
  <div class="row-main" onclick="toggleRow('{detail_id}',this,'{game_id}','{status_key}')">
    <span class="row-date">{fmt_date_short(game_date)}</span>
    <div class="row-teams">
      <div class="row-team{away_cls}">
        <span class="rtn">{away}</span>
        <span class="rrec">{away_rec}</span>
      </div>
      {center_html}
      <div class="row-team right{home_cls}">
        <span class="rtn">{home}</span>
        <span class="rrec">{home_rec}</span>
      </div>
    </div>
    <div class="row-meta">
      {prob_bar}
      <span class="row-pick">{"→" if pred_side=="home" else "←"} {pred_name} {round(pred_prob*100)}%</span>
      <span class="pill {conf_cls}">{conf_label}</span>
      <span class="result-area">{result_pill}</span>
      <span class="expand-btn">▾</span>
    </div>
  </div>
  <div class="row-detail" id="{detail_id}">
    <div class="detail-inner">{pregame_table}</div>
  </div>
</div>'''

# ══════════════════════════════════════════════════════════════════════════════
#  ACCURACY — team breakdown computed in Python
# ══════════════════════════════════════════════════════════════════════════════
def compute_team_breakdown(finals):
    teams = {}
    for row in finals:
        away    = row.get('away_team', '')
        home    = row.get('home_team', '')
        winner  = row.get('winner', '')
        favored = row.get('favored_team', '')
        if not winner or not favored: continue
        ok = (winner == favored)
        for team in [away, home]:
            if not team: continue
            if team not in teams:
                teams[team] = {'total': 0, 'correct': 0, 'fav': 0, 'dog': 0,
                               'fav_correct': 0, 'dog_correct': 0}
            teams[team]['total'] += 1
            if ok: teams[team]['correct'] += 1
            if favored == team:
                teams[team]['fav'] += 1
                if ok: teams[team]['fav_correct'] += 1
            else:
                teams[team]['dog'] += 1
                if ok: teams[team]['dog_correct'] += 1
    result = []
    for t, d in teams.items():
        acc = d['correct'] / d['total'] * 100 if d['total'] else 0
        result.append((t, d['total'], d['correct'], acc,
                       d['fav'], d['fav_correct'], d['dog'], d['dog_correct']))
    result.sort(key=lambda x: (-x[1], -x[3]))
    return result

def render_accuracy(finals, cfg):
    correct = wrong = 0
    buckets = {'high': [0,0], 'med': [0,0], 'low': [0,0]}
    trows   = []
    all_teams = set()

    for row in finals:
        hp = safe_float(row.get('home_win_probability',''), '')
        ap = safe_float(row.get('away_win_probability',''), '')
        if hp == '' or ap == '': continue
        winner  = row.get('winner','')
        favored = row.get('favored_team','')
        away    = row.get('away_team','')
        home    = row.get('home_team','')
        if not winner or not favored: continue
        all_teams.add(away); all_teams.add(home)
        ok  = winner == favored
        cl, cc, ck = conf_info(hp)
        buckets[ck][1] += 1
        if ok: correct += 1; buckets[ck][0] += 1
        else:  wrong   += 1
        pp = max(hp, ap)
        sc = f'{row.get("away_score","")}–{row.get("home_score","")}'
        rk = 'correct' if ok else 'wrong'
        trows.append(
            f'<tr class="acc-row" data-conf="{ck}" data-result="{rk}"'
            f' data-away="{away}" data-home="{home}">'
            f'<td>{row.get("game_date","")[:10]}</td>'
            f'<td class="matchup-cell">{away} <span class="vs-sep">@</span> {home}</td>'
            f'<td>{favored} <span class="prob-txt">({round(pp*100)}%)</span></td>'
            f'<td>{winner} <span class="prob-txt">({sc})</span></td>'
            f'<td><span class="pill {cc}">{cl}</span></td>'
            f'<td class="{"acc-ok" if ok else "acc-no"}">{"✓ Correct" if ok else "✗ Wrong"}</td>'
            f'</tr>'
        )

    total   = correct + wrong
    overall = f'{correct/total*100:.1f}%' if total else '—'
    hi_acc  = f'{buckets["high"][0]/buckets["high"][1]*100:.1f}%' if buckets['high'][1] else '—'
    md_acc  = f'{buckets["med"][0]/buckets["med"][1]*100:.1f}%'   if buckets['med'][1]  else '—'
    lo_acc  = f'{buckets["low"][0]/buckets["low"][1]*100:.1f}%'   if buckets['low'][1]  else '—'

    # Team breakdown table
    breakdown = compute_team_breakdown(finals)
    team_rows = ''
    for t, tot, cor, acc, fav, fav_c, dog, dog_c in breakdown:
        fav_pct = f'{fav_c/fav*100:.0f}%' if fav else '—'
        dog_pct = f'{dog_c/dog*100:.0f}%' if dog else '—'
        acc_cls = 'acc-ok' if acc >= 60 else ('acc-warn' if acc >= 50 else 'acc-no')
        team_rows += (
            f'<tr class="team-row" data-team="{t}">'
            f'<td class="team-name-cell">{t}</td>'
            f'<td>{tot}</td>'
            f'<td class="{acc_cls}">{acc:.0f}%</td>'
            f'<td>{cor}/{tot}</td>'
            f'<td>{fav_pct} <span class="prob-txt">({fav_c}/{fav})</span></td>'
            f'<td>{dog_pct} <span class="prob-txt">({dog_c}/{dog})</span></td>'
            f'</tr>'
        )

    # Team select options for filter
    team_opts = '<option value="all">All Teams</option>'
    for t, *_ in breakdown:
        team_opts += f'<option value="{t}">{t}</option>'

    return f'''
<div class="acc-grid">
  <div class="acc-card"><div class="acc-big">{overall}</div><div class="acc-lbl">Overall Accuracy</div></div>
  <div class="acc-card"><div class="acc-big">{correct}/{total}</div><div class="acc-lbl">Correct Picks</div></div>
  <div class="acc-card acc-hi"><div class="acc-big">{hi_acc}</div><div class="acc-lbl">High Conf ({buckets["high"][1]})</div></div>
  <div class="acc-card acc-md"><div class="acc-big">{md_acc}</div><div class="acc-lbl">Med Conf ({buckets["med"][1]})</div></div>
  <div class="acc-card acc-lo"><div class="acc-big">{lo_acc}</div><div class="acc-lbl">Low Conf ({buckets["low"][1]})</div></div>
</div>

<div class="acc-section-tabs">
  <button class="acc-stab active" onclick="switchAccView(this,'games')">Game Log</button>
  <button class="acc-stab" onclick="switchAccView(this,'teams')">Team Breakdown</button>
</div>

<div id="acc-view-games">
  <div class="filter-bar" id="fb-accuracy">
    <div class="fg">
      <span class="fl">Confidence</span>
      <button class="fb active" data-tab="accuracy" data-filter="conf"   data-value="all"     onclick="setFilter(this)">All</button>
      <button class="fb"        data-tab="accuracy" data-filter="conf"   data-value="high"    onclick="setFilter(this)">High</button>
      <button class="fb"        data-tab="accuracy" data-filter="conf"   data-value="med"     onclick="setFilter(this)">Med</button>
      <button class="fb"        data-tab="accuracy" data-filter="conf"   data-value="low"     onclick="setFilter(this)">Low</button>
    </div>
    <div class="fg">
      <span class="fl">Result</span>
      <button class="fb active" data-tab="accuracy" data-filter="result" data-value="all"     onclick="setFilter(this)">All</button>
      <button class="fb"        data-tab="accuracy" data-filter="result" data-value="correct" onclick="setFilter(this)">✓ Correct</button>
      <button class="fb"        data-tab="accuracy" data-filter="result" data-value="wrong"   onclick="setFilter(this)">✗ Wrong</button>
    </div>
    <div class="fg">
      <span class="fl">Team</span>
      <select class="team-select" id="team-filter-accuracy" onchange="setTeamFilter(this,'accuracy')">{team_opts}</select>
    </div>
    <span class="game-count" id="count-accuracy">{total} games</span>
  </div>
  <table class="acc-table">
    <thead><tr>
      <th>Date</th><th>Matchup</th><th>Predicted</th>
      <th>Actual</th><th>Conf</th><th>Result</th>
    </tr></thead>
    <tbody id="acc-tbody">{''.join(trows[:300])}</tbody>
  </table>
</div>

<div id="acc-view-teams" style="display:none">
  <div class="filter-bar">
    <div class="fg">
      <span class="fl">Sort by</span>
      <button class="fb active" onclick="sortTeams('total',this)">Games</button>
      <button class="fb"        onclick="sortTeams('acc',this)">Accuracy</button>
      <button class="fb"        onclick="sortTeams('fav',this)">As Favorite</button>
      <button class="fb"        onclick="sortTeams('dog',this)">As Underdog</button>
    </div>
    <div class="fg">
      <span class="fl">Search</span>
      <input class="team-search" id="team-search" placeholder="Filter team…" oninput="filterTeamSearch(this)">
    </div>
    <span class="game-count" id="count-teams">{len(breakdown)} teams</span>
  </div>
  <table class="acc-table sortable-team-table" id="team-breakdown-table">
    <thead><tr>
      <th>Team</th>
      <th>Games</th>
      <th>Accuracy</th>
      <th>W–L</th>
      <th>As Favorite</th>
      <th>As Underdog</th>
    </tr></thead>
    <tbody id="team-tbody">{team_rows}</tbody>
  </table>
</div>'''

# ══════════════════════════════════════════════════════════════════════════════
#  FILTER BAR (Today / Recent / Upcoming)
# ══════════════════════════════════════════════════════════════════════════════
FILTER_DEFS = {
    'today': [
        ('conf', 'Confidence', [('all','All'),('high','High'),('med','Med'),('low','Low')]),
        ('pick', 'Favored',    [('all','All'),('home','Home'),('away','Away')]),
    ],
    'recent': [
        ('result','Result',     [('all','All'),('correct','✓ Correct'),('wrong','✗ Wrong')]),
        ('conf',  'Confidence', [('all','All'),('high','High'),('med','Med'),('low','Low')]),
        ('days',  'Period',     [('3','Last 3 days'),('7','Last 7 days'),('10','Last 10 days')]),
    ],
    'upcoming': [
        ('conf', 'Confidence', [('all','All'),('high','High'),('med','Med'),('low','Low')]),
        ('days', 'Window',     [('1','Tomorrow'),('3','Next 3 days'),('7','Next week'),('14','All')]),
        ('pick', 'Favored',    [('all','All'),('home','Home'),('away','Away')]),
    ],
}

def render_filter_bar(tab_id, total):
    filters = FILTER_DEFS.get(tab_id, [])
    groups = ''
    for fid, label, opts in filters:
        btns = ''
        for i, (val, text) in enumerate(opts):
            active = ' active' if i == 0 else ''
            btns += (f'<button class="fb{active}" data-tab="{tab_id}" '
                     f'data-filter="{fid}" data-value="{val}" '
                     f'onclick="setFilter(this)">{text}</button>')
        groups += f'<div class="fg"><span class="fl">{label}</span>{btns}</div>'
    return (f'<div class="filter-bar" id="fb-{tab_id}">'
            f'{groups}'
            f'<span class="game-count" id="count-{tab_id}">{total} game{"s" if total!=1 else ""}</span>'
            f'</div>')

def render_game_section(games, cfg, tab_id):
    emoji_map = {'mlb':'⚾','nhl':'🏒','nba':'🏀','nfl':'🏈'}
    if not games:
        lk = cfg.get('key', tab_id.replace('-',''))
        e  = emoji_map.get(lk, '🏟')
        return (f'<div class="empty"><span class="e-icon">{e}</span>'
                f'No {cfg["name"]} games to show.</div>')
    bar  = render_filter_bar(tab_id, len(games))
    rows = ''.join(render_row(g, cfg, tab_id, i) for i, g in enumerate(games))
    return f'{bar}<div class="game-list" id="list-{tab_id}">{rows}</div>'

# ══════════════════════════════════════════════════════════════════════════════
#  CSS
# ══════════════════════════════════════════════════════════════════════════════
CSS = '''\
:root {
  --bg:     #0b0f17;
  --bg2:    #111827;
  --bg3:    #1a2235;
  --border: #1f2d42;
  --text:   #e2eaf5;
  --muted:  #5d7390;
  --dim:    #374d66;
}
*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
body {
  background: var(--bg); color: var(--text);
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  font-size: 14px; line-height: 1.45;
}

/* ── Header ── */
.page-header {
  display: flex; align-items: center; gap: 14px;
  padding: 14px 24px; background: var(--bg2);
  border-bottom: 1px solid var(--border);
}
.hdr-emoji { font-size: 24px; }
.hdr-info  { flex: 1; }
.hdr-name  { font-size: 16px; font-weight: 700; color: ACCENT; }
.hdr-sub   { font-size: 11px; color: var(--muted); margin-top: 2px; }
.hdr-right { display: flex; flex-direction: column; align-items: flex-end; gap: 3px; }
.hdr-date  { font-size: 11px; color: var(--dim); }
.live-status { font-size: 10px; font-weight: 700; color: var(--dim); transition: color .3s; }
.live-status.is-live { color: #4ade80; }

/* ── Sub-nav ── */
.sub-nav {
  display: flex; background: var(--bg2);
  border-bottom: 1px solid var(--border); padding: 0 24px;
}
.sub-tab {
  padding: 11px 20px; font-size: 13px; font-weight: 500; color: var(--muted);
  border-bottom: 2px solid transparent; cursor: pointer;
  transition: color .15s, border-color .15s; user-select: none; white-space: nowrap;
}
.sub-tab:hover { color: var(--text); }
.sub-tab.active { color: var(--text); border-color: ACCENT; }
.sub-panel { display: none; }
.sub-panel.active { display: block; }

/* ── Filter bar ── */
.filter-bar {
  display: flex; flex-wrap: wrap; align-items: center; gap: 6px 14px;
  padding: 10px 24px; background: var(--bg2);
  border-bottom: 1px solid var(--border);
  position: sticky; top: 0; z-index: 10;
}
.fg { display: flex; align-items: center; gap: 4px; }
.fl { font-size: 10px; font-weight: 700; color: var(--muted);
      text-transform: uppercase; letter-spacing: .4px; margin-right: 2px; white-space: nowrap; }
.fb {
  font-size: 11px; font-weight: 500; padding: 3px 10px; border-radius: 20px;
  background: transparent; color: var(--muted);
  border: 1px solid var(--border); cursor: pointer; transition: all .15s; white-space: nowrap;
}
.fb:hover  { color: var(--text); border-color: var(--dim); }
.fb.active { background: ACCENT; color: #fff; border-color: ACCENT; font-weight: 600; }
.team-select {
  font-size: 11px; padding: 3px 8px; border-radius: 6px;
  background: var(--bg3); color: var(--text);
  border: 1px solid var(--border); cursor: pointer; max-width: 180px;
}
.team-search {
  font-size: 11px; padding: 3px 10px; border-radius: 6px;
  background: var(--bg3); color: var(--text);
  border: 1px solid var(--border); width: 150px; outline: none;
}
.team-search::placeholder { color: var(--muted); }
.game-count { margin-left: auto; font-size: 11px; color: var(--muted); white-space: nowrap; }

/* ── Game list ── */
.game-list { max-width: 1100px; margin: 0 auto; padding: 12px 24px 32px; }

/* ── Game row ── */
.game-row {
  background: var(--bg2); border: 1px solid var(--border);
  border-radius: 8px; margin-bottom: 6px; overflow: hidden; transition: border-color .15s;
}
.game-row:hover { border-color: ACCENTaa; }
.game-row.is-live { border-color: rgba(74,222,128,.4); }
@keyframes flash { 0%,100%{opacity:1;} 50%{opacity:.5;} }
.game-row.just-updated { animation: flash .5s ease; }

.row-main {
  display: flex; align-items: center; gap: 10px;
  padding: 10px 14px; cursor: pointer; user-select: none;
}
.row-main:hover { background: rgba(255,255,255,.02); }

.row-date { font-size: 11px; color: var(--muted); min-width: 42px; flex-shrink: 0; }

.row-teams { display: flex; align-items: center; gap: 8px; flex: 1; min-width: 0; }
.row-team  { flex: 1; min-width: 0; }
.row-team.right { align-items: flex-end; display: flex; flex-direction: column; }
.rtn {
  font-size: 13px; font-weight: 600; white-space: nowrap;
  overflow: hidden; text-overflow: ellipsis; color: var(--text);
}
.row-team.fav    .rtn { color: ACCENT; }
.row-team.winner .rtn { color: #4ade80; }
.rrec { font-size: 10px; color: var(--muted); }

.row-score, .row-time {
  flex-shrink: 0; text-align: center; min-width: 82px; padding: 0 6px;
}
.sc      { font-size: 16px; font-weight: 700; letter-spacing: -.3px; display: block; }
.sc.live { color: #4ade80; }
.sc-lbl  { font-size: 9px; color: var(--muted); text-transform: uppercase; letter-spacing: .7px; }
.row-time { font-size: 12px; color: var(--muted); }
.live-badge-inline { font-size: 9px; font-weight: 800; color: #4ade80; display: block;
                     letter-spacing: .5px; animation: pulse 1.4s ease-in-out infinite; }
.status-line { font-size: 10px; color: var(--muted); display: block; margin-top: 1px; }

.row-meta { display: flex; align-items: center; gap: 7px; flex-shrink: 0; }
.row-bar  { width: 56px; height: 4px; border-radius: 2px;
            background: var(--border); overflow: hidden; display: flex; }
.rb-away  { background: #60a5fa; height: 100%; }
.rb-home  { background: ACCENT; height: 100%; flex: 1; }
.row-pick { font-size: 11px; font-weight: 600; color: var(--muted); white-space: nowrap; min-width: 84px; }
.result-area { display: flex; align-items: center; }
.expand-btn { font-size: 14px; color: var(--dim); transition: transform .2s; flex-shrink: 0; }
.row-main.open .expand-btn { transform: rotate(180deg); }

/* ── Row detail ── */
.row-detail { display: none; border-top: 1px solid var(--border); }
.row-detail.open { display: block; }
.detail-inner { padding: 14px 16px 16px; }

/* Pre-game stats table */
.detail-stats { width: 100%; border-collapse: collapse; max-width: 520px; margin: 0 auto; }
.detail-stats th {
  font-size: 10px; font-weight: 700; color: var(--muted);
  text-transform: uppercase; letter-spacing: .4px; padding: 0 0 8px;
}
.ds-away { width: 30%; text-align: left;  font-weight: 600; color: var(--muted); }
.ds-home { width: 30%; text-align: right; font-weight: 600; color: var(--muted); }
.ds-lbl  { width: 40%; text-align: center; font-size: 10px; font-weight: 600;
           color: var(--dim); text-transform: uppercase; letter-spacing: .35px; }
.detail-stats th.ds-away { text-align: left; }
.detail-stats th.ds-home { text-align: right; }
.detail-stats td { padding: 3px 0; font-size: 12px; }
.detail-stats td.better { color: ACCENT !important; }
.detail-stats td.worse  { color: var(--dim) !important; }

/* Live boxscore */
.boxscore { max-width: 600px; margin: 0 auto; }
.bs-section { margin-bottom: 14px; }
.bs-title { font-size: 10px; font-weight: 700; color: var(--muted);
            text-transform: uppercase; letter-spacing: .5px; margin-bottom: 6px; }
.linescore { width: 100%; border-collapse: collapse; font-size: 12px; margin-bottom: 12px; }
.linescore th { text-align: center; padding: 3px 6px; font-size: 10px; font-weight: 700;
                color: var(--muted); border-bottom: 1px solid var(--border); }
.linescore th.ls-team { text-align: left; }
.linescore td { text-align: center; padding: 4px 6px; }
.linescore td.ls-team { text-align: left; font-weight: 600; font-size: 12px; }
.linescore td.ls-total { font-weight: 700; color: var(--text); border-left: 1px solid var(--border); }
.linescore tr:last-child td { border-top: 1px solid var(--border); }
.linescore td.cur-period { background: rgba(255,255,255,.04); }

.stat-blocks { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; }
.stat-block { background: var(--bg3); border-radius: 6px; padding: 10px 12px; }
.stat-block-title { font-size: 10px; font-weight: 700; color: var(--muted);
                    text-transform: uppercase; letter-spacing: .4px; margin-bottom: 6px; }
.player-stat { display: flex; justify-content: space-between; font-size: 11px;
               padding: 2px 0; border-bottom: 1px solid var(--border); }
.player-stat:last-child { border-bottom: none; }
.ps-name { color: var(--text); font-weight: 500; }
.ps-val  { color: ACCENT; font-weight: 700; font-size: 10px; font-variant-numeric: tabular-nums; }
.bs-loading { text-align: center; padding: 20px; color: var(--muted); font-size: 12px; }
.bs-divider { border: none; border-top: 1px solid var(--border); margin: 10px 0; }

/* ── Pills ── */
.pill {
  font-size: 10px; font-weight: 700; letter-spacing: .4px;
  padding: 2px 8px; border-radius: 20px; text-transform: uppercase; white-space: nowrap;
}
.conf-high    { background: rgba(34,197,94,.13);  color: #4ade80; border: 1px solid rgba(34,197,94,.22); }
.conf-med     { background: rgba(251,191,36,.1);  color: #fbbf24; border: 1px solid rgba(251,191,36,.2); }
.conf-low     { background: rgba(93,115,144,.1);  color: var(--muted); border: 1px solid var(--border); }
.pill-correct { background: rgba(34,197,94,.13);  color: #4ade80; border: 1px solid rgba(34,197,94,.22); }
.pill-wrong   { background: rgba(239,68,68,.13);  color: #f87171; border: 1px solid rgba(239,68,68,.22); }
.pill-live    { background: rgba(74,222,128,.15); color: #4ade80; border: 1px solid rgba(74,222,128,.3);
                animation: pulse 1.8s ease-in-out infinite; }
@keyframes pulse { 0%,100%{opacity:1;} 50%{opacity:.4;} }

/* ── Accuracy tab ── */
.acc-grid {
  display: grid; grid-template-columns: repeat(auto-fill, minmax(130px,1fr));
  gap: 10px; max-width: 900px; margin: 20px auto 0; padding: 0 24px;
}
.acc-card { background: var(--bg2); border: 1px solid var(--border);
            border-radius: 10px; padding: 14px 12px; text-align: center; }
.acc-card.acc-hi .acc-big { color: #4ade80; }
.acc-card.acc-md .acc-big { color: #fbbf24; }
.acc-card.acc-lo .acc-big { color: var(--muted); }
.acc-big { font-size: 22px; font-weight: 700; }
.acc-lbl { font-size: 10px; color: var(--muted); margin-top: 4px; }

.acc-section-tabs {
  display: flex; gap: 2px; padding: 14px 24px 0; border-bottom: 1px solid var(--border);
  background: var(--bg);
}
.acc-stab {
  padding: 8px 18px; font-size: 13px; font-weight: 500; color: var(--muted);
  border: 1px solid var(--border); border-bottom: none; border-radius: 6px 6px 0 0;
  cursor: pointer; background: var(--bg2); transition: all .15s;
}
.acc-stab.active { color: var(--text); background: var(--bg3); border-color: ACCENT; }

.acc-table { width: 100%; border-collapse: collapse; font-size: 13px; }
.acc-table th {
  text-align: left; padding: 9px 14px; color: var(--muted);
  border-bottom: 1px solid var(--border);
  font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: .4px;
  cursor: pointer; white-space: nowrap;
}
.acc-table th:hover { color: var(--text); }
.acc-table td { padding: 9px 14px; border-bottom: 1px solid var(--border); font-size: 12px; }
.acc-table tr:last-child td { border-bottom: none; }
.acc-table tr:hover td { background: var(--bg3); }
.acc-ok   { color: #4ade80; font-weight: 700; }
.acc-warn { color: #fbbf24; font-weight: 700; }
.acc-no   { color: #f87171; font-weight: 700; }
.prob-txt { color: var(--muted); font-size: 11px; }
.vs-sep   { color: var(--dim); font-size: 10px; }
.matchup-cell { font-size: 12px; }
.team-name-cell { font-weight: 600; }

/* ── Empty ── */
.empty { text-align: center; padding: 80px 20px; color: var(--muted); font-size: 15px; }
.e-icon { font-size: 36px; display: block; margin-bottom: 10px; opacity: .3; }

@media (max-width: 700px) {
  .row-bar, .row-date { display: none; }
  .row-pick { min-width: 70px; font-size: 10px; }
  .game-list { padding: 8px 12px 24px; }
  .filter-bar { padding: 8px 12px; }
  .stat-blocks { grid-template-columns: 1fr; }
}
'''

# ══════════════════════════════════════════════════════════════════════════════
#  JAVASCRIPT
# ══════════════════════════════════════════════════════════════════════════════
JS = '''\
<script>
// ── Tab switching ─────────────────────────────────────────────────────────
function sw(el, name) {
  document.querySelectorAll('.sub-tab').forEach(t => t.classList.remove('active'));
  document.querySelectorAll('.sub-panel').forEach(p => p.classList.remove('active'));
  el.classList.add('active');
  document.getElementById('tab-' + name).classList.add('active');
  if (name === 'today') startLive(); else stopLive();
}

// ── Accuracy view toggle ──────────────────────────────────────────────────
function switchAccView(btn, view) {
  document.querySelectorAll('.acc-stab').forEach(b => b.classList.remove('active'));
  btn.classList.add('active');
  document.getElementById('acc-view-games').style.display  = view === 'games'  ? '' : 'none';
  document.getElementById('acc-view-teams').style.display  = view === 'teams'  ? '' : 'none';
}

// ── Row expand / collapse + boxscore fetch ────────────────────────────────
async function toggleRow(detailId, mainEl, gameId, status) {
  const detail = document.getElementById(detailId);
  if (!detail) return;
  const opening = !detail.classList.contains('open');
  detail.classList.toggle('open', opening);
  mainEl.classList.toggle('open', opening);

  if (opening && (status === 'live' || status === 'final') && gameId) {
    const row      = mainEl.closest('.game-row');
    const espnPath = row ? row.dataset.espnPath : '';
    if (espnPath) fetchBoxscore(gameId, detail, espnPath);
  }
}

async function fetchBoxscore(gameId, detailEl, espnPath) {
  const inner = detailEl.querySelector('.detail-inner');
  if (!inner) return;
  inner.innerHTML = '<div class="bs-loading">Loading game stats…</div>';
  try {
    const url  = `https://site.api.espn.com/apis/site/v2/sports/${espnPath}/summary?event=${gameId}`;
    const resp = await fetch(url);
    if (!resp.ok) throw new Error(resp.status);
    const data = await resp.json();
    inner.innerHTML = buildBoxscore(data, espnPath);
  } catch(e) {
    inner.innerHTML = '<div class="bs-loading">Stats unavailable.</div>';
  }
}

function buildBoxscore(data, espnPath) {
  const sport  = espnPath.split('/')[0]; // baseball, hockey, basketball, football
  const comp   = ((data.header||{}).competitions||[])[0] || {};
  const teams  = (comp.competitors||[]);
  const away   = teams.find(t=>t.homeAway==='away') || teams[0] || {};
  const home   = teams.find(t=>t.homeAway==='home') || teams[1] || {};
  const awayName = (away.team||{}).shortDisplayName || (away.team||{}).displayName || 'Away';
  const homeName = (home.team||{}).shortDisplayName || (home.team||{}).displayName || 'Home';

  let html = '<div class="boxscore">';

  // ── Linescore ────────────────────────────────────────────────────────────
  const ls = data.linescore || comp.linescore || [];
  if (ls.length) {
    const periodLabel = sport === 'baseball' ? (i => i+1) :
                        sport === 'hockey'   ? (i => ['P1','P2','P3','OT','SO'][i] || `OT${i-3}`) :
                        sport === 'basketball' ? (i => `Q${i+1}`) :
                        (i => `Q${i+1}`);

    const awayLS = (ls.find ? ls : []).map ? ls : [];
    const periods = (awayLS[0]||{}).columns || awayLS;
    // Try to parse ESPN's linescore structure
    let awayScores = [], homeScores = [];
    // ESPN linescore can come as array of objects per period
    if (Array.isArray(ls) && ls[0] && ls[0].displayValue !== undefined) {
      // Format: [{displayValue, period, ...}, ...]
      const awayPeriods = ls.filter((_,i)=>i%2===0);
      const homePeriods = ls.filter((_,i)=>i%2===1);
      awayScores = awayPeriods.map(p=>p.displayValue||'—');
      homeScores = homePeriods.map(p=>p.displayValue||'—');
    } else if (Array.isArray(ls) && ls[0] && ls[0].values) {
      awayScores = ls[0].values.map(v=>v.displayValue||v.value||'—');
      homeScores = (ls[1]||{values:[]}).values.map(v=>v.displayValue||v.value||'—');
    }

    // Fallback: check boxscore.teams for period scores
    const bsTeams = (data.boxscore||{}).teams||[];
    if (!awayScores.length && bsTeams.length) {
      const bsAway = bsTeams.find(t=>(t.homeAway||'away')==='away')||bsTeams[0]||{};
      const bsHome = bsTeams.find(t=>(t.homeAway||'home')==='home')||bsTeams[1]||{};
      awayScores = (bsAway.statistics||[]).slice(0,10).map(s=>s.displayValue||'—');
      homeScores = (bsHome.statistics||[]).slice(0,10).map(s=>s.displayValue||'—');
    }

    if (awayScores.length || homeScores.length) {
      const n = Math.max(awayScores.length, homeScores.length);
      let headers = '<th class="ls-team">Team</th>';
      for (let i=0;i<n;i++) headers += `<th>${periodLabel(i)}</th>`;
      headers += '<th class="ls-total">T</th>';

      const awayTotal = away.score||'';
      const homeTotal = home.score||'';
      let ar='', hr='';
      for (let i=0;i<n;i++) { ar+=`<td>${awayScores[i]||'—'}</td>`; hr+=`<td>${homeScores[i]||'—'}</td>`; }

      html += `<div class="bs-section">
        <div class="bs-title">Scoring</div>
        <table class="linescore">
          <thead><tr>${headers}</tr></thead>
          <tbody>
            <tr><td class="ls-team">${awayName}</td>${ar}<td class="ls-total">${awayTotal}</td></tr>
            <tr><td class="ls-team">${homeName}</td>${hr}<td class="ls-total">${homeTotal}</td></tr>
          </tbody>
        </table>
      </div>`;
    }
  }

  // ── Player stats ─────────────────────────────────────────────────────────
  const bsTeams = (data.boxscore||{}).teams || [];
  const bsAway  = bsTeams.find(t=>(t.homeAway||'away')==='away') || bsTeams[0] || {};
  const bsHome  = bsTeams.find(t=>(t.homeAway||'home')==='home') || bsTeams[1] || {};

  function topPlayers(bsTeam, statKeys, n=4) {
    const players = (bsTeam.athletes||bsTeam.players||[]);
    const out = [];
    for (const p of players.slice(0,30)) {
      const athlete = p.athlete||p;
      const name    = athlete.shortName||athlete.displayName||'';
      const stats   = p.stats||[];
      const vals    = statKeys.map(k=>{
        if (typeof k === 'number') return stats[k]||'';
        const found = stats.find(s=>s.name===k||s.abbreviation===k);
        return found ? found.displayValue||found.value||'' : (stats[k]||'');
      }).filter(v=>v!=='').join(' | ');
      if (name && vals) out.push({name, vals});
      if (out.length >= n) break;
    }
    return out;
  }

  let awayPlayers = [], homePlayers = [];
  let statLabel = 'Key Stats';

  if (sport === 'baseball') {
    statLabel = 'Pitching / Hitting';
    // For MLB, show pitchers first
    const awayPitch = (bsAway.athletes||[]).filter(p=>(p.position||{}).abbreviation==='P');
    const homePitch = (bsHome.athletes||[]).filter(p=>(p.position||{}).abbreviation==='P');
    const fmtPitch  = (arr) => arr.slice(0,2).map(p=>{
      const s = p.stats||[]; const nm = (p.athlete||p).shortName||'?';
      return {name: nm + ' (P)', vals: s.slice(0,6).map(x=>x.displayValue||x.value||'').filter(Boolean).join(' ')};
    });
    awayPlayers = fmtPitch(awayPitch);
    homePlayers = fmtPitch(homePitch);
    // Add hitters
    const awayHit = (bsAway.athletes||[]).filter(p=>(p.position||{}).abbreviation!=='P');
    const homeHit = (bsHome.athletes||[]).filter(p=>(p.position||{}).abbreviation!=='P');
    const fmtHit  = (arr) => arr.slice(0,3).map(p=>{
      const s = p.stats||[]; const nm = (p.athlete||p).shortName||'?';
      const h = s[0]?.displayValue||''; const ab = s[1]?.displayValue||'';
      const hr= s[5]?.displayValue||''; const rbi= s[6]?.displayValue||'';
      const parts = [];
      if (h&&ab) parts.push(`${h}/${ab}`);
      if (hr&&hr!=='0') parts.push(`${hr} HR`);
      if (rbi&&rbi!=='0') parts.push(`${rbi} RBI`);
      return {name: nm, vals: parts.join(' · ')||'—'};
    });
    awayPlayers = [...awayPlayers, ...fmtHit(awayHit)];
    homePlayers = [...homePlayers, ...fmtHit(homeHit)];

  } else if (sport === 'basketball') {
    statLabel = 'Top Performers';
    const fmt = (bsTeam) => {
      const ps = (bsTeam.athletes||bsTeam.players||[]);
      return ps.slice(0,8).map(p=>{
        const s = p.stats||[]; const nm = (p.athlete||p).shortName||'';
        // NBA stats order: min,fg,3pt,ft,oreb,dreb,reb,ast,stl,blk,to,pf,+/-,pts
        const pts = s[13]?.displayValue||s.find(x=>x.name==='points')?.displayValue||'';
        const reb = s[6]?.displayValue||s.find(x=>x.name==='rebounds')?.displayValue||'';
        const ast = s[7]?.displayValue||s.find(x=>x.name==='assists')?.displayValue||'';
        const parts = []; if(pts) parts.push(`${pts}pts`); if(reb) parts.push(`${reb}reb`); if(ast) parts.push(`${ast}ast`);
        return {name: nm, vals: parts.join(' ')};
      }).filter(p=>p.vals).slice(0,5);
    };
    awayPlayers = fmt(bsAway);
    homePlayers = fmt(bsHome);

  } else if (sport === 'hockey') {
    statLabel = 'Scoring & Saves';
    // Show goal scorers from keyEvents
    const keyEvents = data.keyEvents||data.plays||[];
    const goals = keyEvents.filter(e=>(e.type||{}).text==='Goal'||(e.scoringPlay));
    const goalMap = {};
    goals.forEach(g=>{
      const nm = ((g.participants||[])[0]||{}).displayName||'';
      if (!nm) return;
      goalMap[nm] = (goalMap[nm]||0)+1;
    });
    const goalList = Object.entries(goalMap).map(([n,g])=>({name:n,vals:`${g} G`}));
    awayPlayers = goalList.slice(0,4);
    homePlayers = [];
    // Show goalies
    const awayG = (bsAway.athletes||[]).filter(p=>(p.position||{}).abbreviation==='G');
    const homeG = (bsHome.athletes||[]).filter(p=>(p.position||{}).abbreviation==='G');
    if (awayG[0]) {
      const s=awayG[0].stats||[]; const nm=(awayG[0].athlete||awayG[0]).shortName||'';
      awayPlayers.push({name:nm+' (G)', vals:s.slice(0,4).map(x=>x.displayValue||'').filter(Boolean).join(' ')});
    }
    if (homeG[0]) {
      const s=homeG[0].stats||[]; const nm=(homeG[0].athlete||homeG[0]).shortName||'';
      homePlayers.push({name:nm+' (G)', vals:s.slice(0,4).map(x=>x.displayValue||'').filter(Boolean).join(' ')});
    }

  } else if (sport === 'football') {
    statLabel = 'Key Stats';
    const fmtQB = (bsTeam) => {
      const qbs = (bsTeam.athletes||[]).filter(p=>(p.position||{}).abbreviation==='QB');
      return qbs.slice(0,1).map(p=>{
        const s=p.stats||[]; const nm=(p.athlete||p).shortName||'';
        // Passing: comp/att, yds, td, int
        const pass = s[0]?.displayValue||''; const yds=s[2]?.displayValue||''; const td=s[3]?.displayValue||''; const int_=s[4]?.displayValue||'';
        const vals = [pass&&`${pass}`,yds&&`${yds} yds`,td&&`${td} TD`,int_&&`${int_} INT`].filter(Boolean).join(' · ');
        return {name:nm+' (QB)', vals};
      });
    };
    awayPlayers = fmtQB(bsAway);
    homePlayers = fmtQB(bsHome);
    // Add top rusher/receiver
    const fmtSkill = (bsTeam, pos, statIdx, suffix) => {
      const players = (bsTeam.athletes||[]).filter(p=>(p.position||{}).abbreviation===pos);
      return players.slice(0,2).map(p=>{
        const s=p.stats||[]; const nm=(p.athlete||p).shortName||'';
        const val=s[statIdx]?.displayValue||'';
        return {name:nm, vals:val?`${val} ${suffix}`:'—'};
      }).filter(p=>p.vals!=='—');
    };
    awayPlayers = [...awayPlayers, ...fmtSkill(bsAway,'RB',0,'rush yds'), ...fmtSkill(bsAway,'WR',0,'rec yds')];
    homePlayers = [...homePlayers, ...fmtSkill(bsHome,'RB',0,'rush yds'), ...fmtSkill(bsHome,'WR',0,'rec yds')];
  }

  if (awayPlayers.length || homePlayers.length) {
    const buildBlock = (title, players) => {
      if (!players.length) return '';
      const rows = players.map(p=>
        `<div class="player-stat"><span class="ps-name">${p.name}</span><span class="ps-val">${p.vals}</span></div>`
      ).join('');
      return `<div class="stat-block"><div class="stat-block-title">${title}</div>${rows}</div>`;
    };
    html += `<div class="bs-section">
      <div class="bs-title">${statLabel}</div>
      <div class="stat-blocks">
        ${buildBlock(awayName, awayPlayers)}
        ${buildBlock(homeName, homePlayers)}
      </div>
    </div>`;
  }

  html += '</div>';
  return html || '<div class="bs-loading">No stats available yet.</div>';
}

// ── Filters ───────────────────────────────────────────────────────────────
const activeFilters = {};

function setFilter(btn) {
  const tab    = btn.dataset.tab;
  const filter = btn.dataset.filter;
  const value  = btn.dataset.value;
  // deactivate siblings in same .fg group
  btn.closest('.fg').querySelectorAll('.fb').forEach(b => b.classList.remove('active'));
  btn.classList.add('active');
  if (!activeFilters[tab]) activeFilters[tab] = {};
  activeFilters[tab][filter] = value;
  applyFilters(tab);
}

function setTeamFilter(sel, tab) {
  if (!activeFilters[tab]) activeFilters[tab] = {};
  activeFilters[tab]['team'] = sel.value;
  applyFilters(tab);
}

function applyFilters(tab) {
  const filters = activeFilters[tab] || {};
  const today   = new Date().toISOString().slice(0, 10);

  if (tab === 'accuracy') {
    const tbody = document.getElementById('acc-tbody');
    if (!tbody) return;
    let visible = 0;
    tbody.querySelectorAll('.acc-row').forEach(row => {
      let show = true;
      for (const [key, val] of Object.entries(filters)) {
        if (val === 'all' || val === '') continue;
        if (key === 'team') {
          const away = row.dataset.away || '';
          const home = row.dataset.home || '';
          if (away !== val && home !== val) { show = false; break; }
        } else {
          if ((row.dataset[key] || '') !== val) { show = false; break; }
        }
      }
      row.style.display = show ? '' : 'none';
      if (show) visible++;
    });
    const cnt = document.getElementById('count-accuracy');
    if (cnt) cnt.textContent = visible + ' game' + (visible !== 1 ? 's' : '');
    return;
  }

  const list = document.getElementById('list-' + tab);
  if (!list) return;
  let visible = 0;
  list.querySelectorAll('.game-row').forEach(row => {
    let show = true;
    for (const [key, val] of Object.entries(filters)) {
      if (val === 'all' || val === '') continue;
      if (key === 'days') {
        const gd   = (row.dataset.date || '').slice(0, 10);
        const diff = (new Date(gd) - new Date(today)) / 86400000;
        // recent: diff negative (past), upcoming: diff positive
        if (Math.abs(diff) > parseInt(val)) { show = false; break; }
      } else {
        if ((row.dataset[key] || '') !== val) { show = false; break; }
      }
    }
    row.style.display = show ? '' : 'none';
    if (show) visible++;
  });
  const cnt = document.getElementById('count-' + tab);
  if (cnt) cnt.textContent = visible + ' game' + (visible !== 1 ? 's' : '');
}

// ── Team breakdown sort + search ──────────────────────────────────────────
function sortTeams(col, btn) {
  btn.closest('.fg').querySelectorAll('.fb').forEach(b => b.classList.remove('active'));
  btn.classList.add('active');
  const tbody = document.getElementById('team-tbody');
  if (!tbody) return;
  const rows  = Array.from(tbody.querySelectorAll('.team-row'));
  rows.sort((a, b) => {
    const cells = {total:1, acc:2, fav:4, dog:5};
    const idx   = cells[col] || 1;
    const av    = parseFloat(a.cells[idx]?.textContent) || 0;
    const bv    = parseFloat(b.cells[idx]?.textContent) || 0;
    return bv - av;
  });
  rows.forEach(r => tbody.appendChild(r));
}

function filterTeamSearch(inp) {
  const q = inp.value.toLowerCase();
  const tbody = document.getElementById('team-tbody');
  if (!tbody) return;
  let ct = 0;
  tbody.querySelectorAll('.team-row').forEach(r => {
    const name = (r.dataset.team || '').toLowerCase();
    const show = !q || name.includes(q);
    r.style.display = show ? '' : 'none';
    if (show) ct++;
  });
  const cnt = document.getElementById('count-teams');
  if (cnt) cnt.textContent = ct + ' teams';
}

// ── Live polling ──────────────────────────────────────────────────────────
const SPORT     = document.body.dataset.sport;
const ESPN_PATH = document.body.dataset.espnPath;
const FAST      = 30000;
const SLOW      = 300000;
let liveTimer   = null;

function statusText(comp) {
  const st   = comp.status || {};
  const type = (st.type || {}).name || '';
  const situ = comp.situation || {};
  if (type === 'STATUS_IN_PROGRESS' || type === 'STATUS_HALFTIME') {
    if (SPORT === 'mlb') {
      const inn  = st.period || '';
      const top  = situ.isTopInning !== false;
      const outs = situ.outs != null ? ` · ${situ.outs} out` : '';
      const base = [situ.onFirst?'1B':'', situ.onSecond?'2B':'', situ.onThird?'3B':''].filter(Boolean).join(' ');
      return `${top?'▲':'▼'} ${inn}${outs}${base ? ' · '+base : ''}`;
    } else if (SPORT === 'nhl') {
      const p=st.period||''; const c=st.displayClock||'';
      return `${p===4?'OT':p===5?'SO':'P'+p} ${c}`;
    } else if (SPORT === 'nba') {
      const p=st.period||''; const c=st.displayClock||'';
      return `${p>4?'OT'+(p-4):'Q'+p} ${c}`;
    } else if (SPORT === 'nfl') {
      const p=st.period||''; const c=st.displayClock||'';
      const dn=(comp.situation||{}).downDistanceText||'';
      return `Q${p>4?'OT':p} ${c}`+(dn?`<br><small>${dn}</small>`:'');
    }
  }
  return (st.type||{}).shortDetail || '';
}

async function pollLive() {
  try {
    const today = new Date().toISOString().slice(0,10).replace(/-/g,'');
    const r     = await fetch(`https://site.api.espn.com/apis/site/v2/sports/${ESPN_PATH}/scoreboard?dates=${today}`);
    if (!r.ok) return;
    const data  = await r.json();
    let liveCt  = 0;

    for (const ev of (data.events||[])) {
      const row = document.querySelector(`[data-game-id="${ev.id}"]`);
      const comp  = (ev.competitions||[])[0]; if (!comp) continue;
      const comps = comp.competitors||[];
      const away  = comps.find(c=>c.homeAway==='away')||{};
      const home  = comps.find(c=>c.homeAway==='home')||{};
      const as_   = away.score??''; const hs_ = home.score??'';
      const tname = (comp.status?.type?.name)||'';
      const isLive  = tname==='STATUS_IN_PROGRESS'||tname==='STATUS_HALFTIME';
      const isFinal = tname==='STATUS_FINAL'||comp.status?.type?.completed;

      // Fix scheduled game time using ET conversion
      if (!isLive && !isFinal && ev.date && row) {
        const ctr = document.getElementById('ctr-' + ev.id);
        if (ctr && ctr.classList.contains('row-time')) {
          const d = new Date(ev.date);
          const et = d.toLocaleTimeString('en-US', {
            timeZone:'America/New_York', hour:'numeric', minute:'2-digit'
          });
          ctr.textContent = et + ' ET';
        }
      }

      if (!row) continue;
      const ctr = document.getElementById('ctr-' + ev.id);
      if (!ctr) continue;
      const prev = ctr.innerHTML;

      if (isLive) {
        liveCt++;
        const st = statusText(comp);
        const nh = `<span class="sc live">${as_}–${hs_}</span>` +
                   `<span class="live-badge-inline">● LIVE</span>` +
                   (st?`<span class="status-line">${st}</span>`:'');
        if (nh !== prev) {
          ctr.innerHTML = nh;
          row.dataset.status = 'live';
          row.classList.add('is-live','just-updated');
          setTimeout(()=>row.classList.remove('just-updated'),600);
          // Refresh boxscore if detail is open
          const detail = row.querySelector('.row-detail.open');
          if (detail) fetchBoxscore(ev.id, detail, ESPN_PATH);
        }
        const meta = row.querySelector('.row-meta');
        if (meta && !meta.querySelector('.pill-live')) {
          const lp = document.createElement('span');
          lp.className='pill pill-live'; lp.textContent='LIVE';
          meta.insertBefore(lp, meta.querySelector('.expand-btn'));
        }
      } else if (isFinal && as_!==''&&hs_!=='') {
        const nh = `<span class="sc">${as_}–${hs_}</span><span class="sc-lbl">FINAL</span>`;
        if (nh !== prev) {
          ctr.innerHTML = nh;
          row.dataset.status = 'final';
          row.classList.remove('is-live');
          const pred=row.dataset.pred;
          const actual=parseInt(as_)>parseInt(hs_)?'away':'home';
          const ok=actual===pred;
          row.dataset.result=ok?'correct':'wrong';
          const ra=row.querySelector('.result-area');
          if (ra) ra.innerHTML=`<span class="pill ${ok?'pill-correct':'pill-wrong'}">${ok?'✓':'✗'}</span>`;
          const lp=row.querySelector('.pill-live'); if(lp) lp.remove();
        }
      }
    }

    const ind = document.getElementById('live-indicator');
    const t   = new Date().toLocaleTimeString([],{hour:'2-digit',minute:'2-digit'});
    if (ind) {
      if (liveCt>0) { ind.textContent=`● ${liveCt} live · ${t}`; ind.className='live-status is-live'; reschedule(FAST); }
      else          { ind.textContent=`Updated ${t}`;             ind.className='live-status';         reschedule(SLOW); }
    }
  } catch(e) { console.warn('Poll error',e); }
}

function reschedule(ms) { clearTimeout(liveTimer); liveTimer=setTimeout(pollLive,ms); }
function startLive()    { const tp=document.getElementById('tab-today'); if(tp&&tp.querySelector('[data-game-id]')) pollLive(); }
function stopLive()     { clearTimeout(liveTimer); }
document.addEventListener('DOMContentLoaded', startLive);
</script>
'''

# ══════════════════════════════════════════════════════════════════════════════
#  HTML BUILDER
# ══════════════════════════════════════════════════════════════════════════════
def build_html(league_key, cfg, today_games, recent_games, upcoming_games, all_finals):
    accent = cfg['accent']
    css    = CSS.replace('ACCENT', accent)
    today_s = str(date.today())
    emoji_map = {'mlb':'⚾','nhl':'🏒','nba':'🏀','nfl':'🏈'}
    le = emoji_map.get(league_key, '🏟')
    counts = (f"Today: {len(today_games)}  ·  "
              f"Recent: {len(recent_games)}  ·  "
              f"Upcoming: {len(upcoming_games)}")

    def section(games, tab_id):
        if not games:
            return (f'<div class="empty"><span class="e-icon">{le}</span>'
                    f'No {cfg["name"]} games to show.</div>')
        bar  = render_filter_bar(tab_id, len(games))
        rows = ''.join(render_row(g, cfg, tab_id, i) for i, g in enumerate(games))
        return f'{bar}<div class="game-list" id="list-{tab_id}">{rows}</div>'

    return f'''<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>{cfg["name"]} Predictions — {cfg["season_label"]}</title>
<style>{css}</style>
</head>
<body data-sport="{league_key}" data-espn-path="{cfg['espn_path']}">

<div class="page-header">
  <div class="hdr-emoji">{cfg["emoji"]}</div>
  <div class="hdr-info">
    <div class="hdr-name">{cfg["name"]} Predictions</div>
    <div class="hdr-sub">{cfg["season_label"]}  ·  {counts}</div>
  </div>
  <div class="hdr-right">
    <div class="hdr-date">Updated {today_s}</div>
    <div class="live-status" id="live-indicator">Checking for live games…</div>
  </div>
</div>

<div class="sub-nav">
  <div class="sub-tab active" onclick="sw(this,'today')">Today</div>
  <div class="sub-tab" onclick="sw(this,'recent')">Recent Results</div>
  <div class="sub-tab" onclick="sw(this,'upcoming')">Upcoming</div>
  <div class="sub-tab" onclick="sw(this,'accuracy')">Accuracy</div>
</div>

<div id="tab-today"    class="sub-panel active">{section(today_games,   'today')}</div>
<div id="tab-recent"   class="sub-panel">{section(recent_games,  'recent')}</div>
<div id="tab-upcoming" class="sub-panel">{section(upcoming_games,'upcoming')}</div>
<div id="tab-accuracy" class="sub-panel">
  <div style="padding-bottom:32px">{render_accuracy(all_finals, cfg)}</div>
</div>

{JS}
</body>
</html>'''

# ══════════════════════════════════════════════════════════════════════════════
#  MAIN
# ══════════════════════════════════════════════════════════════════════════════
def generate(league_key):
    if league_key not in LEAGUES:
        print(f'Unknown league: {league_key}. Choose from: {list(LEAGUES)}')
        sys.exit(1)
    cfg      = LEAGUES[league_key]
    csv_path = os.path.join(cfg['script_dir'], cfg['csv_file'])
    if not os.path.exists(csv_path):
        print(f'  [{cfg["name"]}] CSV not found: {csv_path} — skipping')
        return None
    with open(csv_path, newline='', encoding='utf-8') as f:
        rows = list(csv.DictReader(f))
    today_s      = str(date.today())
    recent_cut   = str(date.today() - timedelta(days=10))
    upcoming_cut = str(date.today() + timedelta(days=14))
    today_games    = [r for r in rows if r.get('game_date','')[:10] == today_s]
    recent_games   = [r for r in rows
                      if recent_cut <= r.get('game_date','')[:10] < today_s
                      and r.get('status','') == 'Final']
    upcoming_games = [r for r in rows
                      if today_s < r.get('game_date','')[:10] <= upcoming_cut
                      and r.get('status','') != 'Final']
    all_finals     = [r for r in rows
                      if r.get('status','') == 'Final'
                      and r.get('winner','').strip()
                      and r.get('favored_team','').strip()]
    recent_games.sort(  key=lambda r: r.get('game_date',''), reverse=True)
    upcoming_games.sort(key=lambda r: r.get('game_date',''))
    html     = build_html(league_key, cfg, today_games, recent_games, upcoming_games, all_finals)
    out_path = os.path.join(cfg['script_dir'], cfg['output_file'])
    os.makedirs(cfg['script_dir'], exist_ok=True)
    with open(out_path, 'w', encoding='utf-8') as f:
        f.write(html)
    print(f'  {cfg["name"]} → {out_path}')
    print(f'    Today: {len(today_games)} | Recent: {len(recent_games)} | '
          f'Upcoming: {len(upcoming_games)} | Finals: {len(all_finals)}')
    return out_path

if __name__ == '__main__':
    leagues = sys.argv[1:] if len(sys.argv) > 1 else list(LEAGUES)
    for lg in leagues:
        generate(lg)
