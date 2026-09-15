"""ESPN data access for player-level information.

This module is deliberately paranoid. ESPN's public JSON is undocumented and
changes shape between endpoints and sports, so every reader accepts several
layouts, every field is optional, and any failure degrades to "no props for
this game" rather than breaking the build.
"""
from __future__ import annotations

import re

from .config import STAT_ALIASES
from .util import Http, dig, num

SITE_API = 'https://site.api.espn.com/apis/site/v2/sports'
WEB_API = 'https://site.web.api.espn.com/apis'

# How many athletes to pull per sport, ordered by the sort key below. Season
# leaders are who props are actually offered on.
BULK_LIMIT = 400
BULK_PAGES = 3

SORT_KEYS = {
    'basketball': 'offensive.avgPoints:desc',
    'baseball': None,
    'football': None,
    'hockey': None,
}


def norm(s):
    return re.sub(r'[^a-z0-9]', '', (s or '').lower())


def norm_team(name):
    """Collapse the many spellings ESPN uses for one franchise."""
    n = norm(name)
    aliases = {
        'athletics': 'athletics', 'oaklandathletics': 'athletics',
        'lasvegasathletics': 'athletics', 'sacramentoathletics': 'athletics',
        'laangels': 'losangelesangels', 'lakers': 'losangeleslakers',
        'clippers': 'laclippers', 'losangelesclippers': 'laclippers',
    }
    return aliases.get(n, n)


# ─────────────────────────────────────────────────────────────────────────────
#  Stat extraction
# ─────────────────────────────────────────────────────────────────────────────
def _alias_map(sport):
    """ESPN stat name (normalised) -> our internal key."""
    out = {}
    for internal, candidates in STAT_ALIASES.get(sport, {}).items():
        for cand in candidates:
            out.setdefault(norm(cand), internal)
    return out


AVG_MARKERS = ('avg', 'pergame', 'permatch', 'average')


def is_average_name(name):
    """True when ESPN's own field name says the number is already per game.

    ESPN mixes ``homeRuns`` (a season total) and ``avgPoints`` (a rate) in the
    same payload, so the field name is the most reliable signal we get about
    which one we are holding.
    """
    n = norm(name)
    return any(m in n for m in AVG_MARKERS)


def extract_stats(sport, names, values):
    """Zip parallel name/value arrays into our internal stat keys.

    ``__avg__`` records which keys arrived already expressed per game, so the
    caller does not have to guess from the magnitude alone.
    """
    amap = _alias_map(sport)
    stats = {}
    avg_keys = []
    for name, value in zip(names or [], values or []):
        key = amap.get(norm(name))
        if key is None or key in stats:
            continue
        v = num(value)
        if v is None and isinstance(value, str) and ':' in value:
            v = _clock_to_minutes(value)
        if v is None:
            continue
        stats[key] = v
        if is_average_name(name):
            avg_keys.append(key)
    if avg_keys:
        stats['__avg__'] = avg_keys
    return stats


def _clock_to_minutes(value):
    """'18:42' -> 18.7 (used by NHL time-on-ice and NBA minutes)."""
    try:
        parts = [float(p) for p in str(value).split(':')]
    except ValueError:
        return None
    if len(parts) == 2:
        return parts[0] + parts[1] / 60.0
    if len(parts) == 3:
        return parts[0] * 60 + parts[1] + parts[2] / 60.0
    return None


# ─────────────────────────────────────────────────────────────────────────────
#  Bulk athlete statistics
# ─────────────────────────────────────────────────────────────────────────────
def fetch_athlete_stats(http, sport, league, season=None, pages=BULK_PAGES):
    """Season statistics for the league's most-used players.

    Returns ``{normalised_team_name: [player, ...]}``. Empty on any failure.
    """
    players = []
    for page in range(1, pages + 1):
        url = (f'{WEB_API}/common/v3/sports/{sport}/{league}/statistics/byathlete'
               f'?region=us&lang=en&contentorigin=espn&isqualified=false'
               f'&limit={BULK_LIMIT}&page={page}')
        if season:
            url += f'&season={season}'
        sort = SORT_KEYS.get(sport)
        if sort:
            url += f'&sort={sort}'
        data = http.get_json(url)
        if not data:
            break
        batch = _parse_byathlete(data, sport)
        players.extend(batch)
        total_pages = num(dig(data, 'pagination', 'pages'), 1) or 1
        if page >= total_pages or not batch:
            break

    pool = {}
    for p in players:
        if not p.get('team'):
            continue
        pool.setdefault(norm_team(p['team']), []).append(p)
    return pool


def _parse_byathlete(data, sport):
    """Handle both layouts: category names at the top level, or per athlete."""
    top_names = {}
    for cat in data.get('categories') or []:
        cname = cat.get('name') or ''
        names = cat.get('names') or cat.get('labels') or []
        if names:
            top_names[cname] = names

    out = []
    for entry in data.get('athletes') or []:
        ath = entry.get('athlete') or {}
        stats = {}
        for cat in entry.get('categories') or []:
            cname = cat.get('name') or ''
            names = cat.get('names') or cat.get('labels') or top_names.get(cname) or []
            values = cat.get('totals') or cat.get('values') or cat.get('stats') or []
            if isinstance(values, list) and values and isinstance(values[0], dict):
                names = [v.get('name') or v.get('abbreviation') for v in values]
                values = [v.get('value', v.get('displayValue')) for v in values]
            found = extract_stats(sport, names, values)
            merged_avg = list(stats.get('__avg__', [])) + list(found.pop('__avg__', []))
            stats.update(found)
            if merged_avg:
                stats['__avg__'] = merged_avg
        if not {k for k in stats if k != '__avg__'}:
            continue
        player = _athlete_meta(ath)
        player['stats'] = stats
        if player['name']:
            out.append(player)
    return out


def _athlete_meta(ath):
    team = (ath.get('teamName') or dig(ath, 'team', 'displayName')
            or dig(ath, 'team', 'name') or '')
    short_team = (ath.get('teamShortName') or dig(ath, 'team', 'abbreviation') or '')
    if team and short_team and norm(team) == norm(short_team):
        team = dig(ath, 'team', 'displayName') or team
    return {
        'id': str(ath.get('id') or ''),
        'name': ath.get('displayName') or ath.get('fullName') or '',
        'short': ath.get('shortName') or ath.get('displayName') or '',
        'pos': (dig(ath, 'position', 'abbreviation')
                or dig(ath, 'position', 'name') or ''),
        'jersey': str(ath.get('jersey') or ''),
        'team': team,
        'team_abbr': short_team,
        'headshot': dig(ath, 'headshot', 'href') or '',
    }


# ─────────────────────────────────────────────────────────────────────────────
#  Roster fallback (per team)
# ─────────────────────────────────────────────────────────────────────────────
def fetch_team_rosters(http, sport, league, team_ids, cap=40):
    """Fallback source: one roster request per team, with season splits."""
    pool = {}
    for tid in list(team_ids)[:cap]:
        url = f'{SITE_API}/{sport}/{league}/teams/{tid}/roster?enable=stats'
        data = http.get_json(url)
        if not data:
            continue
        team = (dig(data, 'team', 'displayName') or dig(data, 'team', 'name') or '')
        entries = []
        groups = data.get('athletes') or []
        for grp in groups:
            items = grp.get('items') if isinstance(grp, dict) and 'items' in grp else [grp]
            for ath in items or []:
                meta = _athlete_meta(ath)
                if not meta['name']:
                    continue
                meta['team'] = meta['team'] or team
                meta['stats'] = _roster_stats(ath, sport)
                if meta['stats']:
                    entries.append(meta)
        if entries:
            pool.setdefault(norm_team(team), []).extend(entries)
    return pool


def _roster_stats(ath, sport):
    stats = {}
    for block in (ath.get('statistics') or []):
        for split in (block.get('splits') or []):
            items = split.get('stats') or []
            if items and isinstance(items[0], dict):
                names = [i.get('name') or i.get('abbreviation') for i in items]
                values = [i.get('value', i.get('displayValue')) for i in items]
                found = extract_stats(sport, names, values)
                merged_avg = list(stats.get('__avg__', [])) + list(found.pop('__avg__', []))
                stats.update(found)
                if merged_avg:
                    stats['__avg__'] = merged_avg
    return stats


# ─────────────────────────────────────────────────────────────────────────────
#  Per-event extras: probable pitchers and game leaders
# ─────────────────────────────────────────────────────────────────────────────
def fetch_scoreboard(http, sport, league, yyyymmdd):
    url = f'{SITE_API}/{sport}/{league}/scoreboard?dates={yyyymmdd}'
    return http.get_json(url)


def probable_starters(scoreboard):
    """MLB probable pitchers, keyed by game id -> {'home': ..., 'away': ...}."""
    out = {}
    for ev in (scoreboard or {}).get('events') or []:
        comp = dig(ev, 'competitions', 0) or {}
        slot = {}
        for c in comp.get('competitors') or []:
            side = c.get('homeAway')
            probable = c.get('probables') or []
            if not probable:
                continue
            ath = dig(probable, 0, 'athlete') or {}
            if not ath:
                continue
            slot[side] = {
                'id': str(ath.get('id') or ''),
                'name': ath.get('displayName') or ath.get('shortName') or '',
                'headshot': dig(ath, 'headshot', 'href') or '',
                'summary': dig(probable, 0, 'statistics', 0, 'displayValue') or '',
            }
        # Older payloads put both pitchers under competition.probables.
        for probable in comp.get('probables') or []:
            ath = probable.get('athlete') or {}
            tid = str(dig(probable, 'team', 'id') or '')
            for c in comp.get('competitors') or []:
                if str(dig(c, 'team', 'id') or '') == tid and c.get('homeAway') not in slot:
                    slot[c['homeAway']] = {
                        'id': str(ath.get('id') or ''),
                        'name': ath.get('displayName') or '',
                        'headshot': dig(ath, 'headshot', 'href') or '',
                        'summary': '',
                    }
        if slot:
            out[str(ev.get('id') or '')] = slot
    return out


def event_leaders(scoreboard, sport):
    """Per-game season leaders ESPN already ships with the scoreboard.

    A thin but very reliable source: it needs no extra requests and always
    covers the teams actually playing today.
    """
    out = {}
    for ev in (scoreboard or {}).get('events') or []:
        comp = dig(ev, 'competitions', 0) or {}
        per_team = {}
        for c in comp.get('competitors') or []:
            team = dig(c, 'team', 'displayName') or ''
            people = []
            for cat in c.get('leaders') or []:
                for ldr in cat.get('leaders') or []:
                    ath = ldr.get('athlete') or {}
                    if not ath.get('displayName'):
                        continue
                    people.append({
                        **_athlete_meta(ath),
                        'team': team,
                        'leader_cat': cat.get('name') or '',
                        'leader_value': ldr.get('displayValue') or '',
                    })
            if people:
                per_team[c.get('homeAway') or team] = people
        if per_team:
            out[str(ev.get('id') or '')] = per_team
    return out


def fetch_team_index(http, sport, league):
    """team id -> display name, used by the roster fallback."""
    data = http.get_json(f'{SITE_API}/{sport}/{league}/teams?limit=50')
    out = {}
    for grp in dig(data, 'sports', 0, 'leagues', 0, 'teams') or []:
        t = grp.get('team') or {}
        if t.get('id'):
            out[str(t['id'])] = t.get('displayName') or t.get('name') or ''
    return out
