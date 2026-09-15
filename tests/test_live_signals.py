"""Injuries, box scores, the props ledger, and the MLB starter edge."""
import os
import shutil
import tempfile
import unittest
from datetime import date, timedelta

from sportspred import config, espn, pipeline, props
from sportspred.learn import PropsLedger
from sportspred.util import Http, read_csv, write_csv
from tests import espn_fixtures as fx
from tests.helpers import player_pool, synthetic_rows


# ─────────────────────────────────────────────────────────────────────────────
#  Fixtures for the endpoints the R scripts never touched
# ─────────────────────────────────────────────────────────────────────────────
def injuries_payload():
    """Team-grouped layout: injuries[].injuries[]."""
    return {'injuries': [
        {'id': '1', 'displayName': 'Boston Red Sox', 'injuries': [
            {'athlete': {'id': '501', 'displayName': 'Slugger One',
                         'position': {'abbreviation': 'RF'}},
             'status': 'Out', 'details': {'type': 'Hamstring'}},
            {'athlete': {'id': '502', 'displayName': 'Catcher Two',
                         'position': {'abbreviation': 'C'}},
             'status': 'Day-To-Day', 'details': {'type': 'Illness'}},
            {'athlete': {'id': '503', 'displayName': 'Fine Player',
                         'position': {'abbreviation': '1B'}},
             'status': 'Active'},
        ]},
    ]}


def injuries_flat_payload():
    """Flat layout some sports use: injuries[] each with an athlete + team."""
    return {'injuries': [
        {'athlete': {'id': '900', 'displayName': 'Wing Back',
                     'position': {'abbreviation': 'LW'},
                     'team': {'displayName': 'Toronto Maple Leafs'}},
         'status': 'Injured Reserve', 'shortComment': 'Knee'},
    ]}


def summary_payload(final=True, sport='baseball'):
    status = {'type': {'name': 'STATUS_FINAL' if final else 'STATUS_IN_PROGRESS',
                       'completed': final, 'shortDetail': 'Final' if final else 'Top 5th'}}
    if sport == 'baseball':
        players = [{'team': {'displayName': 'Team A'}, 'statistics': [
            {'name': 'batting', 'labels': ['AB', 'R', 'H', 'RBI', 'HR', 'BB', 'K', 'AVG'],
             'athletes': [
                 {'athlete': {'id': 'b0', 'displayName': 'Team A Batter 0'},
                  'stats': ['4', '1', '2', '1', '1', '0', '1', '.301']},
                 {'athlete': {'id': 'b1', 'displayName': 'Team A Batter 1'},
                  'stats': ['3', '0', '0', '0', '0', '1', '2', '.250']},
                 {'athlete': {'id': 'b5', 'displayName': 'Bench Guy'}, 'stats': [],
                  'didNotPlay': True},
             ]},
            {'name': 'pitching', 'labels': ['IP', 'H', 'R', 'ER', 'BB', 'K', 'HR', 'PC-ST', 'ERA'],
             'athletes': [
                 {'athlete': {'id': 'p0', 'displayName': 'Team A Pitcher 0'},
                  'stats': ['6.2', '5', '2', '2', '1', '8', '1', '98-64', '3.40']}]},
        ]}]
    elif sport == 'football':
        players = [{'team': {'displayName': 'Team A'}, 'statistics': [
            {'name': 'passing', 'labels': ['C/ATT', 'YDS', 'AVG', 'TD', 'INT', 'SACKS', 'QBR', 'RTG'],
             'athletes': [{'athlete': {'id': 'q1', 'displayName': 'QB'},
                           'stats': ['24/35', '287', '8.2', '2', '1', '2-14', '71.3', '101.2']}]},
            {'name': 'rushing', 'labels': ['CAR', 'YDS', 'AVG', 'TD', 'LONG'],
             'athletes': [{'athlete': {'id': 'r1', 'displayName': 'RB'},
                           'stats': ['18', '92', '5.1', '1', '24']},
                          {'athlete': {'id': 'q1', 'displayName': 'QB'},
                           'stats': ['3', '12', '4.0', '0', '7']}]},
            {'name': 'receiving', 'labels': ['REC', 'YDS', 'AVG', 'TD', 'LONG', 'TGTS'],
             'athletes': [{'athlete': {'id': 'w1', 'displayName': 'WR'},
                           'stats': ['7', '104', '14.9', '1', '38', '9']},
                          {'athlete': {'id': 'r1', 'displayName': 'RB'},
                           'stats': ['3', '21', '7.0', '0', '11', '4']}]},
        ]}]
    elif sport == 'hockey':
        players = [{'team': {'displayName': 'Team A'}, 'statistics': [
            {'name': 'forwards', 'labels': ['G', 'A', '+/-', 'S', 'SOG', 'HITS', 'BS', 'PN', 'PIM', 'TOI'],
             'athletes': [{'athlete': {'id': 'f1', 'displayName': 'Sniper'},
                           'stats': ['1', '1', '+2', '5', '5', '2', '1', '0', '0', '18:42']}]},
            {'name': 'goalies', 'labels': ['SA', 'GA', 'SV', 'SV%', 'TOI'],
             'athletes': [{'athlete': {'id': 'g1', 'displayName': 'Wall'},
                           'stats': ['31', '2', '29', '.935', '60:00']}]},
        ]}]
    else:
        players = [{'team': {'displayName': 'Team A'}, 'statistics': [
            {'name': '', 'labels': ['MIN', 'FG', '3PT', 'FT', 'OREB', 'DREB', 'REB', 'AST',
                                    'STL', 'BLK', 'TO', 'PF', '+/-', 'PTS'],
             'names': ['minutes', 'fieldGoalsMade-fieldGoalsAttempted',
                       'threePointFieldGoalsMade-threePointFieldGoalsAttempted',
                       'freeThrowsMade-freeThrowsAttempted', 'offensiveRebounds',
                       'defensiveRebounds', 'rebounds', 'assists', 'steals', 'blocks',
                       'turnovers', 'fouls', 'plusMinus', 'points'],
             'athletes': [{'athlete': {'id': 'n1', 'displayName': 'Star'},
                           'stats': ['36:10', '11-22', '4-9', '6-7', '1', '6', '7', '9',
                                     '1', '1', '3', '2', '+12', '32']}]},
        ]}]
    return {'header': {'competitions': [{'status': status}]},
            'boxscore': {'players': players}}


# ─────────────────────────────────────────────────────────────────────────────
class TestInjuries(unittest.TestCase):
    def test_team_grouped_layout(self):
        http = fx.FakeHttp([('/injuries', injuries_payload())])
        rep = espn.fetch_injuries(http, 'baseball', 'mlb')
        sox = rep[espn.norm_team('Boston Red Sox')]
        self.assertEqual(sox['501']['level'], 'out')
        self.assertEqual(sox['502']['level'], 'limited')
        self.assertNotIn('503', sox)                 # active players are not listed

    def test_flat_layout(self):
        http = fx.FakeHttp([('/injuries', injuries_flat_payload())])
        rep = espn.fetch_injuries(http, 'hockey', 'nhl')
        leafs = rep[espn.norm_team('Toronto Maple Leafs')]
        self.assertEqual(leafs['900']['level'], 'out')
        self.assertEqual(leafs['900']['detail'], 'Knee')

    def test_status_classification(self):
        for status, want in (('Out', 'out'), ('Injured Reserve', 'out'), ('Suspended', 'out'),
                             ('Out For Season', 'out'), ('Questionable', 'limited'),
                             ('Day-To-Day', 'limited'), ('Probable', 'limited'),
                             ('Active', 'active'), ('', 'active')):
            self.assertEqual(espn.classify_status(status), want, status)

    def test_outage_yields_empty(self):
        self.assertEqual(espn.fetch_injuries(fx.FakeHttp([]), 'baseball', 'mlb'), {})


class TestBoxScores(unittest.TestCase):
    def test_baseball_batting_and_pitching(self):
        box = espn.boxscore_player_stats(summary_payload(sport='baseball'), 'baseball')
        self.assertEqual(box['b0']['stats']['hits'], 2)
        self.assertEqual(box['b0']['stats']['tb'], 5)             # single + HR
        self.assertEqual(box['p0']['stats']['p_so'], 8)
        self.assertEqual(box['p0']['stats']['outs'], 20)          # 6.2 IP
        self.assertFalse(box['b5']['played'])

    def test_football_categories_disambiguate_yards(self):
        box = espn.boxscore_player_stats(summary_payload(sport='football'), 'football')
        qb, rb, wr = box['q1']['stats'], box['r1']['stats'], box['w1']['stats']
        self.assertEqual(qb['pass_cmp'], 24)
        self.assertEqual(qb['pass_yds'], 287)
        self.assertEqual(qb['rush_yds'], 12)                      # same player, other category
        self.assertEqual(rb['rush_att'], 18)
        self.assertEqual(rb['scrim_yds'], 92 + 21)
        self.assertEqual(rb['td'], 1)
        self.assertEqual(wr['rec'], 7)
        self.assertEqual(wr['targets'], 9)

    def test_hockey_skaters_and_goalies(self):
        box = espn.boxscore_player_stats(summary_payload(sport='hockey'), 'hockey')
        self.assertEqual(box['f1']['stats']['sog'], 5)
        self.assertEqual(box['f1']['stats']['points'], 2)
        self.assertAlmostEqual(box['f1']['stats']['toi'], 18.7, places=1)
        self.assertEqual(box['g1']['stats']['saves'], 29)
        self.assertEqual(box['g1']['stats']['ga'], 2)

    def test_basketball_names_and_combos(self):
        box = espn.boxscore_player_stats(summary_payload(sport='basketball'), 'basketball')
        st = box['n1']['stats']
        self.assertEqual(st['pts'], 32)
        self.assertEqual(st['fg3'], 4)
        self.assertEqual(st['pra'], 32 + 7 + 9)
        self.assertEqual(st['stlblk'], 2)

    def test_game_state(self):
        self.assertEqual(espn.game_state(summary_payload(final=True))[0], 'final')
        self.assertEqual(espn.game_state(summary_payload(final=False))[0], 'live')
        self.assertEqual(espn.game_state({})[0], 'scheduled')

    def test_empty_summary(self):
        self.assertEqual(espn.boxscore_player_stats({}, 'baseball'), {})


class TestPropsInjuries(unittest.TestCase):
    def setUp(self):
        self.cfg = config.LEAGUES['mlb']
        self.pool = player_pool(['Team A', 'Team B'])
        self.env = props.team_environment(
            [dict(final=True, home='Team A', away='Team B', home_score=5, away_score=4)] * 10,
            self.cfg)
        self.game = {'home': 'Team A', 'away': 'Team B', 'game_id': '1'}

    def _board(self, injuries):
        return props.build_for_game(self.game, self.pool, self.env, self.cfg,
                                    'baseball', 0.55, injuries=injuries)

    def test_out_players_are_removed_and_listed(self):
        first = self.pool['teama'][0]
        inj = {'teama': {first['id']: {'name': first['name'], 'pos': first['pos'],
                                       'status': 'Out', 'level': 'out', 'detail': 'Wrist'}}}
        board = self._board(inj)
        self.assertNotIn(first['name'], [p['name'] for p in board['home']])
        self.assertEqual(board['home_out'][0]['name'], first['name'])
        self.assertEqual(board['away_out'], [])

    def test_limited_players_stay_flagged_and_dampened(self):
        first = self.pool['teama'][0]
        clean = self._board(None)
        inj = {'teama': {first['id']: {'name': first['name'], 'status': 'Questionable',
                                       'level': 'limited', 'detail': ''}}}
        board = self._board(inj)
        before = next(p for p in clean['home'] if p['name'] == first['name'])
        after = next(p for p in board['home'] if p['name'] == first['name'])
        self.assertEqual(after['status'], 'Questionable')
        self.assertLess(after['props'][0]['proj'], before['props'][0]['proj'])

    def test_tuning_moves_projections(self):
        clean = self._board(None)
        key = clean['home'][0]['props'][0]['key']
        tuned = props.build_for_game(self.game, self.pool, self.env, self.cfg, 'baseball', 0.55,
                                     tuning={key: {'bias': 1.2, 'spread': 1.0, 'n': 100}})
        a = clean['home'][0]['props'][0]['proj']
        b = next(p for p in tuned['home'][0]['props'] if p['key'] == key)['proj']
        self.assertAlmostEqual(b / a, 1.2, places=2)


class TestPropsLedger(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.ledger = PropsLedger('mlb', history_dir=self.dir)
        self.game = {'game_id': 'g1', 'date': date.today()}
        self.player = {'id': 'b0', 'name': 'Team A Batter 0'}

    def tearDown(self):
        shutil.rmtree(self.dir, ignore_errors=True)

    def _prop(self, key='hits', stat='hits_pg', line=0.5, proj=1.1, pick='over', dist='poisson'):
        return {'key': key, 'label': key, 'stat': stat, 'dist': dist, 'line': line,
                'proj': proj, 'season': proj, 'over': 0.66 if pick == 'over' else 0.34,
                'pick': pick, 'conf': 'med'}

    def test_records_once(self):
        self.assertTrue(self.ledger.record(self.game, 'home', self.player, self._prop()))
        self.assertFalse(self.ledger.record(self.game, 'home', self.player, self._prop(proj=9)))
        self.assertEqual(len(self.ledger.rows), 1)

    def test_grades_a_hit_and_a_miss(self):
        self.ledger.record(self.game, 'home', self.player, self._prop())          # over 0.5 hits
        self.ledger.record(self.game, 'home', {'id': 'b1', 'name': 'B1'},
                           self._prop(pick='over'))                               # b1 had 0 hits
        box = espn.boxscore_player_stats(summary_payload(), 'baseball')
        self.assertEqual(self.ledger.grade_game('g1', box), 2)
        rows = {r['athlete_id']: r for r in self.ledger.rows.values()}
        self.assertEqual(rows['b0']['hit'], '1')
        self.assertEqual(rows['b1']['hit'], '0')

    def test_a_push_is_neither(self):
        self.ledger.record(self.game, 'home', self.player, self._prop(line=2.0))
        box = espn.boxscore_player_stats(summary_payload(), 'baseball')     # 2 hits
        self.ledger.grade_game('g1', box)
        row = list(self.ledger.rows.values())[0]
        self.assertEqual(row['push'], '1')
        self.assertEqual(self.ledger.graded(), [])

    def test_a_scratch_is_graded_but_not_scored(self):
        self.ledger.record(self.game, 'home', {'id': 'b5', 'name': 'Bench'}, self._prop())
        box = espn.boxscore_player_stats(summary_payload(), 'baseball')
        self.ledger.grade_game('g1', box)
        row = list(self.ledger.rows.values())[0]
        self.assertEqual(row['played'], '0')
        self.assertEqual(self.ledger.scorecard()['total'], 0)

    def test_scorecard_by_market_and_confidence(self):
        self.ledger.record(self.game, 'home', self.player, self._prop())
        self.ledger.record(self.game, 'home', self.player, self._prop(key='hr', stat='hr_pg',
                                                                       pick='under'))  # had 1 HR
        box = espn.boxscore_player_stats(summary_payload(), 'baseball')
        self.ledger.grade_game('g1', box)
        card = self.ledger.scorecard()
        self.assertEqual(card['total'], 2)
        self.assertEqual(card['by_key']['hits']['hit'], 1)
        self.assertEqual(card['by_key']['hr']['hit'], 0)

    def test_tuning_needs_thirty_graded(self):
        for i in range(29):
            self.ledger.rows[str(i)] = {'key': 'hits', 'stat': 'hits_pg', 'dist': 'poisson',
                                        'proj': '1.0', 'actual': '1.2', 'graded': '1',
                                        'played': '1', 'push': '0', 'hit': '1'}
        self.assertEqual(self.ledger.tune(), {})

    def test_tuning_detects_systematic_bias(self):
        for i in range(200):
            self.ledger.rows[str(i)] = {'key': 'hits', 'stat': 'hits_pg', 'dist': 'poisson',
                                        'proj': '1.0', 'actual': str(1.3 if i % 2 else 1.1),
                                        'graded': '1', 'played': '1', 'push': '0', 'hit': '1'}
        t = self.ledger.tune()['hits']
        self.assertGreater(t['bias'], 1.1)
        self.assertEqual(t['n'], 200)
        # The correction is bounded so a bad month cannot swing a market wildly.
        self.assertLessEqual(t['bias'], 1.5)
        self.assertGreaterEqual(t['spread'], 0.67)

    def test_ungraded_games_only_lists_finished_ones(self):
        self.ledger.record(self.game, 'home', self.player, self._prop())
        self.assertEqual(self.ledger.ungraded_games({'g1', 'g2'}), ['g1'])
        self.assertEqual(self.ledger.ungraded_games({'g2'}), [])

    def test_round_trips_through_disk(self):
        self.ledger.record(self.game, 'home', self.player, self._prop())
        self.ledger.save()
        again = PropsLedger('mlb', history_dir=self.dir)
        self.assertEqual(len(again.rows), 1)


class TestStarterEdge(unittest.TestCase):
    def _records(self, away_id, home_id):
        return [{'game': {'game_id': 'g1', 'row': {'away_probable_id': away_id,
                                                    'home_probable_id': home_id}}}]

    def _pool(self, away_era, home_era, starts=20):
        return {'teama': [{'id': 'pa', 'name': 'A', 'stats': {'era': away_era, 'starts': starts, 'gp': starts}}],
                'teamb': [{'id': 'ph', 'name': 'H', 'stats': {'era': home_era, 'starts': starts, 'gp': starts}}]}

    def test_better_home_starter_favours_home(self):
        edges = pipeline.starter_edge_by_game(self._records('pa', 'ph'), self._pool(4.5, 3.0))
        self.assertAlmostEqual(edges['g1'], pipeline.STARTER_COEF * 1.5, places=6)

    def test_edge_is_capped(self):
        edges = pipeline.starter_edge_by_game(self._records('pa', 'ph'), self._pool(9.0, 1.0))
        self.assertEqual(edges['g1'], pipeline.STARTER_CAP)

    def test_too_few_starts_means_no_edge(self):
        self.assertEqual(pipeline.starter_edge_by_game(self._records('pa', 'ph'),
                                                       self._pool(4.5, 3.0, starts=2)), {})

    def test_missing_probable_means_no_edge(self):
        self.assertEqual(pipeline.starter_edge_by_game(self._records('', 'ph'),
                                                       self._pool(4.5, 3.0)), {})

    def test_edge_shifts_the_prior_not_the_learned_model(self):
        from sportspred import model
        rows = synthetic_rows(n_days=130, seed=61)
        trained = model.train('mlb', rows, config.LEAGUES['mlb'], tune=False)
        # A game sitting inside the league cap, so a shift has room to move it.
        rec = next(r for r in trained['records'] if not r['game']['final']
                   and 0.35 < model.predict(r, trained, config.LEAGUES['mlb'], 'mlb')['prob'] < 0.65)
        base = model.predict(rec, trained, config.LEAGUES['mlb'], 'mlb')
        shifted = model.predict(rec, trained, config.LEAGUES['mlb'], 'mlb', prior_shift=0.3)
        self.assertGreater(shifted['prior_prob'], base['prior_prob'])
        self.assertEqual(shifted['elo_prob'], base['elo_prob'])
        self.assertGreater(shifted['prob'], base['prob'])
        self.assertEqual(shifted['starter_edge'], 0.3)


class TestPipelineWithLiveSignals(unittest.TestCase):
    """The whole thing against a fake ESPN: props priced, recorded, graded."""

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.saved = (config.HISTORY_DIR, config.STATE_DIR, config.DATA_DIR, config.BASE)
        config.HISTORY_DIR = os.path.join(self.dir, 'history')
        config.STATE_DIR = os.path.join(self.dir, 'model_state')
        config.DATA_DIR = os.path.join(self.dir, 'data')
        config.BASE = self.dir
        pipeline._POOL_CACHE.clear()
        self.csv = os.path.join(self.dir, config.LEAGUES['mlb']['csv_file'])
        write_csv(self.csv, synthetic_rows(n_days=60, seed=71))

    def tearDown(self):
        (config.HISTORY_DIR, config.STATE_DIR, config.DATA_DIR, config.BASE) = self.saved
        pipeline._POOL_CACHE.clear()
        shutil.rmtree(self.dir, ignore_errors=True)

    def _http(self, summaries=None):
        pool_payload = {'categories': [], 'athletes': []}
        # byathlete layout with per-athlete names, for a Team A / Team B pool.
        for team, roster in player_pool(['Team A', 'Team B']).items():
            for p in roster:
                names = list(p['stats'].keys())
                pool_payload['athletes'].append({
                    'athlete': {'id': p['id'] + team, 'displayName': p['name'],
                                'position': {'abbreviation': p['pos']},
                                'team': {'displayName': p['team']}},
                    'categories': [{'name': 'general', 'names': names,
                                    'totals': [p['stats'][n] for n in names]}]})
        routes = [('statistics/byathlete', pool_payload),
                  ('/injuries', {'injuries': []}),
                  ('scoreboard', {'events': []})]
        for gid, payload in (summaries or {}).items():
            routes.append((f'summary?event={gid}', payload))
        return fx.FakeHttp(routes)

    def test_props_are_priced_and_recorded_before_kickoff(self):
        payload, _, _ = pipeline.run('mlb', fetch_props=True, http=self._http(), tune=False)
        self.assertEqual(payload['props_status'], 'live')
        with_props = [g for g in payload['games'] if g.get('props')]
        self.assertTrue(with_props)
        ledger = read_csv(os.path.join(config.HISTORY_DIR, 'mlb_props.csv'))
        self.assertTrue(ledger)
        self.assertTrue(all(r['graded'] == '0' for r in ledger))

    def test_finished_games_get_graded_next_run(self):
        pipeline.run('mlb', fetch_props=True, http=self._http(), tune=False)
        ledger = read_csv(os.path.join(config.HISTORY_DIR, 'mlb_props.csv'))
        gid = ledger[0]['game_id']
        # Play the game out and provide its box score.
        rows = read_csv(self.csv)
        for r in rows:
            if r['game_id'] == gid:
                r['status'], r['winner'] = 'Final', r['home_team']
                r['home_score'], r['away_score'] = '6', '2'
        write_csv(self.csv, rows)
        athlete_ids = {r['athlete_id'] for r in ledger if r['game_id'] == gid}
        summary = {'header': {'competitions': [{'status': {'type': {'name': 'STATUS_FINAL',
                                                                      'completed': True}}}]},
                   'boxscore': {'players': [{'team': {'displayName': 'x'}, 'statistics': [
                       {'name': 'batting', 'labels': ['AB', 'R', 'H', 'RBI', 'HR'],
                        'athletes': [{'athlete': {'id': aid}, 'stats': ['4', '1', '2', '1', '0']}
                                     for aid in athlete_ids]},
                       {'name': 'pitching', 'labels': ['IP', 'H', 'R', 'ER', 'BB', 'K'],
                        'athletes': [{'athlete': {'id': aid}, 'stats': ['6.0', '5', '2', '2', '1', '6']}
                                     for aid in athlete_ids]}]}]}}
        payload, _, _ = pipeline.run('mlb', fetch_props=True,
                                     http=self._http({gid: summary}), tune=False)
        ledger = read_csv(os.path.join(config.HISTORY_DIR, 'mlb_props.csv'))
        graded = [r for r in ledger if r['game_id'] == gid and r['graded'] == '1']
        self.assertTrue(graded)
        self.assertTrue(any(r['hit'] in ('0', '1') for r in graded))
        self.assertGreater(payload['props_record']['total'], 0)

    def test_feed_outage_falls_back_to_cache(self):
        pipeline.run('mlb', fetch_props=True, http=self._http(), tune=False)
        pipeline._POOL_CACHE.clear()
        class Dead(Http):
            def get_json(self, url, cache=True):
                return None
        payload, _, _ = pipeline.run('mlb', fetch_props=True, http=Dead(), tune=False)
        self.assertEqual(payload['props_status'], 'cached')
        self.assertTrue(any(g.get('props') for g in payload['games']))


if __name__ == '__main__':
    unittest.main()
