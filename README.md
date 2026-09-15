# Sports Predictions

Game predictions and player prop projections for MLB, NFL, NBA and NHL,
rebuilt every hour by GitHub Actions and served as a static site.

Live site: open `index.html` (or the per-sport pages `mlb.html`, `nfl.html`,
`nba.html`, `nhl.html`).

---

## What it does

**Game predictions.** Every scheduled game gets a win probability, a
confidence tier, and a plain-language breakdown of what is driving the pick —
Elo edge, recent form, scoring margin, rest, home/road splits, head-to-head.

**Player props.** Open a game, pick a side, pick a player, and you get their
most-shopped markets: hits, total bases, home runs, RBIs and strikeouts in
baseball; points, rebounds, assists and threes in basketball; passing,
rushing and receiving lines in football; shots, points and saves in hockey.
Each carries a projection, a line, a likely range, and an over/under
probability. A league-wide **Player Props** board ranks the whole slate.

**Honest accuracy reporting.** The Model tab shows out-of-sample accuracy,
Brier score, log loss and a calibration table — not in-sample numbers.

---

## How the model works

```
ESPN  ──R──▶  enriched CSV  ──Python──▶  archive + model + props  ──▶  static site
```

1. **Ingest** (`generate_*_predictions.R`) pulls standings and schedules from
   ESPN into a per-league CSV. The window is set by `SP_LOOKBACK_DAYS` and
   `SP_FORWARD_DAYS`.
2. **Archive** (`sportspred/learn.py`) folds each window's completed games
   into `history/<league>_games.csv`. The ingest only ever sees a few weeks;
   the archive is what gives the ratings a season-long memory.
3. **Rate** (`sportspred/ratings.py`) replays the archive through Elo with a
   margin-of-victory multiplier and between-season regression. Ratings are
   always *pre-game*.
4. **Feature** (`sportspred/features.py`) builds one vector per game from
   games that finished before it: Elo edge, season-to-date Pythagorean, recent
   form and scoring margin, home/road split, rest, back-to-backs,
   head-to-head.
5. **Fit** (`sportspred/model.py`) trains a ridge-penalised logistic
   regression, picks the penalty and the Elo/model blend by walk-forward
   validation, and recalibrates with Platt scaling.
6. **Project** (`sportspred/props.py`) turns each player's season line into a
   per-game rate, adjusts it for the opponent and the projected game
   environment, and prices it with a Poisson, negative binomial or normal
   distribution.
7. **Render** (`sportspred/render.py`) writes a data file per league plus a
   few kilobytes of HTML shell.

### Why the accuracy number went down

The previous model reported about 58.6% on MLB. That figure was measured by
scoring past games with **end-of-season standings** — it knew how the season
turned out. Rebuilt so that each game is scored only with information that
existed beforehand, the honest out-of-sample number is lower. Nothing got
worse; the measurement got truthful. The published pick is an ensemble of the
learned model and the standings model, and the Model tab reports what it
actually achieved.

### How it improves itself

Three mechanisms, all persisted to the repository so every run starts from
what the last one learned:

- **The archive grows.** More history means better-separated Elo ratings and
  more training rows, so the learned model earns more weight over time.
- **The prediction ledger.** Every forecast is written to
  `history/<league>_ledger.csv` *before* kickoff and graded afterwards. This
  is the only measurement in the system that hindsight cannot flatter. Once it
  holds enough graded games, it — not a fixed schedule — sets how far to lean
  on the learned model versus the standings model, and recalibrates the output.
- **Champion/challenger.** Each run re-tunes the Elo parameters, the ridge
  penalty and the blend weight, but new parameters are adopted only if they
  beat the incumbent on validation. A noisy hour cannot make the model worse.
  The Model tab lists what was adopted and why.

---

## Running it locally

```bash
python3 run_pipeline.py                 # every league, with player props
python3 run_pipeline.py mlb nfl         # only these leagues
python3 run_pipeline.py --no-props      # skip the player-statistics fetch
python3 run_pipeline.py --no-tune       # reuse the stored Elo parameters
python3 -m unittest discover -s tests   # the test suite
```

Only the standard library is required. The R ingest needs `httr`, `jsonlite`
and `dplyr`.

To refresh the source CSVs you also need the ingest step:

```bash
SP_LOOKBACK_DAYS=120 Rscript generate_mlb_predictions.R
```

---

## Layout

| Path | What it is |
| --- | --- |
| `run_pipeline.py` | Entry point: model, props, site |
| `sportspred/` | The engine (see the pipeline steps above) |
| `assets/app.css`, `assets/app.js` | Shared front end, cached across all four sports |
| `data/<league>.js` | Per-league payload the page renders from — one JSON object behind a `window.SP_DATA[...] =` assignment |
| `data/<league>-history.js` | Older graded games, loaded only when the Results tab is opened |
| `history/` | Game archive and prediction ledger — the long-term memory |
| `model_state/` | Tuned parameters, Elo snapshot, run log |
| `generate_*_predictions.R` | ESPN ingest |
| `tests/` | Test suite |

---

## A note on the numbers

These are model projections, not betting advice and not sportsbook lines.
Prop lines here are anchored to each player's own season baseline, so a "lean"
means the model disagrees with that baseline for this matchup — it says
nothing about whether a real market price offers value. Check the actual
market before acting on anything here.
