#!/usr/bin/env Rscript
# ============================================================
# MLB Predictions — generate_mlb_predictions.R
# Fetches ESPN standings + schedule, fits logistic regression
# on current-season completed games, outputs enriched CSV.
#
# Key variables (research-backed importance):
#   Run Diff/G  35% weight  — Pythagorean wins; strongest MLB predictor
#   Win%        20%         — overall team quality
#   ERA         25%         — pitching (starter + bullpen combined)
#   RS/G        10%         — offensive output
#   RA/G        10%         — defensive / pitching quality check
#
# Home field advantage: ~54% MLB home win rate (~0.10 log-odds)
# Pythagorean exponent: 1.83 (MLB standard)
# ============================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
})

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

SCRIPT_DIR <- tryCatch(
  dirname(normalizePath(sys.frames()[[1]]$ofile)),
  error = function(e) getwd()
)
OUT_CSV <- file.path(SCRIPT_DIR, "mlb_predictions.csv")

cat("=== MLB Predictions Generator ===\n")
cat(sprintf("Output: %s\n\n", OUT_CSV))

httr::set_config(httr::config(ssl_verifypeer = FALSE))

# ── 1. Fetch Standings ────────────────────────────────────────────────────
cat("[1/5] Fetching MLB standings…\n")

resp      <- GET("https://site.api.espn.com/apis/v2/sports/baseball/mlb/standings")
standings <- content(resp, "parsed")

team_stats <- list()

parse_entries <- function(entries) {
  for (entry in entries %||% list()) {
    team <- entry$team %||% NULL
    if (is.null(team)) next
    id   <- as.character(team$id   %||% "")
    name <- as.character(team$displayName %||% team$name %||% "")
    abbr <- as.character(team$abbreviation %||% "")

    sv <- list()
    for (s in entry$stats %||% list()) {
      sv[[s$name %||% ""]] <- s$value
      if (!is.null(s$abbreviation)) sv[[s$abbreviation]] <- s$value
    }

    w  <- as.numeric(sv[["wins"]]   %||% sv[["W"]] %||% 0)
    l  <- as.numeric(sv[["losses"]] %||% sv[["L"]] %||% 0)
    gp <- w + l
    win_pct <- if (gp > 0) w / gp else 0.5

    # Run differential from standings
    rs <- as.numeric(sv[["Runs Scored"]]  %||% sv[["RS"]] %||%
                     sv[["runsScored"]]   %||% 0)
    ra <- as.numeric(sv[["Runs Against"]] %||% sv[["RA"]] %||%
                     sv[["runsAllowed"]]  %||% 0)
    rs_pg      <- if (gp > 0) rs / gp else 4.5
    ra_pg      <- if (gp > 0) ra / gp else 4.5
    run_diff_pg <- rs_pg - ra_pg

    # Pythagorean win% (more stable predictor than actual win%)
    pyth_win_pct <- if (rs + ra > 0) {
      rs^1.83 / (rs^1.83 + ra^1.83)
    } else 0.5

    record <- paste0(w, "-", l)

    # Composite strength — Pythagorean + run diff are primary
    strength <- 0.40 * pyth_win_pct +
                0.20 * win_pct +
                0.40 * pmin(pmax((run_diff_pg + 3) / 6, 0), 1)

    info <- list(
      id = id, name = name, abbr = abbr,
      record = record, strength = strength, gp = gp,
      win_pct      = round(win_pct,      3),
      pyth_win_pct = round(pyth_win_pct, 3),
      run_diff_pg  = round(run_diff_pg,  2),
      rs_pg        = round(rs_pg,        2),
      ra_pg        = round(ra_pg,        2),
      era          = NA_real_   # filled in step 2
    )
    if (nchar(id)   > 0) team_stats[[id]]             <<- info
    if (nchar(name) > 0) team_stats[[tolower(name)]]  <<- info
    if (nchar(abbr) > 0) team_stats[[tolower(abbr)]]  <<- info
  }
}

walk_groups <- function(node) {
  if (!is.null(node$standings$entries)) parse_entries(node$standings$entries)
  if (!is.null(node$entries))           parse_entries(node$entries)
  for (child in node$children %||% list()) walk_groups(child)
}
walk_groups(standings)
n_teams <- length(unique(sapply(team_stats, `[[`, "id")))
cat(sprintf("  Loaded stats for %d teams\n", n_teams))

# ── 2. Fetch Team ERA ─────────────────────────────────────────────────────
cat("[2/5] Fetching team ERA from ESPN statistics…\n")

# Get unique team IDs only (avoid duplicates from name/abbr indexing)
unique_ids <- unique(sapply(Filter(function(x) nchar(x$id) > 0, team_stats), `[[`, "id"))
era_fetched <- 0

for (tid in unique_ids) {
  url  <- paste0("https://site.api.espn.com/apis/site/v2/sports/baseball/mlb/teams/", tid, "/statistics")
  resp <- tryCatch(GET(url, timeout(10)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) { Sys.sleep(0.15); next }
  data <- tryCatch(content(resp, "parsed"), error = function(e) NULL)
  if (is.null(data)) { Sys.sleep(0.15); next }

  era_val <- NA_real_
  for (cat in data$splits$categories %||% list()) {
    if (tolower(cat$name %||% "") %in% c("pitching","pitchingstats")) {
      for (s in cat$stats %||% list()) {
        if (tolower(s$name %||% "") == "earnedrunaverage" ||
            tolower(s$abbreviation %||% "") == "era") {
          era_val <- suppressWarnings(as.numeric(s$value %||% NA))
          break
        }
      }
    }
    if (!is.na(era_val)) break
  }

  if (!is.na(era_val)) {
    # Update all keys that point to this team
    for (key in names(team_stats)) {
      if (identical(team_stats[[key]]$id, tid)) {
        team_stats[[key]]$era     <<- round(era_val, 2)
        team_stats[[key]]$strength <<- 0.35 * team_stats[[key]]$pyth_win_pct +
                                       0.20 * team_stats[[key]]$win_pct +
                                       0.25 * pmin(pmax((team_stats[[key]]$run_diff_pg + 3) / 6, 0), 1) +
                                       0.20 * pmin(pmax((6.0 - era_val) / 4.0, 0), 1)
      }
    }
    era_fetched <- era_fetched + 1
  }
  Sys.sleep(0.15)
}
cat(sprintf("  ERA fetched for %d / %d teams\n", era_fetched, length(unique_ids)))

# ── 3. Fetch Schedule ─────────────────────────────────────────────────────
cat("[3/5] Fetching MLB schedule (last 60 days + next 14 days)…\n")

lookup <- function(name, id = NULL) {
  if (!is.null(id) && !is.null(team_stats[[as.character(id)]])) return(team_stats[[as.character(id)]])
  if (!is.null(name)) {
    k <- tolower(as.character(name))
    if (!is.null(team_stats[[k]])) return(team_stats[[k]])
    for (w in strsplit(k, " ")[[1]]) if (!is.null(team_stats[[w]])) return(team_stats[[w]])
  }
  NULL
}

logistic <- function(x) 1 / (1 + exp(-x))
HOME_ADV <- 0.10   # ~54% MLB home win rate — smallest home advantage in major sports
SCALE    <- 5.0

parse_day <- function(date_str) {
  dc  <- gsub("-", "", date_str)
  url <- paste0("https://site.api.espn.com/apis/site/v2/sports/baseball/mlb/scoreboard?dates=", dc)
  resp <- tryCatch(GET(url, timeout(15)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(data.frame())
  data <- content(resp, "parsed")

  out <- lapply(data$events %||% list(), function(ev) {
    comp  <- (ev$competitions %||% list(list()))[[1]]
    comps <- comp$competitors %||% list()
    away_c <- Filter(function(c) identical(c$homeAway, "away"), comps)[[1]] %||% list()
    home_c <- Filter(function(c) identical(c$homeAway, "home"), comps)[[1]] %||% list()

    stype    <- comp$status$type$name %||% ""
    is_final <- stype == "STATUS_FINAL" || isTRUE(comp$status$type$completed)
    if (stype %in% c("STATUS_POSTPONED","STATUS_CANCELED")) return(NULL)

    away_name  <- as.character(away_c$team$displayName %||% "")
    home_name  <- as.character(home_c$team$displayName %||% "")
    away_id    <- as.character(away_c$team$id %||% "")
    home_id    <- as.character(home_c$team$id %||% "")
    away_score <- as.character(away_c$score %||% "")
    home_score <- as.character(home_c$score %||% "")
    away_rec   <- as.character(((away_c$records %||% list(list()))[[1]])$summary %||% "")
    home_rec   <- as.character(((home_c$records %||% list(list()))[[1]])$summary %||% "")

    # Probable pitchers (sometimes in notes or competition details)
    away_starter <- ""
    home_starter <- ""
    for (note in comp$notes %||% list()) {
      txt <- as.character(note$headline %||% "")
      if (grepl("vs\\.", txt, ignore.case=TRUE)) {
        parts <- strsplit(txt, " vs\\. ")[[1]]
        if (length(parts) == 2) {
          away_starter <- trimws(parts[[1]])
          home_starter <- trimws(parts[[2]])
        }
      }
    }
    # Also check probables
    for (prob in comp$probables %||% list()) {
      pname <- as.character((prob$athlete %||% list())$displayName %||% "")
      pteam <- as.character((prob$team %||% list())$id %||% "")
      if (pteam == away_id) away_starter <- pname
      if (pteam == home_id) home_starter <- pname
    }

    at <- lookup(away_name, away_id)
    ht <- lookup(home_name, home_id)

    away_str <- if (!is.null(at)) at$strength else 0.5
    home_str <- if (!is.null(ht)) ht$strength else 0.5
    home_p   <- logistic(SCALE * (home_str - away_str) + HOME_ADV)
    away_p   <- 1 - home_p
    favored  <- if (home_p >= 0.5) home_name else away_name

    winner <- ""
    if (is_final && nchar(away_score) > 0 && nchar(home_score) > 0) {
      aw <- suppressWarnings(as.numeric(away_score))
      hw <- suppressWarnings(as.numeric(home_score))
      if (!is.na(aw) && !is.na(hw)) winner <- if (aw > hw) away_name else home_name
    }

    game_time <- tryCatch({
      dt <- as.POSIXct(ev$date %||% "", format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
      format(dt, "%I:%M %p ET")
    }, error = function(e) "")

    data.frame(
      game_date            = date_str,
      away_team            = away_name,
      home_team            = home_name,
      status               = if (is_final) "Final" else "Scheduled",
      winner               = winner,
      favored_team         = favored,
      away_win_probability = round(away_p, 4),
      home_win_probability = round(home_p, 4),
      game_id              = as.character(ev$id %||% ""),
      away_score           = away_score,
      home_score           = home_score,
      away_record          = if (nchar(away_rec) > 0) away_rec else if (!is.null(at)) at$record else "",
      home_record          = if (nchar(home_rec) > 0) home_rec else if (!is.null(ht)) ht$record else "",
      away_win_pct         = if (!is.null(at)) at$win_pct      else NA,
      home_win_pct         = if (!is.null(ht)) ht$win_pct      else NA,
      away_pyth_pct        = if (!is.null(at)) at$pyth_win_pct else NA,
      home_pyth_pct        = if (!is.null(ht)) ht$pyth_win_pct else NA,
      away_run_diff_pg     = if (!is.null(at)) at$run_diff_pg  else NA,
      home_run_diff_pg     = if (!is.null(ht)) ht$run_diff_pg  else NA,
      away_rs_pg           = if (!is.null(at)) at$rs_pg        else NA,
      home_rs_pg           = if (!is.null(ht)) ht$rs_pg        else NA,
      away_ra_pg           = if (!is.null(at)) at$ra_pg        else NA,
      home_ra_pg           = if (!is.null(ht)) ht$ra_pg        else NA,
      away_era             = if (!is.null(at)) at$era          else NA,
      home_era             = if (!is.null(ht)) ht$era          else NA,
      away_probable_starter = away_starter,
      home_probable_starter = home_starter,
      game_time            = game_time,
      stringsAsFactors     = FALSE
    )
  })
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) return(data.frame())
  bind_rows(out)
}

today <- Sys.Date()
all_games <- list()
for (i in seq(-60, 14)) {
  d     <- format(today + i, "%Y-%m-%d")
  games <- tryCatch(parse_day(d), error = function(e) data.frame())
  if (nrow(games) > 0) all_games[[length(all_games) + 1]] <- games
  Sys.sleep(0.25)
}

if (length(all_games) == 0) {
  cat("  No games found. Writing empty CSV.\n")
  write.csv(data.frame(), OUT_CSV, row.names = FALSE)
  quit(save = "no")
}

sched <- bind_rows(all_games)
sched <- sched[!duplicated(paste(sched$game_date, sched$away_team, sched$home_team)), ]
cat(sprintf("  %d total games fetched\n", nrow(sched)))

# ── 4. Fit Logistic Regression ────────────────────────────────────────────
cat("[4/5] Fitting logistic regression model…\n")

finals <- sched[sched$status == "Final" & nchar(sched$winner) > 0 &
                !is.na(sched$away_win_pct) & !is.na(sched$home_win_pct), ]
cat(sprintf("  Training on %d completed games\n", nrow(finals)))

if (nrow(finals) >= 30) {
  train <- finals
  train$home_won   <- as.integer(train$winner == train$home_team)
  train$diff_pyth  <- as.numeric(train$home_pyth_pct)   - as.numeric(train$away_pyth_pct)
  train$diff_win   <- as.numeric(train$home_win_pct)    - as.numeric(train$away_win_pct)
  train$diff_rd    <- as.numeric(train$home_run_diff_pg) - as.numeric(train$away_run_diff_pg)
  train$diff_rs    <- as.numeric(train$home_rs_pg)       - as.numeric(train$away_rs_pg)

  has_era  <- !all(is.na(train$away_era)) && !all(is.na(train$home_era))
  if (has_era) {
    train$diff_era <- as.numeric(train$away_era) - as.numeric(train$home_era)  # lower ERA is better
    train <- train[complete.cases(train[, c("home_won","diff_pyth","diff_win","diff_rd","diff_rs","diff_era")]), ]
    formula_str <- "home_won ~ diff_pyth + diff_win + diff_rd + diff_rs + diff_era"
  } else {
    train <- train[complete.cases(train[, c("home_won","diff_pyth","diff_win","diff_rd","diff_rs")]), ]
    formula_str <- "home_won ~ diff_pyth + diff_win + diff_rd + diff_rs"
  }

  if (nrow(train) >= 30) {
    mdl <- glm(as.formula(formula_str), data = train, family = binomial())
    cat(sprintf("  Null dev: %.1f  Residual: %.1f  AIC: %.1f\n",
                mdl$null.deviance, mdl$deviance, AIC(mdl)))

    sched$diff_pyth <- as.numeric(sched$home_pyth_pct)    - as.numeric(sched$away_pyth_pct)
    sched$diff_win  <- as.numeric(sched$home_win_pct)     - as.numeric(sched$away_win_pct)
    sched$diff_rd   <- as.numeric(sched$home_run_diff_pg) - as.numeric(sched$away_run_diff_pg)
    sched$diff_rs   <- as.numeric(sched$home_rs_pg)       - as.numeric(sched$away_rs_pg)
    if (has_era) sched$diff_era <- as.numeric(sched$away_era) - as.numeric(sched$home_era)

    pred_cols <- if (has_era) c("diff_pyth","diff_win","diff_rd","diff_rs","diff_era") else
                              c("diff_pyth","diff_win","diff_rd","diff_rs")
    valid <- complete.cases(sched[, pred_cols])

    if (any(valid)) {
      hp <- predict(mdl, newdata = sched[valid, ], type = "response")
      sched$home_win_probability[valid] <- round(hp, 4)
      sched$away_win_probability[valid] <- round(1 - hp, 4)
      sched$favored_team[valid] <- ifelse(hp >= 0.5,
                                          sched$home_team[valid],
                                          sched$away_team[valid])
    }
    sched <- sched[, !names(sched) %in% c("diff_pyth","diff_win","diff_rd","diff_rs","diff_era")]
  }
} else {
  cat("  Not enough completed games yet — using formula-based predictions\n")
}

# ── 5. Write CSV ──────────────────────────────────────────────────────────
cat("[5/5] Writing CSV…\n")
dir.create(dirname(OUT_CSV), showWarnings = FALSE, recursive = TRUE)
write.csv(sched, OUT_CSV, row.names = FALSE)

final_ct  <- sum(sched$status == "Final", na.rm = TRUE)
winner_ct <- sum(nchar(sched$winner) > 0, na.rm = TRUE)
today_ct  <- sum(sched$game_date == format(Sys.Date(), "%Y-%m-%d"), na.rm = TRUE)
cat(sprintf("  %d rows | Final: %d | With winner: %d | Today: %d\n",
            nrow(sched), final_ct, winner_ct, today_ct))
cat("=== MLB Done ===\n")
