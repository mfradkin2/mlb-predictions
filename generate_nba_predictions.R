#!/usr/bin/env Rscript
# ============================================================
# NBA Predictions — generate_nba_predictions.R
# Fetches ESPN standings + schedule, fits logistic regression
# on current-season completed games, outputs enriched CSV.
#
# Key variables (research-backed importance):
#   Net Rating  40% weight  — #1 NBA predictor (off - def rating)
#   Win%        25%         — overall team quality
#   PPG         17.5%       — offensive output
#   Opp PPG     17.5%       — defensive quality
#   Rest diff   bonus       — back-to-back games ~3% swing
#
# Home court advantage: ~58% NBA home win rate (~0.28 log-odds)
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
OUT_CSV <- file.path(SCRIPT_DIR, "nba_2026_schedule_enriched.csv")

cat("=== NBA Predictions Generator ===\n")
cat(sprintf("Output: %s\n\n", OUT_CSV))

httr::set_config(httr::config(ssl_verifypeer = FALSE))

# ── 1. Fetch Standings ────────────────────────────────────────────────────
cat("[1/4] Fetching NBA standings…\n")

resp     <- GET("https://site.api.espn.com/apis/v2/sports/basketball/nba/standings")
standings <- content(resp, "parsed")

team_stats <- list()

parse_entries <- function(entries) {
  for (entry in entries %||% list()) {
    team <- entry$team %||% NULL
    if (is.null(team)) next
    id   <- as.character(team$id %||% "")
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

    ppg     <- as.numeric(sv[["Points For/Game"]]     %||% sv[["PPG"]]  %||%
                          sv[["avgPointsFor"]]         %||% 110.0)
    opp_ppg <- as.numeric(sv[["Points Against/Game"]] %||% sv[["OPPG"]] %||%
                          sv[["avgPointsAgainst"]]     %||% 110.0)
    net_rtg  <- ppg - opp_ppg
    # Pace is not always in ESPN standings; approximate from pts
    pace <- as.numeric(sv[["pace"]] %||% sv[["Pace"]] %||% 100.0)

    record <- paste0(w, "-", l)

    # Composite strength
    strength <- 0.40 * pmin(pmax((net_rtg + 15) / 30, 0), 1) +
                0.25 * win_pct +
                0.175 * pmin(ppg / 130, 1) +
                0.175 * (1 - pmin(opp_ppg / 130, 1))

    info <- list(
      id = id, name = name, abbr = abbr,
      record = record, strength = strength, gp = gp,
      win_pct = round(win_pct, 3),
      ppg     = round(ppg,     1),
      opp_ppg = round(opp_ppg, 1),
      net_rtg = round(net_rtg, 1),
      pace    = round(pace,    1)
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

# ── 2. Fetch Schedule ─────────────────────────────────────────────────────
cat("[2/4] Fetching NBA schedule (last 60 days + next 14 days)…\n")

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
HOME_ADV <- 0.28
SCALE    <- 5.5

# Track last game date per team for rest calculation
last_game_date <- list()

parse_day <- function(date_str) {
  dc  <- gsub("-", "", date_str)
  url <- paste0("https://site.api.espn.com/apis/site/v2/sports/basketball/nba/scoreboard?dates=", dc)
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

    at <- lookup(away_name, away_id)
    ht <- lookup(home_name, home_id)

    # Rest advantage (days since last game)
    cur_date   <- as.Date(date_str)
    away_last  <- last_game_date[[away_name]] %||% (cur_date - 3)
    home_last  <- last_game_date[[home_name]] %||% (cur_date - 3)
    away_rest  <- as.integer(cur_date - as.Date(away_last))
    home_rest  <- as.integer(cur_date - as.Date(home_last))
    rest_adv   <- pmin(home_rest, 5) - pmin(away_rest, 5)  # cap at 5 days

    away_str <- if (!is.null(at)) at$strength else 0.5
    home_str <- if (!is.null(ht)) ht$strength else 0.5
    home_p   <- logistic(SCALE * (home_str - away_str) + HOME_ADV + 0.06 * rest_adv)
    away_p   <- 1 - home_p
    favored  <- if (home_p >= 0.5) home_name else away_name

    winner <- ""
    if (is_final && nchar(away_score) > 0 && nchar(home_score) > 0) {
      aw <- suppressWarnings(as.numeric(away_score))
      hw <- suppressWarnings(as.numeric(home_score))
      if (!is.na(aw) && !is.na(hw)) winner <- if (aw > hw) away_name else home_name
    }

    if (is_final) {
      last_game_date[[away_name]] <<- date_str
      last_game_date[[home_name]] <<- date_str
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
      away_record          = if (nchar(away_rec) > 0) away_rec else if (!is.null(at)) paste0(round(at$win_pct*100),"% win") else "",
      home_record          = if (nchar(home_rec) > 0) home_rec else if (!is.null(ht)) paste0(round(ht$win_pct*100),"% win") else "",
      away_win_pct         = if (!is.null(at)) at$win_pct  else NA,
      home_win_pct         = if (!is.null(ht)) ht$win_pct  else NA,
      away_ppg             = if (!is.null(at)) at$ppg      else NA,
      home_ppg             = if (!is.null(ht)) ht$ppg      else NA,
      away_opp_ppg         = if (!is.null(at)) at$opp_ppg  else NA,
      home_opp_ppg         = if (!is.null(ht)) ht$opp_ppg  else NA,
      away_net_rtg         = if (!is.null(at)) at$net_rtg  else NA,
      home_net_rtg         = if (!is.null(ht)) ht$net_rtg  else NA,
      away_pace            = if (!is.null(at)) at$pace     else NA,
      home_pace            = if (!is.null(ht)) ht$pace     else NA,
      rest_advantage       = rest_adv,
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

# ── 3. Fit Logistic Regression ────────────────────────────────────────────
cat("[3/4] Fitting logistic regression model…\n")

finals <- sched[sched$status == "Final" & nchar(sched$winner) > 0 &
                !is.na(sched$away_win_pct) & !is.na(sched$home_win_pct), ]
cat(sprintf("  Training on %d completed games\n", nrow(finals)))

if (nrow(finals) >= 30) {
  train <- finals
  train$home_won    <- as.integer(train$winner == train$home_team)
  train$diff_win    <- as.numeric(train$home_win_pct)  - as.numeric(train$away_win_pct)
  train$diff_net    <- as.numeric(train$home_net_rtg)  - as.numeric(train$away_net_rtg)
  train$diff_ppg    <- as.numeric(train$home_ppg)      - as.numeric(train$away_ppg)
  train$rest_adv    <- as.numeric(train$rest_advantage)
  train <- train[complete.cases(train[, c("home_won","diff_win","diff_net","diff_ppg","rest_adv")]), ]

  if (nrow(train) >= 30) {
    mdl <- glm(home_won ~ diff_win + diff_net + diff_ppg + rest_adv,
               data = train, family = binomial())
    cat(sprintf("  Null dev: %.1f  Residual: %.1f  AIC: %.1f\n",
                mdl$null.deviance, mdl$deviance, AIC(mdl)))

    sched$diff_win <- as.numeric(sched$home_win_pct) - as.numeric(sched$away_win_pct)
    sched$diff_net <- as.numeric(sched$home_net_rtg) - as.numeric(sched$away_net_rtg)
    sched$diff_ppg <- as.numeric(sched$home_ppg)     - as.numeric(sched$away_ppg)
    sched$rest_adv <- as.numeric(sched$rest_advantage)
    valid <- complete.cases(sched[, c("diff_win","diff_net","diff_ppg","rest_adv")])

    if (any(valid)) {
      hp <- predict(mdl, newdata = sched[valid, ], type = "response")
      sched$home_win_probability[valid] <- round(hp, 4)
      sched$away_win_probability[valid] <- round(1 - hp, 4)
      sched$favored_team[valid] <- ifelse(hp >= 0.5,
                                          sched$home_team[valid],
                                          sched$away_team[valid])
    }
    sched <- sched[, !names(sched) %in% c("diff_win","diff_net","diff_ppg","rest_adv")]
  }
} else {
  cat("  Not enough completed games yet — using formula-based predictions\n")
}

# ── 4. Write CSV ──────────────────────────────────────────────────────────
cat("[4/4] Writing CSV…\n")
dir.create(dirname(OUT_CSV), showWarnings = FALSE, recursive = TRUE)
write.csv(sched, OUT_CSV, row.names = FALSE)

final_ct  <- sum(sched$status == "Final", na.rm = TRUE)
winner_ct <- sum(nchar(sched$winner) > 0, na.rm = TRUE)
today_ct  <- sum(sched$game_date == format(Sys.Date(), "%Y-%m-%d"), na.rm = TRUE)
cat(sprintf("  %d rows | Final: %d | With winner: %d | Today: %d\n",
            nrow(sched), final_ct, winner_ct, today_ct))
cat("=== NBA Done ===\n")
