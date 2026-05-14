#!/usr/bin/env Rscript
# ============================================================
# NFL Predictions — generate_nfl_predictions.R
# Fetches ESPN standings + schedule, fits logistic regression
# on current-season completed games, outputs enriched CSV.
#
# Key variables (research-backed importance):
#   Pt Diff/G   35% weight  — Pythagorean wins proxy, best NFL predictor
#   Win%        20%         — overall team quality
#   YPG Off     15%         — offensive efficiency
#   YPG Def     15%         — defensive efficiency
#   TO Margin   15%         — turnover differential
#
# Home field advantage: ~57% NFL home win rate (~0.40 log-odds)
# NFL is off-season in April–August; script handles gracefully.
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
OUT_CSV <- file.path(SCRIPT_DIR, "nfl_2026_schedule_enriched.csv")

cat("=== NFL Predictions Generator ===\n")
cat(sprintf("Output: %s\n\n", OUT_CSV))

httr::set_config(httr::config(ssl_verifypeer = FALSE))

# ── 1. Fetch Standings ────────────────────────────────────────────────────
cat("[1/4] Fetching NFL standings…\n")

resp      <- GET("https://site.api.espn.com/apis/v2/sports/football/nfl/standings")
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
    t  <- as.numeric(sv[["ties"]]   %||% sv[["T"]] %||% 0)
    gp <- w + l + t
    win_pct <- if (gp > 0) (w + 0.5 * t) / gp else 0.5

    pf  <- as.numeric(sv[["Points For"]]     %||% sv[["PF"]] %||% 0)
    pa  <- as.numeric(sv[["Points Against"]] %||% sv[["PA"]] %||% 0)
    pt_diff_pg <- if (gp > 0) (pf - pa) / gp else 0

    ypg_off <- as.numeric(sv[["Total Yards/Game"]]       %||%
                          sv[["Offensive Yards/Game"]]   %||%
                          sv[["avgYardsPerGame"]]        %||% 340.0)
    ypg_def <- as.numeric(sv[["Opponent Yards/Game"]]    %||%
                          sv[["Defensive Yards/Game"]  ] %||%
                          sv[["avgOppYardsPerGame"]]     %||% 340.0)
    to_for   <- as.numeric(sv[["Takeaways"]]  %||% sv[["TO"]]   %||% 0)
    to_against <- as.numeric(sv[["Giveaways"]] %||% sv[["TFL"]] %||% 0)
    to_margin  <- if (gp > 0) (to_for - to_against) / gp else 0

    record <- if (t > 0) paste0(w,"-",l,"-",t) else paste0(w,"-",l)

    # Composite strength
    strength <- 0.35 * pmin(pmax((pt_diff_pg + 20) / 40, 0), 1) +
                0.20 * win_pct +
                0.15 * pmin(ypg_off / 450, 1) +
                0.15 * (1 - pmin(ypg_def / 450, 1)) +
                0.15 * pmin(pmax((to_margin + 2) / 4, 0), 1)

    info <- list(
      id = id, name = name, abbr = abbr,
      record = record, strength = strength, gp = gp,
      win_pct    = round(win_pct,    3),
      pt_diff_pg = round(pt_diff_pg, 1),
      ypg_off    = round(ypg_off,    1),
      ypg_def    = round(ypg_def,    1),
      to_margin  = round(to_margin,  2)
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
cat("[2/4] Fetching NFL schedule…\n")

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
HOME_ADV <- 0.40
SCALE    <- 4.5

parse_day <- function(date_str) {
  dc  <- gsub("-", "", date_str)
  url <- paste0("https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard?dates=", dc)
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
      away_win_pct         = if (!is.null(at)) at$win_pct    else NA,
      home_win_pct         = if (!is.null(ht)) ht$win_pct    else NA,
      away_pt_diff_pg      = if (!is.null(at)) at$pt_diff_pg else NA,
      home_pt_diff_pg      = if (!is.null(ht)) ht$pt_diff_pg else NA,
      away_ypg_off         = if (!is.null(at)) at$ypg_off    else NA,
      home_ypg_off         = if (!is.null(ht)) ht$ypg_off    else NA,
      away_ypg_def         = if (!is.null(at)) at$ypg_def    else NA,
      home_ypg_def         = if (!is.null(ht)) ht$ypg_def    else NA,
      away_to_margin       = if (!is.null(at)) at$to_margin  else NA,
      home_to_margin       = if (!is.null(ht)) ht$to_margin  else NA,
      game_time            = game_time,
      stringsAsFactors     = FALSE
    )
  })
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) return(data.frame())
  bind_rows(out)
}

# NFL: try current season window (Sep–Feb) + upcoming weeks
today <- Sys.Date()
all_games <- list()

# Try last 180 days for completed season games + next 30 days for upcoming
for (i in seq(-180, 30)) {
  d     <- format(today + i, "%Y-%m-%d")
  games <- tryCatch(parse_day(d), error = function(e) data.frame())
  if (nrow(games) > 0) all_games[[length(all_games) + 1]] <- games
  Sys.sleep(0.20)
}

if (length(all_games) == 0) {
  cat("  NFL off-season or no games found. Writing placeholder CSV.\n")
  # Create a minimal placeholder so dashboard can display off-season message
  placeholder <- data.frame(
    game_date = character(), away_team = character(), home_team = character(),
    status = character(), winner = character(), favored_team = character(),
    away_win_probability = numeric(), home_win_probability = numeric(),
    game_id = character(), away_score = character(), home_score = character(),
    away_record = character(), home_record = character(),
    away_win_pct = numeric(), home_win_pct = numeric(),
    away_pt_diff_pg = numeric(), home_pt_diff_pg = numeric(),
    away_ypg_off = numeric(), home_ypg_off = numeric(),
    away_ypg_def = numeric(), home_ypg_def = numeric(),
    away_to_margin = numeric(), home_to_margin = numeric(),
    game_time = character(), stringsAsFactors = FALSE
  )
  write.csv(placeholder, OUT_CSV, row.names = FALSE)
  cat(sprintf("  Saved empty schedule to: %s\n", OUT_CSV))
  cat("=== NFL Done (off-season) ===\n")
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

if (nrow(finals) >= 20) {
  train <- finals
  train$home_won   <- as.integer(train$winner == train$home_team)
  train$diff_win   <- as.numeric(train$home_win_pct)    - as.numeric(train$away_win_pct)
  train$diff_pt    <- as.numeric(train$home_pt_diff_pg) - as.numeric(train$away_pt_diff_pg)
  train$diff_yoff  <- as.numeric(train$home_ypg_off)    - as.numeric(train$away_ypg_off)
  train$diff_ydef  <- as.numeric(train$away_ypg_def)    - as.numeric(train$home_ypg_def)
  train$diff_to    <- as.numeric(train$home_to_margin)  - as.numeric(train$away_to_margin)
  train <- train[complete.cases(train[, c("home_won","diff_win","diff_pt","diff_yoff","diff_ydef","diff_to")]), ]

  if (nrow(train) >= 20) {
    mdl <- glm(home_won ~ diff_win + diff_pt + diff_yoff + diff_ydef + diff_to,
               data = train, family = binomial())
    cat(sprintf("  Null dev: %.1f  Residual: %.1f  AIC: %.1f\n",
                mdl$null.deviance, mdl$deviance, AIC(mdl)))

    sched$diff_win  <- as.numeric(sched$home_win_pct)    - as.numeric(sched$away_win_pct)
    sched$diff_pt   <- as.numeric(sched$home_pt_diff_pg) - as.numeric(sched$away_pt_diff_pg)
    sched$diff_yoff <- as.numeric(sched$home_ypg_off)    - as.numeric(sched$away_ypg_off)
    sched$diff_ydef <- as.numeric(sched$away_ypg_def)    - as.numeric(sched$home_ypg_def)
    sched$diff_to   <- as.numeric(sched$home_to_margin)  - as.numeric(sched$away_to_margin)
    valid <- complete.cases(sched[, c("diff_win","diff_pt","diff_yoff","diff_ydef","diff_to")])

    if (any(valid)) {
      hp <- predict(mdl, newdata = sched[valid, ], type = "response")
      sched$home_win_probability[valid] <- round(hp, 4)
      sched$away_win_probability[valid] <- round(1 - hp, 4)
      sched$favored_team[valid] <- ifelse(hp >= 0.5,
                                          sched$home_team[valid],
                                          sched$away_team[valid])
    }
    sched <- sched[, !names(sched) %in% c("diff_win","diff_pt","diff_yoff","diff_ydef","diff_to")]
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
cat("=== NFL Done ===\n")
