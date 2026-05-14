#!/usr/bin/env Rscript
# ============================================================
# NHL Predictions — generate_nhl_predictions.R
# Fetches ESPN standings + schedule, fits logistic regression
# on current-season completed games, outputs enriched CSV.
#
# Key variables (research-backed importance):
#   Points%   35% weight  — best overall NHL team strength metric
#   GF/G      20%         — offensive firepower
#   GA/G      20%         — defensive soundness
#   PP%       12.5%       — power play efficiency
#   PK%       12.5%       — penalty kill efficiency
#   Save%     used in model training when available
#
# Home ice advantage: ~55% NHL home win rate (~0.30 log-odds)
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
OUT_CSV <- file.path(SCRIPT_DIR, "nhl_2026_schedule_enriched.csv")

cat("=== NHL Predictions Generator ===\n")
cat(sprintf("Output: %s\n\n", OUT_CSV))

httr::set_config(httr::config(ssl_verifypeer = FALSE))

# ── 1. Fetch Standings ────────────────────────────────────────────────────
cat("[1/4] Fetching NHL standings…\n")

resp <- GET("https://site.api.espn.com/apis/v2/sports/hockey/nhl/standings")
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
      sv[[s$name %||% ""]]  <- s$value
      if (!is.null(s$abbreviation)) sv[[s$abbreviation]] <- s$value
    }

    w   <- as.numeric(sv[["wins"]]   %||% sv[["W"]]  %||% 0)
    l   <- as.numeric(sv[["losses"]] %||% sv[["L"]]  %||% 0)
    otl <- as.numeric(sv[["otl"]]    %||% sv[["OTL"]] %||%
                      sv[["Overtime Losses"]] %||% 0)
    gp  <- w + l + otl
    pts <- as.numeric(sv[["points"]] %||% sv[["PTS"]] %||% (w * 2 + otl))
    max_pts  <- max(gp * 2, 1)
    pts_pct  <- pts / max_pts

    gf     <- as.numeric(sv[["Goals For"]]     %||% sv[["GF"]] %||% 0)
    ga     <- as.numeric(sv[["Goals Against"]] %||% sv[["GA"]] %||% 0)
    gf_pg  <- if (gp > 0) gf / gp else 3.0
    ga_pg  <- if (gp > 0) ga / gp else 3.0

    pp_pct   <- as.numeric(sv[["Power Play %"]]  %||% sv[["PP%"]]  %||% 20.0)
    pk_pct   <- as.numeric(sv[["Penalty Kill %"]]%||% sv[["PK%"]]  %||% 80.0)
    save_pct <- as.numeric(sv[["Save %"]]        %||% sv[["SV%"]]  %||% 0.910)

    record <- if (otl > 0) paste0(w,"-",l,"-",otl) else paste0(w,"-",l)

    # Composite strength (research-based weights)
    strength <- 0.35 * pts_pct +
                0.20 * pmin(gf_pg / 4.5, 1) +
                0.20 * (1 - pmin(ga_pg / 4.5, 1)) +
                0.125 * pmin(pp_pct / 30, 1) +
                0.125 * pmin(pk_pct / 100, 1)

    info <- list(
      id = id, name = name, abbr = abbr,
      record = record, strength = strength, gp = gp,
      pts_pct  = round(pts_pct * 100, 1),
      gf_pg    = round(gf_pg,  2),
      ga_pg    = round(ga_pg,  2),
      pp_pct   = round(pp_pct, 1),
      pk_pct   = round(pk_pct, 1),
      save_pct = round(save_pct, 3)
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
cat("[2/4] Fetching NHL schedule (last 60 days + next 14 days)…\n")

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
HOME_ADV <- 0.30   # ~55% home win rate in NHL
SCALE    <- 5.0

parse_day <- function(date_str) {
  dc  <- gsub("-", "", date_str)
  url <- paste0("https://site.api.espn.com/apis/site/v2/sports/hockey/nhl/scoreboard?dates=", dc)
  resp <- tryCatch(GET(url, timeout(15)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(data.frame())
  data <- content(resp, "parsed")

  out <- lapply(data$events %||% list(), function(ev) {
    comp  <- (ev$competitions %||% list(list()))[[1]]
    comps <- comp$competitors %||% list()
    away_c <- Filter(function(c) identical(c$homeAway, "away"), comps)[[1]] %||% list()
    home_c <- Filter(function(c) identical(c$homeAway, "home"), comps)[[1]] %||% list()

    stype      <- comp$status$type$name %||% ""
    is_final   <- stype == "STATUS_FINAL" || isTRUE(comp$status$type$completed)
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
      away_pts_pct         = if (!is.null(at)) at$pts_pct  else NA,
      home_pts_pct         = if (!is.null(ht)) ht$pts_pct  else NA,
      away_gf_pg           = if (!is.null(at)) at$gf_pg    else NA,
      home_gf_pg           = if (!is.null(ht)) ht$gf_pg    else NA,
      away_ga_pg           = if (!is.null(at)) at$ga_pg    else NA,
      home_ga_pg           = if (!is.null(ht)) ht$ga_pg    else NA,
      away_pp_pct          = if (!is.null(at)) at$pp_pct   else NA,
      home_pp_pct          = if (!is.null(ht)) ht$pp_pct   else NA,
      away_pk_pct          = if (!is.null(at)) at$pk_pct   else NA,
      home_pk_pct          = if (!is.null(ht)) ht$pk_pct   else NA,
      away_save_pct        = if (!is.null(at)) at$save_pct else NA,
      home_save_pct        = if (!is.null(ht)) ht$save_pct else NA,
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

# ── 3. Fit Logistic Regression on Completed Games ─────────────────────────
cat("[3/4] Fitting logistic regression model…\n")

finals <- sched[sched$status == "Final" & nchar(sched$winner) > 0 &
                !is.na(sched$away_pts_pct) & !is.na(sched$home_pts_pct), ]
cat(sprintf("  Training on %d completed games\n", nrow(finals)))

if (nrow(finals) >= 30) {
  train <- finals
  train$home_won    <- as.integer(train$winner == train$home_team)
  train$diff_pts    <- as.numeric(train$home_pts_pct) - as.numeric(train$away_pts_pct)
  train$diff_gf     <- as.numeric(train$home_gf_pg)   - as.numeric(train$away_gf_pg)
  train$diff_ga     <- as.numeric(train$away_ga_pg)   - as.numeric(train$home_ga_pg)
  train$diff_pp     <- as.numeric(train$home_pp_pct)  - as.numeric(train$away_pp_pct)
  train$diff_pk     <- as.numeric(train$home_pk_pct)  - as.numeric(train$away_pk_pct)
  train <- train[complete.cases(train[, c("home_won","diff_pts","diff_gf","diff_ga","diff_pp","diff_pk")]), ]

  if (nrow(train) >= 30) {
    mdl <- glm(home_won ~ diff_pts + diff_gf + diff_ga + diff_pp + diff_pk,
               data = train, family = binomial())
    cat(sprintf("  Null dev: %.1f  Residual: %.1f  AIC: %.1f\n",
                mdl$null.deviance, mdl$deviance, AIC(mdl)))

    sched$diff_pts <- as.numeric(sched$home_pts_pct) - as.numeric(sched$away_pts_pct)
    sched$diff_gf  <- as.numeric(sched$home_gf_pg)   - as.numeric(sched$away_gf_pg)
    sched$diff_ga  <- as.numeric(sched$away_ga_pg)   - as.numeric(sched$home_ga_pg)
    sched$diff_pp  <- as.numeric(sched$home_pp_pct)  - as.numeric(sched$away_pp_pct)
    sched$diff_pk  <- as.numeric(sched$home_pk_pct)  - as.numeric(sched$away_pk_pct)
    valid <- complete.cases(sched[, c("diff_pts","diff_gf","diff_ga","diff_pp","diff_pk")])

    if (any(valid)) {
      hp <- predict(mdl, newdata = sched[valid, ], type = "response")
      sched$home_win_probability[valid] <- round(hp, 4)
      sched$away_win_probability[valid] <- round(1 - hp, 4)
      sched$favored_team[valid] <- ifelse(hp >= 0.5,
                                          sched$home_team[valid],
                                          sched$away_team[valid])
    }
    sched <- sched[, !names(sched) %in% c("diff_pts","diff_gf","diff_ga","diff_pp","diff_pk")]
  }
} else {
  cat("  Not enough completed games yet — using formula-based predictions\n")
}

# ── 4. Write CSV ──────────────────────────────────────────────────────────
cat("[4/4] Writing CSV…\n")
dir.create(dirname(OUT_CSV), showWarnings = FALSE, recursive = TRUE)
write.csv(sched, OUT_CSV, row.names = FALSE)

final_ct  <- sum(sched$status == "Final",  na.rm = TRUE)
winner_ct <- sum(nchar(sched$winner) > 0,  na.rm = TRUE)
today_ct  <- sum(sched$game_date == format(Sys.Date(), "%Y-%m-%d"), na.rm = TRUE)
cat(sprintf("  %d rows | Final: %d | With winner: %d | Today: %d\n",
            nrow(sched), final_ct, winner_ct, today_ct))
cat("=== NHL Done ===\n")
