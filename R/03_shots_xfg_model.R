# 03_shots_xfg_model.R
# Expected field-goal (xFG) model: probability a shot goes in given only WHERE and
# HOW it was taken -- never WHO took it (shooter identity would absorb the skill
# the index is trying to measure).
#
# Model: gradient-boosted trees (xgboost) on ~6.4M shots, 1996-97 -> 2025-26.
#   train 1996-97..2023-24 | early-stopping validation 2024-25 | test 2025-26
# Baseline: previous-season make rate by shot zone (zone basic x area x range).
# Final xFG for every shot is CROSS-FITTED by game: 5 folds of game IDs, each
# shot predicted by a model that never saw its game, so in-sample overfitting
# cannot leak a player's makes into his own expectation.
# Outputs: data/interim/shots_xfg.parquet, data/final/xfg_eval.csv, figures/xfg_*.png

source("R/utils.R")
suppressPackageStartupMessages({ library(arrow); library(xgboost) })

# ---- Features ---------------------------------------------------------------------------
# Converts the shot table into a numeric feature matrix. Categorical fields use
# fixed level orders so every fold/season gets identical encodings.
ZONES_BASIC <- c("Restricted Area", "In The Paint (Non-RA)", "Mid-Range", "Left Corner 3",
                 "Right Corner 3", "Above the Break 3", "Backcourt")
ZONES_AREA  <- c("Center(C)", "Left Side(L)", "Right Side(R)", "Left Side Center(LC)",
                 "Right Side Center(RC)", "Back Court(BC)")
FAMILIES    <- c("jumper", "layup", "dunk", "hook", "tip")

build_features <- function(s) {
  X <- s[, .(
    loc_x = LOC_X, loc_y = LOC_Y,
    dist = SHOT_DISTANCE,
    angle = atan2(abs(LOC_X), pmax(LOC_Y, -50)) * 180 / pi,
    at_origin = as.integer(LOC_X == 0 & LOC_Y == 0),     # pre-2011 rim-shot coding artefact
    zone_basic = match(SHOT_ZONE_BASIC, ZONES_BASIC),
    zone_area  = match(SHOT_ZONE_AREA, ZONES_AREA),
    family = match(family, FAMILIES),
    moving = as.integer(moving),
    is3 = as.integer(is3),
    period = pmin(period, 5L),
    clock_sec = clock_sec,
    season_year = season_year(season),
    playoffs = as.integer(season_type == "PO"))]
  as.matrix(X)
}

load_model_shots <- function() {
  s <- as.data.table(read_parquet(file.path(paths$interim, "shots.parquet")))
  s[heave == FALSE & in_chart == TRUE & personId > 0]
}

XGB_PARAMS <- list(objective = "binary:logistic", eval_metric = "logloss", eta = 0.1,
                   max_depth = 7, min_child_weight = 50, subsample = 0.8,
                   colsample_bytree = 0.8, tree_method = "hist", nthread = parallel::detectCores())

logloss <- function(y, p) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
brier   <- function(y, p) mean((p - y)^2)

# ---- 1. Temporal evaluation --------------------------------------------------------------------
evaluate_xfg <- function(s) {
  yr <- season_year(s$season)
  tr <- s[yr <= 2023]; va <- s[yr == 2024]; te <- s[yr == 2025]
  dtr <- xgb.DMatrix(build_features(tr), label = as.integer(tr$made))
  dva <- xgb.DMatrix(build_features(va), label = as.integer(va$made))
  fit <- xgb.train(XGB_PARAMS, dtr, nrounds = 2000, evals = list(valid = dva),
                   early_stopping_rounds = 30, verbose = 0)
  best <- xgb.attr(fit, "best_iteration")
  best <- if (is.null(best)) 2000L else as.integer(best) + 1L
  te[, p_xgb := predict(fit, xgb.DMatrix(build_features(te)), iterationrange = c(1, best))]

  # Baselines: league-average make rate (constant) and prior-season zone averages
  zone_avg <- va[, .(p_zone = mean(made)), .(SHOT_ZONE_BASIC, SHOT_ZONE_AREA, SHOT_ZONE_RANGE)]
  te[zone_avg, on = .(SHOT_ZONE_BASIC, SHOT_ZONE_AREA, SHOT_ZONE_RANGE), p_zone := i.p_zone]
  te[is.na(p_zone), p_zone := mean(va$made)]
  te[, p_const := mean(va$made)]

  ev <- rbindlist(lapply(c(const = "p_const", zone = "p_zone", xgb = "p_xgb"), function(col)
    data.table(model = col, logloss = logloss(te$made, te[[col]]), brier = brier(te$made, te[[col]]),
               mean_pred = mean(te[[col]]), actual = mean(te$made))))
  ev[, model := c("League average (2024-25)", "Zone average (2024-25)", "xgboost xFG")]
  list(fit = fit, best = best, test = te, eval = ev)
}

# Calibration plot: predicted vs actual make rate by 20 quantile bins, test season
plot_calibration <- function(te) {
  cal <- te[, .(pred = mean(p_xgb), actual = mean(made), n = .N), .(bin = cut(p_xgb, unique(quantile(p_xgb, 0:20 / 20)), include.lowest = TRUE))]
  ggplot(cal, aes(pred, actual)) +
    geom_abline(color = pal$axis, linetype = "dashed") +
    geom_line(color = pal$blue, linewidth = 0.7) +
    geom_point(color = pal$blue, size = 2.6) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(title = "xFG model is well calibrated on unseen data",
         subtitle = "2025-26 held-out shots, binned by predicted make probability (20 equal-size bins)",
         x = "Predicted make probability", y = "Actual make rate", caption = CAPTION) +
    theme_ehi()
}

# ---- 2. Cross-fitted predictions for every shot ------------------------------------------------
crossfit_xfg <- function(s, nrounds, k = 5, seed = 2026) {
  set.seed(seed)
  games <- unique(s$gameId)
  fold_of <- setNames(sample(rep_len(1:k, length(games))), games)
  s[, fold := fold_of[as.character(gameId)]]
  s[, xfg := NA_real_]
  for (f in 1:k) {
    message("  cross-fit fold ", f, "/", k)
    tr <- s$fold != f
    fit <- xgb.train(XGB_PARAMS, xgb.DMatrix(build_features(s[tr]), label = as.integer(s$made[tr])),
                     nrounds = nrounds, verbose = 0)
    s[!tr, xfg := predict(fit, xgb.DMatrix(build_features(s[!tr])))]
  }
  s[, xpts := xfg * fifelse(is3, 3, 2)]
  s[, .(gameId, actionNumber, personId, teamId, season, season_type, made, is3, family,
        SHOT_ZONE_BASIC, and1, assisted, xfg, xpts)]
}

if (sys.nframe() == 0) {
  s <- load_model_shots()
  message("shots for model: ", nrow(s))
  r <- evaluate_xfg(s)
  print(r$eval)
  fwrite(r$eval, file.path(paths$final, "xfg_eval.csv"))
  save_fig(plot_calibration(r$test), "xfg_calibration", width = 7, height = 6.5)
  imp <- xgb.importance(model = r$fit)
  fwrite(imp, "data/audit/xfg_importance.csv"); print(imp)
  out <- crossfit_xfg(s, nrounds = r$best)
  write_parquet(out, file.path(paths$interim, "shots_xfg.parquet"))
  fwrite(data.table(n_shots = nrow(s), n_train = sum(season_year(s$season) <= 2023)), file.path(paths$final, "xfg_meta.csv"))
}
