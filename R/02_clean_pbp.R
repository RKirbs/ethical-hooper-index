# 02_clean_pbp.R
# Turns the raw bulk play-by-play (PBP V3) + shot-detail CSVs into three clean
# Parquet tables in data/interim/:
#   shots.parquet      one row per field-goal attempt, with location, harmonised
#                      shot family, assisted flag and and-one flag (feeds xFG)
#   ft_trips.parquet   one row per free-throw trip, classified (and-one, 3-shot,
#                      2-shot shooting, penalty, technical/flagrant/other)
#   player_team_season.parquet  counting stats per player x team x season x type
# Why: every downstream metric is a ratio of counts from these tables, so the
# event logic (how a foul drawn / and-one / unassisted make is detected) lives
# in exactly one place.

source("R/utils.R")
suppressPackageStartupMessages(library(arrow))

# ---- Parsing helpers -----------------------------------------------------------------

# "PT11M47.00S" (V3) or "11:47" (V2) -> seconds remaining in the period
clock_to_sec <- function(x) {
  v3 <- grepl("^PT", x)
  out <- numeric(length(x))
  m <- regmatches(x[v3], regexec("PT(\\d+)M([0-9.]+)S", x[v3]))
  out[v3] <- vapply(m, function(z) as.numeric(z[2]) * 60 + as.numeric(z[3]), numeric(1))
  p <- strsplit(x[!v3], ":", fixed = TRUE)
  out[!v3] <- vapply(p, function(z) as.numeric(z[1]) * 60 + as.numeric(z[2]), numeric(1))
  out
}

# Collapse ~70 era-dependent ACTION_TYPE labels into 5 families that exist in
# every season (Phase 0 decision #3). Order matters: a "Putback Dunk" is a dunk.
shot_family <- function(action) {
  fcase(grepl("Dunk", action), "dunk",
        grepl("Tip|Putback|Follow Up", action), "tip",
        grepl("Hook", action), "hook",
        grepl("Layup|Finger Roll|Alley Oop", action), "layup",
        default = "jumper")
}
# "Driving"/"Running" modifiers are the only motion tags present since 1996-97
shot_moving <- function(action) grepl("Driving|Running", action)

# ---- 2012-13 patch: convert V2 rows for the 16 games missing from V3 --------------------
# The (EVENTMSGTYPE, EVENTMSGACTIONTYPE) -> (actionType, subType) mapping is learned
# from the 1,213 games present in both formats, not typed by hand.
patch_2012 <- function(v3) {
  v2 <- fread(file.path(paths$bulk, "nbastats_2012.csv"), showProgress = FALSE)
  v3_full <- fread(file.path(paths$bulk, "nbastatsv3_2012.csv"), showProgress = FALSE,
                   select = c("gameId", "actionNumber", "actionType", "subType"))
  map <- merge(v2[, .(gameId = GAME_ID, actionNumber = EVENTNUM, EVENTMSGTYPE, EVENTMSGACTIONTYPE)],
               v3_full, by = c("gameId", "actionNumber"))[
    , .N, .(EVENTMSGTYPE, EVENTMSGACTIONTYPE, actionType, subType)][
    order(-N)][, .SD[1], .(EVENTMSGTYPE, EVENTMSGACTIONTYPE)]
  fwrite(map, "data/audit/v2_to_v3_event_map_2012.csv")
  miss <- setdiff(unique(v2$GAME_ID), unique(v3$gameId))
  add <- v2[GAME_ID %in% miss]
  add <- merge(add, map[, .(EVENTMSGTYPE, EVENTMSGACTIONTYPE, actionType, subType)],
               by = c("EVENTMSGTYPE", "EVENTMSGACTIONTYPE"), all.x = TRUE)
  # V2 shot rows: type 1 = made, 2 = missed (subType comes from the shot chart later)
  add[EVENTMSGTYPE == 1, actionType := "Made Shot"]
  add[EVENTMSGTYPE == 2, actionType := "Missed Shot"]
  desc <- fcoalesce(na_if_empty(add$HOMEDESCRIPTION), na_if_empty(add$VISITORDESCRIPTION),
                    na_if_empty(add$NEUTRALDESCRIPTION))
  out <- add[, .(gameId = GAME_ID, actionNumber = EVENTNUM, clock = PCTIMESTRING, period = PERIOD,
                 teamId = PLAYER1_TEAM_ID, personId = PLAYER1_ID, playerName = PLAYER1_NAME,
                 actionType = fcoalesce(actionType, ""), subType = fcoalesce(subType, ""),
                 description = fcoalesce(desc, ""))]
  message("  2012-13 patch: added ", length(miss), " games, ", nrow(out), " events")
  rbind(v3, out, fill = TRUE)
}
na_if_empty <- function(x) { x[!nzchar(x)] <- NA_character_; x }

# ---- Per-season processing ---------------------------------------------------------------

V3_COLS <- c("gameId", "actionNumber", "clock", "period", "teamId", "personId", "playerName",
             "actionType", "subType", "description")
SHOT_COLS <- c("GAME_ID", "GAME_EVENT_ID", "PLAYER_ID", "TEAM_ID", "ACTION_TYPE", "SHOT_TYPE",
               "SHOT_ZONE_BASIC", "SHOT_ZONE_AREA", "SHOT_ZONE_RANGE", "SHOT_DISTANCE", "LOC_X", "LOC_Y")

process_season <- function(y, po = FALSE) {
  tag <- if (po) sprintf("po_%d", y) else as.character(y)
  stype <- if (po) "PO" else "RS"
  ev <- fread(file.path(paths$bulk, sprintf("nbastatsv3_%s.csv", tag)), select = V3_COLS, showProgress = FALSE)
  if (y == 2012 && !po) ev <- patch_2012(ev)
  ev[, clock_sec := clock_to_sec(clock)]
  setorder(ev, gameId, actionNumber)
  sc <- fread(file.path(paths$bulk, sprintf("shotdetail_%s.csv", tag)), select = SHOT_COLS, showProgress = FALSE)

  # -- Shots: PBP FG rows joined to the shot chart --------------------------------------------
  shots <- ev[actionType %in% c("Made Shot", "Missed Shot"),
              .(gameId, actionNumber, period, clock_sec, teamId, personId, playerName,
                made = actionType == "Made Shot", description)]
  shots <- merge(shots, sc, by.x = c("gameId", "actionNumber"), by.y = c("GAME_ID", "GAME_EVENT_ID"), all.x = TRUE)
  shots[, `:=`(
    is3      = fifelse(!is.na(SHOT_TYPE), SHOT_TYPE == "3PT Field Goal", grepl("3PT", description, fixed = TRUE)),
    assisted = made & grepl("AST)", description, fixed = TRUE),
    family   = shot_family(fcoalesce(ACTION_TYPE, "Jump Shot")),
    moving   = shot_moving(fcoalesce(ACTION_TYPE, "")),
    in_chart = !is.na(SHOT_TYPE)
  )]
  # Heaves (>= 36 ft, <= 3 s left) are excluded everywhere; 2025-26 began logging
  # missed heaves as team attempts, so this keeps all seasons on one definition.
  shots[, heave := fcoalesce(SHOT_DISTANCE, 0L) >= 36 & clock_sec <= 3]

  # -- Free-throw trips -----------------------------------------------------------------------
  ft <- ev[actionType == "Free Throw"]
  ft[, ft_made := !grepl("^MISS", description)]
  ft[, `:=`(n_of = suppressWarnings(as.integer(sub(".* of (\\d)$", "\\1", subType))),
            k_of = suppressWarnings(as.integer(sub(".*(\\d) of \\d$", "\\1", subType))))]
  ft[, kind := fcase(grepl("Technical", subType), "technical",
                     grepl("Flagrant|Clear Path", subType), "flagrant_clearpath",
                     default = "regular")]
  # a trip = the "1 of n" row; FTA/FTM summed over all rows of that shooter at that stoppage
  trips <- ft[kind != "technical" & k_of == 1 & !is.na(n_of),
              .(gameId, actionNumber, period, clock_sec, teamId, personId, n_of, kind)]
  trip_ftm <- ft[kind != "technical", .(trip_fta = .N, trip_ftm = sum(ft_made)), .(gameId, period, clock_sec, personId)]
  trips <- merge(trips, trip_ftm, by = c("gameId", "period", "clock_sec", "personId"), all.x = TRUE)

  # and-one: a "1 of 1" trip whose shooter made a FG at the same game clock
  made_keys <- shots[made == TRUE, .(gameId, period, clock_sec, personId, fg_action = actionNumber,
                                     and1_is3 = is3, and1_family = family, and1_zone = SHOT_ZONE_BASIC)]
  made_keys <- made_keys[order(-fg_action)][, .SD[1], .(gameId, period, clock_sec, personId)]
  trips <- merge(trips, made_keys, by = c("gameId", "period", "clock_sec", "personId"), all.x = TRUE)
  # shooting foul at the same stoppage distinguishes shooting 2-shot trips from penalty trips
  shoot_foul_keys <- unique(ev[actionType == "Foul" & grepl("Shooting", subType), .(gameId, period, clock_sec)])[, sf := TRUE]
  trips <- merge(trips, shoot_foul_keys, by = c("gameId", "period", "clock_sec"), all.x = TRUE)
  trips[, trip_type := fcase(
    kind == "flagrant_clearpath", "flagrant_clearpath",
    n_of == 1 & !is.na(fg_action), "and1",
    n_of == 3, "three_shot",
    n_of == 2 & !is.na(sf), "two_shot_shooting",
    n_of == 2, "penalty",
    default = "other_1of1")]
  trips[trip_type != "and1", c("and1_is3", "and1_family", "and1_zone", "fg_action") := NA]
  setnames(trips, "fg_action", "and1_fg_action")
  trips[, c("sf", "kind") := NULL]
  # flag the and-one shot itself on the shots table
  shots[, and1 := FALSE]
  shots[trips[trip_type == "and1"], on = .(gameId, actionNumber = and1_fg_action), and1 := TRUE]

  # -- Fouls committed and turnovers --------------------------------------------------------
  fouls <- ev[actionType == "Foul" & personId > 0, .(
    off_fouls = sum(subType %in% c("Offensive", "Offensive Charge")),
    charges   = sum(subType == "Offensive Charge"),
    flops     = sum(grepl("Flop", subType))), .(personId, teamId)]
  tov <- ev[actionType == "Turnover" & personId > 0, .(
    tov     = .N,
    travels = sum(subType == "Traveling")), .(personId, teamId)]

  # -- Player x team aggregates --------------------------------------------------------------
  s <- shots[heave == FALSE & personId > 0]
  shot_agg <- s[, .(
    fga = .N, fgm = sum(made), fg3a = sum(is3), fg3m = sum(made & is3),
    fg2m_unast = sum(made & !is3 & !assisted), fg3m_unast = sum(made & is3 & !assisted),
    fga_rim   = sum(SHOT_ZONE_BASIC == "Restricted Area", na.rm = TRUE),
    fga_paint = sum(SHOT_ZONE_BASIC == "In The Paint (Non-RA)", na.rm = TRUE),
    fga_mid   = sum(SHOT_ZONE_BASIC == "Mid-Range", na.rm = TRUE),
    fga_c3    = sum(SHOT_ZONE_BASIC %in% c("Left Corner 3", "Right Corner 3"), na.rm = TRUE),
    fga_jumper = sum(family == "jumper"),
    heaves_excl = 0L), .(personId, teamId)]
  heave_agg <- shots[heave == TRUE & personId > 0, .(heaves_excl = .N), .(personId, teamId)]
  shot_agg[heave_agg, on = .(personId, teamId), heaves_excl := i.heaves_excl]

  ft_all <- ft[personId > 0, .(fta = .N, ftm = sum(ft_made),
                               fta_tech = sum(kind == "technical")), .(personId, teamId)]
  trip_agg <- trips[personId > 0, .(
    trips_and1        = sum(trip_type == "and1"),
    and1_3pt          = sum(trip_type == "and1" & and1_is3 %in% TRUE),
    and1_jumper       = sum(trip_type == "and1" & and1_family %in% "jumper"),
    and1_rim          = sum(trip_type == "and1" & and1_zone %in% "Restricted Area"),
    trips_three_shot  = sum(trip_type == "three_shot"),
    trips_two_shoot   = sum(trip_type == "two_shot_shooting"),
    trips_penalty     = sum(trip_type == "penalty"),
    fta_shooting      = sum(trip_fta[trip_type %in% c("and1", "three_shot", "two_shot_shooting")], na.rm = TRUE)),
    .(personId, teamId)]

  keys <- unique(rbindlist(list(shot_agg[, .(personId, teamId)], ft_all[, .(personId, teamId)],
                                tov[, .(personId, teamId)], fouls[, .(personId, teamId)])))
  pts <- Reduce(function(a, b) merge(a, b, by = c("personId", "teamId"), all.x = TRUE),
                list(keys, shot_agg, ft_all, trip_agg, fouls, tov))
  num <- setdiff(names(pts), c("personId", "teamId"))
  for (j in num) set(pts, which(is.na(pts[[j]])), j, 0L)
  names_dt <- ev[personId > 0 & nzchar(playerName), .(player = playerName[.N]), personId]
  pts <- merge(pts, names_dt, by = "personId", all.x = TRUE)

  lab <- season_label(y)
  pts[, `:=`(season = lab, season_type = stype)]
  shots[, `:=`(season = lab, season_type = stype)]
  trips[, `:=`(season = lab, season_type = stype)]
  list(shots = shots[, !c("description", "SHOT_TYPE")], trips = trips, pts = pts,
       qa = data.table(season = lab, season_type = stype,
                       games = uniqueN(ev$gameId), fga = nrow(shots), in_chart = mean(shots$in_chart),
                       heaves = sum(shots$heave),
                       and1_trips = trips[trip_type == "and1", .N], other_1of1 = trips[trip_type == "other_1of1", .N],
                       three_shot = trips[trip_type == "three_shot", .N]))
}

# ---- Run all seasons -----------------------------------------------------------------------
build_clean_tables <- function() {
  grid <- CJ(y = SEASONS, po = c(FALSE, TRUE))
  res <- lapply(seq_len(nrow(grid)), function(i) {
    message("cleaning ", season_label(grid$y[i]), if (grid$po[i]) " PO" else " RS")
    process_season(grid$y[i], grid$po[i])
  })
  shots <- rbindlist(lapply(res, `[[`, "shots"), fill = TRUE)
  trips <- rbindlist(lapply(res, `[[`, "trips"), fill = TRUE)
  pts   <- rbindlist(lapply(res, `[[`, "pts"), fill = TRUE)
  qa    <- rbindlist(lapply(res, `[[`, "qa"))
  write_parquet(shots, file.path(paths$interim, "shots.parquet"))
  write_parquet(trips, file.path(paths$interim, "ft_trips.parquet"))
  write_parquet(pts,   file.path(paths$interim, "player_team_season.parquet"))
  fwrite(qa, "data/audit/clean_qa.csv")
  invisible(qa)
}

if (sys.nframe() == 0) print(build_clean_tables())
