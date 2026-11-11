# 00_data_audit.R
# Phase 0 data audit: summarises coverage and event encoding of the bulk
# NBA.com play-by-play (PlayByPlayV3) and shot-detail archives, one row per
# season x season type. Output: data/audit/pbp_shot_audit.csv
# Why: we must confirm every season is complete and consistently encoded
# before any metric is built on top of it.

suppressPackageStartupMessages(library(data.table))

raw_dir   <- "data/raw/nba_data"
audit_dir <- "data/audit"
dir.create(audit_dir, showWarnings = FALSE, recursive = TRUE)

# Season start year -> "1996-97" label
season_label <- function(y) sprintf("%d-%02d", y, (y + 1) %% 100)

# Audit one season/type: returns a one-row data.table of coverage + encoding stats
audit_one <- function(y, po = FALSE) {
  tag  <- if (po) sprintf("po_%d", y) else as.character(y)
  pbp  <- fread(file.path(raw_dir, sprintf("nbastatsv3_%s.csv", tag)), showProgress = FALSE,
                select = c("gameId", "actionNumber", "personId", "actionType", "subType",
                           "description", "xLegacy", "yLegacy"))
  shot <- fread(file.path(raw_dir, sprintf("shotdetail_%s.csv", tag)), showProgress = FALSE,
                select = c("GAME_ID", "GAME_EVENT_ID", "PLAYER_ID", "ACTION_TYPE", "SHOT_TYPE"))
  fg   <- pbp[actionType %in% c("Made Shot", "Missed Shot")]
  made <- fg[actionType == "Made Shot"]
  ft   <- pbp[actionType == "Free Throw"]
  fl   <- pbp[actionType == "Foul"]
  to   <- pbp[actionType == "Turnover"]
  j    <- merge(fg[, .(gameId, actionNumber, personId)],
                shot[, .(GAME_ID, GAME_EVENT_ID, PLAYER_ID)],
                by.x = c("gameId", "actionNumber"), by.y = c("GAME_ID", "GAME_EVENT_ID"))
  gid_prefix <- substr(sprintf("%010d", as.integer(unique(pbp$gameId))), 3, 3)
  data.table(
    season        = season_label(y),
    type          = if (po) "PO" else "RS",
    games         = uniqueN(pbp$gameId),
    game_types    = paste(names(table(gid_prefix)), table(gid_prefix), sep = ":", collapse = " "),
    pbp_rows      = nrow(pbp),
    pbp_fga       = nrow(fg),
    shot_rows     = nrow(shot),
    shot_join_pct = round(100 * nrow(j) / nrow(fg), 2),
    same_player_pct = round(100 * mean(j$personId == j$PLAYER_ID), 2),
    fga_3pt_pct   = round(100 * mean(shot$SHOT_TYPE == "3PT Field Goal"), 1),
    ast_tag_pct   = round(100 * mean(grepl("AST)", made$description, fixed = TRUE)), 1),
    shot_action_types = uniqueN(shot$ACTION_TYPE),
    ft_trips_3shot = ft[subType == "Free Throw 1 of 3", .N],
    ft_1of1       = ft[subType == "Free Throw 1 of 1", .N],
    shooting_fouls = fl[subType == "Shooting", .N],
    off_fouls     = fl[subType == "Offensive", .N],
    off_charge    = fl[subType == "Offensive Charge", .N],
    flopping      = fl[grepl("Flop", subType), .N],
    travels       = to[subType == "Traveling", .N],
    missing_person_fg = fg[is.na(personId) | personId == 0, .N]
  )
}

grid <- CJ(y = 1996:2025, po = c(FALSE, TRUE))
audit <- rbindlist(lapply(seq_len(nrow(grid)), function(i) {
  message("auditing ", grid$y[i], if (grid$po[i]) " PO" else " RS")
  audit_one(grid$y[i], grid$po[i])
}))
fwrite(audit, file.path(audit_dir, "pbp_shot_audit.csv"))
print(audit)

# Shot ACTION_TYPE granularity by season (regular season) -- matters for xFG features
at <- rbindlist(lapply(1996:2025, function(y) {
  s <- fread(file.path(raw_dir, sprintf("shotdetail_%d.csv", y)), select = "ACTION_TYPE", showProgress = FALSE)
  s[, .N, ACTION_TYPE][, season := season_label(y)]
}))
fwrite(at, file.path(audit_dir, "shot_action_types_by_season.csv"))
