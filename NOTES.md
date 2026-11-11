# NOTES.md: Ethical Hooper Index working log

Running log of decisions, data quirks, and open questions. Newest phase at the bottom.

---

## Phase 0: Data Audit (2026-11-11)

### Environment
- R 4.5.1. Installed: tidyverse, data.table, fixest, mgcv, xgboost, ggplot2, ggrepel, patchwork, renv, httr, rvest, hoopR 3.1.0 (installed during audit).
- Missing R packages (to install in Phase 1 via renv): arrow, gt, reactable, targets, quarto.
- **Quarto CLI not installed** (needed by Phase 7). GitHub CLI `gh` not installed (needed by Phase 8, or we use the web UI).
- Disk: ~500 GB free. Raw bulk CSVs occupy ~3.7 GB in `data/raw/nba_data/` (will be gitignored, converted to Parquet).

### Source-by-source status

| # | Source | Status | Coverage |
|---|--------|--------|----------|
| 1 | `shufinskiy/nba_data` bulk PBP | **Available.** Repo active (last push 2026-11-06). | `nbastatsv3` (PlayByPlayV3): 1996-97 → 2025-26, RS + PO, all 60 files. `nbastats` (V2): only through 2024-25. |
| 2 | Shot detail (x/y, action type) | **Available** in same repo. | 1996-97 → 2025-26, RS + PO, all 60 files. |
| 3 | NBA Stats API via hoopR (box/advanced) | **Blocked from this machine.** All 5 test calls timed out (connection accepted, 0 bytes returned after 60 s). | n/a |
| 3b | pbpstats.com API (replacement) | **Available.** Player season totals keyed by NBA person ID. | 1996-97 → 2025-26, RS + PO. ~230 fields incl. Minutes, OffPoss, Usage, FTA, Turnovers, ThreePtShootingFoulsDrawn, Travels, Offensive/Charge Fouls, assisted/unassisted pts. |
| 4 | Basketball Reference | **Available** (HTTP 200, fast). Season PBP table has shooting fouls drawn, and-ones, offensive fouls drawn/committed; season totals have position. | 1996-97+ |
| 5 | Tracking (drives, pull-ups, defender distance) | **Blocked** (same stats.nba.com timeout). | n/a until resolved |
| 6 | Flopping | PBP has an in-game "Flopping" foul subtype only from 2023-24 (79 / 17 / 22 calls per RS). League fine/warning lists 2012-13+ are press releases, not structured. | Annotation only |

### Bulk PBP + shot detail audit (full table: `data/audit/pbp_shot_audit.csv`, script `R/00_data_audit.R`)
- Every season/type present. Game counts match the real schedule: 1189 (29 teams), 725 (1998-99 lockout), 990 (2011-12), 1059 (2019-20), 1080 (2020-21), 1230 otherwise.
- Play-in (game-ID prefix 5) and NBA Cup final are **not** in the files, which matches official RS/PO stat conventions.
- Shot chart ↔ PBP join on (gameId, actionNumber = GAME_EVENT_ID): **≥ 99.88% of PBP FGAs in every season**, same shooter ≥ 99.95%, made flag agreement 100%.
- **Quirk: 2012-13 RS PBP is missing 16 games** (IDs 21201215–21201230, final night of the season). Shot chart has them; the V2 archive (`nbastats_2012`) has them (2,687 FG events). Fix: patch those 16 games from V2 in Phase 2.
- **Quirk: shot ACTION_TYPE granularity changes by era.** 9–10 types (1996-97 → 1999-00), 22–29 types (2000-01 → 2006-07), 46–53 types (2007-08+). "Step Back", "Pullup", "Fadeaway" don't exist in early seasons. → affects xFG design (see open question 3).
- **Quirk: pre-~2011 rim shots are stamped at exactly (0,0)** (95% of restricted-area attempts in 1996-97, 77% in 2005-06, 0% in 2012-13). Zone and distance still usable; fine-grained location inside the restricted area only exists later.
- **Quirk: charges** are a separate foul subtype ("Offensive Charge") only from 2010-11; earlier they're inside "Offensive". Summing both gives a consistent "offensive fouls incl. charges" count.
- League-wide drift (handled by within-season z-scores): 3-shot FT trips 239 (1996-97) → 933 (2025-26); travel calls 3,925 (2006-07 peak) → 1,301 (2025-26); 3PA share 21% → 42%.

### How the needed events are encoded in PBP V3
- **Shooting foul:** `actionType="Foul"`, `subType="Shooting"`. `personId` = the *fouler*. The parenthetical name in the description (e.g. "(J.DeRosa)") is the **referee**, not the fouled player. → Fouls *drawn* are credited to the free-throw shooter on the following FT rows.
- **Three-shot foul:** FT rows with `subType="Free Throw 1 of 3"` (non-flagrant). The fouled missed 3PA is *not* logged as an FGA.
- **And-one:** made FG → Shooting foul at same clock → `"Free Throw 1 of 1"` by the shooter. Shot location comes from the made FG (jumper vs rim classifiable).
- **Travel:** `actionType="Turnover"`, `subType="Traveling"`.
- **Offensive foul:** `Foul/Offensive` or `Foul/Offensive Charge`, followed by a `Turnover` row ("Foul" pre-2010s, "Offensive Foul Turnover" later). Count the Foul row only to avoid double counting.
- **Assist attribution:** only in the made-shot description, e.g. `"... (Green 3 AST)"`. ~57–64% of made FG tagged, stable across seasons. Unassisted = made FG without the tag.
- **3PT flag:** `shotValue` column missing in 1996-97 file; use shot-chart `SHOT_TYPE` (joined) instead.

### Open questions for Rob (Phase 0 checkpoint)
1. Approve PBP V3 as the primary PBP source (all 30 seasons, one format), patching 2012-13 from V2?
2. Approve pbpstats.com as the box-score source (minutes, possessions, usage, FTA, TOV) + Basketball Reference for position, since stats.nba.com is blocked?
3. xFG action types: harmonized coarse taxonomy across all seasons (recommended) vs. full detail?
4. Tier 2 tracking validation: how to proceed given stats.nba.com is blocked?
