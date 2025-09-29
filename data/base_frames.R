# Genreal query template
# fmt: skip
query_template <- function(qry_obj, sn = "cur", pf = TRUE, lg = TRUE) {
  paste0(
    "SELECT * FROM ", qry_obj,
    " WHERE (season >= '{", sn, "_season}' OR season IS NULL)",
    if (pf) " AND (platform = '{platform}' OR platform IS NULL)",
    if (lg) " AND (league_id = {league_id} OR league_id IS NULL)",
      " AND (season != '2025-26' OR season IS NULL)" # DELETE
  )
}


#  -- SPLIT PROCESS INTO NBA DATA & FTY LEAGUE DATA
#  ---------------------------------- NBA
# News --------------------------------------------------------------------

cat("\t- df_nba_news\n")
df_nba_news <<- nba.dataRub::dh_getQuery(db_con, "SELECT * FROM nba.transaction_log WHERE date >= '{cur_date}'::DATE - 60") |>
  dplyr::mutate(dplyr::across(c(transaction_type, team, acc_req), as.factor))


# Player Box Score --------------------------------------------------------------

cat("\t- df_nba_player_box_score\n")
df_nba_player_box_score <<- nba.dataRub::dh_getQuery(db_con, query_template("nba.nba_player_box_score_vw", sn = "prev", pf = FALSE, lg = FALSE)) |>
  dplyr::mutate(game_date = lubridate::force_tz(game_date, tz = "EST")) |>
  dplyr::mutate(season = ordered(season)) |>
  dplyr::mutate(season_type = ordered(season_type, c("Pre Season", "Regular Season", "Playoffs"))) |>
  dplyr::mutate(year_season_type = forcats::fct_cross(season_type, stringr::str_sub(season, start = 6), sep = " "))


# Team Box Score --------------------------------------------------------------

cat("\t- df_nba_team_box_score\n")
df_nba_team_box_score <<- nba.dataRub::dh_getQuery(db_con, query_template("nba.nba_team_box_score_vw", sn = "prev", pf = FALSE, lg = FALSE)) |>
  dplyr::mutate(game_date = lubridate::force_tz(game_date, tz = "EST")) |>
  dplyr::mutate(season = ordered(season)) |>
  dplyr::mutate(season_type = ordered(season_type, c("Pre Season", "Regular Season", "Playoffs"))) |>
  dplyr::mutate(year_season_type = forcats::fct_cross(season_type, stringr::str_sub(season, start = 6), sep = " "))


# Season segments ---------------------------------------------------------

cat("\t- df_nba_season_segments\n")
df_nba_season_segments <<- nba.dataRub::dh_getQuery(db_con, query_template("nba.nba_season_segments_vw", sn = "prev", pf = FALSE, lg = FALSE)) |>
  dplyr::mutate(dplyr::across(tidyselect::ends_with("date"), \(x) lubridate::force_tz(x, "EST"))) |>
  (\(df) {
    dplyr::bind_rows(
      # REPLACE THIS SECTION WITH CUR_DATE?????
      dplyr::filter(df, cur_date > begin_date, cur_date < end_date) |>
        dplyr::mutate(end_date = cur_date),

      setdiff(df, dplyr::filter(df, cur_date > begin_date, cur_date < end_date))
    )
  })() |>
  dplyr::mutate(mid_date = begin_date + (end_date - begin_date) / 2)


# NBA schedule ------------------------------------------------------------

# ALTERED THIS, NOW NEED TO CHECK IF IT WORKS
cat("\t- df_nba_schedule\n")
df_nba_schedule <<- nba.dataRub::dh_getQuery(db_con, query_template("nba.nba_season_segments_vw", sn = "prev", pf = FALSE, lg = FALSE)) |>
  dplyr::filter(season == cur_season, season_type == "Regular Season") |>
  dplyr::select(begin_date, end_date) |>
  dplyr::mutate(dplyr::across(tidyselect::ends_with("date"), as.Date)) |>
  tidyr::pivot_longer(cols = tidyselect::ends_with("date"), values_to = "game_date") |>
  tidyr::complete(game_date = seq.Date(min(game_date), max(game_date), by = "day")) |>
  dplyr::select(game_date) |>
  dplyr::left_join(
    nba.dataRub::dh_getQuery(db_con, query_template("nba.nba_schedule_vw", pf = FALSE, lg = FALSE)),
    by = dplyr::join_by(game_date)
  ) |>
  dplyr::mutate(
    season = cur_season,
    season_type = "Regular Season",
    game_date = lubridate::force_tz(game_date, tz = "EST"),
    scheduled_to_play = ifelse(!is.na(game_id), 1, game_id) # used in h2h calculations
  )


# NBA team roster -------------------------------------------------------

cat("\t- df_nba_roster\n")
df_nba_roster <<- nba.dataRub::dh_getQuery(db_con, query_template("nba.nba_latest_team_roster_vw", pf = FALSE, lg = FALSE))


# NBA Injuries ------------------------------------------------------------

cat("\t- df_nba_injuries\n")
df_nba_injuries <<- nba.dataRub::dh_getQuery(db_con, query_template("nba.nba_injuries_vw", pf = FALSE, lg = FALSE)) |>
  dplyr::mutate(
    reason = dplyr::if_else(status == "Available", NA, stringr::str_remove(reason, "Injury/Illness - ")),
    status = ordered(status, c("Available", "Probable", "Questionable", "Doubtful", "Out"))
  )


# Save base NBA objects -------------------------------------------------------
save(list = stringr::str_subset(objects(), "df_nba"), file = here::here("nba_base.RData"))


#  ---------------------------------- FANTASY
# Fty base object ---------------------------------------------------------

cat("\t- df_fty_base\n")
df_fty_base <<- nba.dataRub::dh_getQuery(db_con, query_template("fty.fty_base_vw", pf = FALSE, lg = FALSE))
saveRDS(df_fty_base, here::here("fty_base.RDS"))


#### Map leagues
purrr::pwalk(distinct(df_fty_base, platform, league_id), \(platform, league_id) {
  cat(paste0(platform, "_", league_id, "\n"))

  # Fantasy competitor -------------------------------------------------
  cat("\t- df_fantasy_competitor\n")
  df_fty_competitor <- nba.dataRub::dh_getQuery(db_con, glue::glue(query_template("fty.league_competitor")))

  # Fantasy league schedule -------------------------------------------------
  cat("\t- df_fantasy_schedule\n")
  df_fty_schedule <- nba.dataRub::dh_getQuery(db_con, glue::glue(query_template("fty.fty_league_schedule_vw")))

  # Fantasy competitor roster -------------------------------------------------------
  cat("\t- df_fty_roster\n")
  df_fty_roster <- nba.dataRub::dh_getQuery(db_con, glue::glue(query_template("fty.fty_team_roster_schedule_vw"))) |>
    dplyr::mutate(timestamp = lubridate::with_tz(timestamp, tzone = "EST")) |>
    dplyr::mutate(date = lubridate::as_date(timestamp), .after = timestamp) |>
    dplyr::filter((max(timestamp) - timestamp) < 1000, .by = c(date, competitor_id)) |>
    dplyr::select(-c(competitor_name, opponent_name)) |>
    dplyr::left_join(
      dplyr::select(df_nba_season_segments, tidyr::starts_with("season"), begin_date, end_date),
      by = dplyr::join_by(season, timestamp >= begin_date, timestamp <= end_date)
    ) |>
    dplyr::filter(season_type == "Regular Season")

  # Fantasy Box Score -------------------------------------------------------
  cat("\t- df_fty_box_score\n")
  df_fty_box_score <- nba.dataRub::dh_getQuery(db_con, glue::glue(query_template("fty.fty_matchup_box_score_vw"))) |>
    dplyr::relocate(tidyselect::starts_with("competitor"), .before = matchup) |>
    dplyr::select(-tidyselect::matches("r_name|r_abbrev"))

  # Fantasy free agents -------------------------------------------------------
  cat("\t- df_fty_free_agents\n")
  df_fty_free_agents <- nba.dataRub::dh_getQuery(db_con, glue::glue(query_template("fty.free_agents")))

  # Fantasy category labels -------------------------------------------------------
  cat("\t- df_fty_cats\n")
  df_fty_cats <- nba.dataRub::dh_getQuery(db_con, glue::glue(query_template("fty.fty_categories_vw"))) |>
    fill(season, platform, league_id) |> # fill missing values downwards
    arrange(display_order)

  # fmt: skip
  fmt_to_nba_cat_name <- magrittr::`%$%`(df_fty_cats, purrr::map(setNames(nba_category, fmt_category), \(x) as.vector(x)))
  nba_to_fmt_cat_name <- magrittr::`%$%`(df_fty_cats, purrr::map(setNames(fmt_category, nba_category), \(x) as.vector(x)))
  fty_h2h_cols <- filter(df_fty_cats, measured)$nba_category

  df_fty_cats2 <- nba.dataRub::dh_getQuery(db_con, glue::glue(query_template("fty.fty_categories2_vw"))) |>
    # fill(season, platform, league_id, .direction = "updown") |> # fill missing values downwards
    arrange(display_order)

  # Save data objects
  save(list = stringr::str_subset(objects(), "fty|cat"), file = here::here(paste0("fty_", platform, "_", league_id, ".RData")))
})
