# IS IT VIABLE QUERYING DATA FROM COCKROACH ON THE SHINY SERVER?

base_frames <- function(db_con, base_parameters) {
  # Set variables needed
  cur_date <- base_parameters$cur_date
  cur_season <- base_parameters$cur_season
  prev_season <- base_parameters$prev_season

  # Genreal query template
  query_template <- function(qry_obj, sn = "cur", pf = TRUE, lg = TRUE) {
    # fmt: skip
    paste0(
      "SELECT * FROM ", qry_obj,
      " WHERE (season >= '{", sn, "_season}' OR season IS NULL)",
      if (pf) " AND (platform = '{platform}' OR platform IS NULL)",
      if (lg) " AND (league_id = {league_id} OR league_id IS NULL)"
    )
  }

  #  -- SPLIT PROCESS INTO NBA DATA & FTY LEAGUE DATA
  #  ---------------------------------- NBA
  # Player Box Score --------------------------------------------------------------

  cat("\t- df_nba_player_box_score\n")
  df_nba_player_box_score <- dh_getQuery(db_con, glue(query_template("nba.nba_player_box_score_vw", sn = "prev", pf = FALSE, lg = FALSE))) |>
    mutate(game_date = force_tz(game_date, tz = "EST")) |>
    mutate(season = ordered(season)) |>
    mutate(season_type = ordered(season_type, c("Pre Season", "Regular Season", "Playoffs"))) |>
    mutate(year_season_type = fct_cross(season_type, str_sub(season, start = 6), sep = " "))

  # Team Box Score --------------------------------------------------------------

  cat("\t- df_nba_team_box_score\n")
  df_nba_team_box_score <- dh_getQuery(db_con, glue(query_template("nba.nba_team_box_score_vw", sn = "prev", pf = FALSE, lg = FALSE))) |>
    mutate(game_date = force_tz(game_date, tz = "EST")) |>
    mutate(season = ordered(season)) |>
    mutate(season_type = ordered(season_type, c("Pre Season", "Regular Season", "Playoffs"))) |>
    mutate(year_season_type = fct_cross(season_type, str_sub(season, start = 6), sep = " "))

  # Season segments ---------------------------------------------------------

  cat("\t- df_nba_season_segments\n")
  df_nba_season_segments <- dh_getQuery(db_con, glue(query_template("nba.nba_season_segments_vw", sn = "prev", pf = FALSE, lg = FALSE))) |>
    mutate(across(ends_with("date"), \(x) force_tz(x, "EST"))) |>
    (\(df) {
      bind_rows(
        # REPLACE THIS SECTION WITH CUR_DATE?????
        filter(df, cur_date > begin_date, cur_date < end_date) |>
          mutate(end_date = cur_date),

        setdiff(df, filter(df, cur_date > begin_date, cur_date < end_date))
      )
    })() |>
    mutate(mid_date = begin_date + (end_date - begin_date) / 2)

  # NBA schedule ------------------------------------------------------------

  # ALTERED THIS, NOW NEED TO CHECK IF IT WORKS
  cat("\t- df_nba_schedule\n")
  df_nba_schedule <- dh_getQuery(db_con, glue(query_template("nba.nba_season_segments_vw", sn = "prev", pf = FALSE, lg = FALSE))) |>
    filter(season == cur_season, season_type == "Regular Season") |>
    select(begin_date, end_date) |>
    mutate(across(ends_with("date"), as.Date)) |>
    pivot_longer(cols = ends_with("date"), values_to = "game_date") |>
    complete(game_date = seq.Date(min(game_date), max(game_date), by = "day")) |>
    select(game_date) |>
    left_join(
      dh_getQuery(db_con, glue(query_template("nba.nba_schedule_vw", pf = FALSE, lg = FALSE))),
      by = join_by(game_date)
    ) |>
    mutate(
      season = cur_season,
      season_type = "Regular Season",
      game_date = force_tz(game_date, tz = "EST"),
      scheduled_to_play = ifelse(!is.na(game_id), 1, game_id) # used in h2h calculations
    )

  # NBA team roster -------------------------------------------------------

  cat("\t- df_nba_roster\n")
  df_nba_roster <- dh_getQuery(db_con, glue(query_template("nba.nba_latest_team_roster_vw", pf = FALSE, lg = FALSE)))

  # NBA Injuries ------------------------------------------------------------

  cat("\t- df_nba_injuries\n")
  df_nba_injuries <- dh_getQuery(db_con, glue(query_template("nba.nba_injuries_vw", pf = FALSE, lg = FALSE))) |>
    mutate(
      reason = if_else(status == "Available", NA, str_remove(reason, "Injury/Illness - ")),
      status = ordered(status, c("Available", "Probable", "Questionable", "Doubtful", "Out"))
    )

  # Save base NBA objects -------------------------------------------------------
  save(list = str_subset(objects(), "df_nba"), file = here("nba_base.RData"))

  #  ---------------------------------- FANTASY
  # Fty base object ---------------------------------------------------------

  cat("\t- df_fty_base\n")
  df_fty_base <- dh_getQuery(db_con, glue(query_template("fty.fty_base_vw", pf = FALSE, lg = FALSE))) |>
    mutate(league_handle = str_c(platform, league_id, sep = "_"))

  saveRDS(df_fty_base, here("fty_base.RDS"))

  #### Map leagues
  pwalk(distinct(df_fty_base, platform, league_id), \(platform, league_id) {
    cat(paste0(platform, "_", league_id, "\n"))

    # Fantasy competitor -------------------------------------------------
    cat("\t- df_fantasy_competitor\n")
    df_fty_competitor <- dh_getQuery(db_con, glue(query_template("fty.league_competitor")))

    # Fantasy league schedule -------------------------------------------------
    cat("\t- df_fantasy_schedule\n")
    df_fty_schedule <- dh_getQuery(db_con, glue(query_template("fty.fty_league_schedule_vw")))

    # Fantasy competitor roster -------------------------------------------------------
    cat("\t- df_fty_roster\n")
    df_fty_roster <- dh_getQuery(db_con, glue(query_template("fty.fty_team_roster_schedule_vw"))) |>
      mutate(timestamp = with_tz(timestamp, tzone = "EST")) |>
      mutate(date = as_date(timestamp), .after = timestamp) |>
      filter((max(timestamp) - timestamp) < 1000, .by = c(date, competitor_id)) |>
      select(-c(competitor_name, opponent_name)) |>
      left_join(
        select(df_nba_season_segments, starts_with("season"), begin_date, end_date),
        by = join_by(season, timestamp >= begin_date, timestamp <= end_date)
      ) |>
      filter(season_type == "Regular Season")

    # Fantasy Box Score -------------------------------------------------------
    cat("\t- df_fty_box_score\n")
    df_fty_box_score <- dh_getQuery(db_con, glue(query_template("fty.fty_matchup_box_score_vw"))) |>
      relocate(starts_with("competitor"), .before = matchup) |>
      select(-matches("r_name|r_abbrev"))

    # Fantasy free agents -------------------------------------------------------
    cat("\t- df_fty_free_agents\n")
    df_fty_free_agents <- dh_getQuery(db_con, glue(query_template("fty.free_agents")))

    # Fantasy category labels -------------------------------------------------------
    cat("\t- df_fty_cats\n")
    df_fty_cats <- dh_getQuery(db_con, glue(query_template("fty.fty_categories_vw"))) |>
      arrange(display_order)

    # Save data objects
    save(list = str_subset(objects(), "fty|cat"), file = here(paste0("fty_", platform, "_", league_id, ".RData")))
  })
}
