
# SPLIT PROCESS INTO NBA DATA & FTY LEAGUE DATA

#  ---------------------------------- NBA
# News --------------------------------------------------------------------

cat("\t- df_nba_news\n")
df_nba_news <<- nba.dataRub::dh_getQuery(db_con, "SELECT * FROM nba.transaction_log WHERE date >= '{cur_date}'::DATE - 60") |>
  dplyr::mutate(dplyr::across(c(transaction_type, team, acc_req), as.factor))


# Player Box Score --------------------------------------------------------------

cat("\t- df_nba_player_box_score\n")
df_nba_player_box_score <<- nba.dataRub::dh_getQuery(db_con, "sql/player_box_score.sql") |> 
  dplyr::mutate(game_date = lubridate::force_tz(game_date, tz = "EST")) |> 
  dplyr::mutate(season = ordered(season)) |>
  dplyr::mutate(season_type = ordered(season_type, c("Pre Season", "Regular Season", "Playoffs"))) |>
  dplyr::mutate(year_season_type = forcats::fct_cross(season_type, stringr::str_sub(season, start = 6), sep=" "))
  

# Season segments ---------------------------------------------------------

cat("\t- df_nba_season_segments\n")
df_nba_season_segments <<- nba.dataRub::dh_getQuery(db_con, "sql/season_segments.sql") |>
  dplyr::mutate(dplyr::across(tidyselect::ends_with("date"), \(x) lubridate::force_tz(x, "EST"))) |> 
  (\(df){
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
df_nba_schedule <<- nba.dataRub::dh_getQuery(db_con, "sql/season_segments.sql") |> 
  dplyr::filter(season == cur_season, season_type == "Regular Season") |> 
  dplyr::select(begin_date, end_date) |> 
  dplyr::mutate(dplyr::across(tidyselect::ends_with("date"), as.Date)) |> 
  tidyr::pivot_longer(cols = tidyselect::ends_with("date"), values_to = "game_date") |> 
  tidyr::complete(game_date = seq.Date(min(game_date), max(game_date), by = "day")) |> 
  dplyr::select(game_date) |> 
  dplyr::left_join(nba.dataRub::dh_getQuery(db_con, "sql/nba_schedule.sql"), by = dplyr::join_by(game_date)) |> 
  dplyr::mutate(
    season = cur_season,
    season_type = "Regular Season",
    game_date = lubridate::force_tz(game_date, tz = "EST"),
    scheduled_to_play = ifelse(!is.na(game_id), 1, game_id) # used in h2h calculations
  )


# NBA team roster -------------------------------------------------------

cat("\t- df_nba_roster\n")
df_nba_roster <<- nba.dataRub::dh_getQuery(db_con, "sql/nba_team_roster.sql") 


# Save base NBA objects -------------------------------------------------------
save(list = stringr::str_subset(objects(), "df_nba"), file = here::here("nba_base.RData"))


#  ---------------------------------- FANTASY
# Fty base object ---------------------------------------------------------

cat("\t- df_fty_base\n")
df_fty_base <<- nba.dataRub::dh_getQuery(db_con, "sql/fty_base.sql") 
saveRDS(df_fty_base, here::here("fty_base.RDS"))


#### Map leagues
purrr::walk2(unique(df_fty_base$platform), unique(df_fty_base$league_id), \(platform, league_id){
  cat(paste0(platform, "_", league_id, "\n"))
  
  # Fantasy competitor -------------------------------------------------
  cat("\t- df_fantasy_competitor\n")
  df_fty_competitor <- nba.dataRub::dh_getQuery(db_con, glue::glue(readr::read_file(here::here("data/sql/fty_league_competitor.sql"))))
  
  # Fantasy league schedule -------------------------------------------------
  cat("\t- df_fantasy_schedule\n")
  df_fty_schedule <- nba.dataRub::dh_getQuery(db_con, glue::glue(readr::read_file(here::here("data/sql/fty_league_schedule.sql"))))

  # Fantasy competitor roster -------------------------------------------------------
  cat("\t- df_fty_roster\n")
  df_fty_roster <- nba.dataRub::dh_getQuery(db_con,  glue::glue(readr::read_file(here::here("data/sql/fty_team_roster.sql")))) |>
    dplyr::mutate(timestamp = lubridate::with_tz(timestamp, tzone = "EST")) |> 
    dplyr::mutate(date = lubridate::as_date(timestamp), .after = timestamp) |> 
    dplyr::filter(max(timestamp) - timestamp < 1000, .by = c(date, competitor_id)) |>
    dplyr::select(-c(competitor_name, opponent_name)) |> 
    dplyr::left_join(
      dplyr::select(df_nba_season_segments, tidyr::starts_with("season"), begin_date, end_date),
      by = dplyr::join_by(season, timestamp >= begin_date, timestamp <= end_date)
    ) |> 
    dplyr::filter(season_type == "Regular Season")
  
  # Fantasy Box Score -------------------------------------------------------
  cat("\t- df_fty_box_score\n")
  df_fty_box_score <- nba.dataRub::dh_getQuery(db_con, glue::glue(readr::read_file(here::here("data/sql/fty_box_score.sql")))) |>
    dplyr::relocate(tidyselect::starts_with("competitor"), .before = matchup) |>
    dplyr::select(-tidyselect::matches("r_name|r_abbrev"))
  
  # Fantasy free agents -------------------------------------------------------
  cat("\t- df_fty_free_agents\n")
  df_fty_free_agents <- nba.dataRub::dh_getQuery(db_con,  glue::glue(readr::read_file(here::here("data/sql/fty_free_agents.sql"))))
  
  # Save data objects
  save(list = stringr::str_subset(objects(), "df_fty"), file = here::here(paste0("fty_", platform, "_", league_id, ".RData")))
  
})


