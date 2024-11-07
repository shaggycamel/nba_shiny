
# SPLIT PROCESS INTO NBA DATA & FTY LEAGUE DATA

#  ---------------------------------- NBA
# News --------------------------------------------------------------------

cat("\t- df_nba_news\n")
df_nba_news <<- nba.dataRub::dh_getQuery(db_con, "SELECT * FROM nba.transaction_log WHERE date >= '{cur_date}'::DATE - 60") |>
  dplyr::mutate(dplyr::across(c(transaction_type, team, acc_req), as.factor))


# Player Box Score --------------------------------------------------------------

cat("\t- df_nba_player_box_score\n")
df_nba_player_box_score <<- nba.dataRub::dh_getQuery(db_con, "sql/player_box_score.sql") |>
  dplyr::mutate(season = ordered(season)) |>
  dplyr::mutate(season_type = ordered(season_type, c("Pre Season", "Regular Season", "Playoffs"))) |>
  dplyr::mutate(year_season_type = forcats::fct_cross(season_type, stringr::str_sub(season, start = 6), sep=" "))
  

# Season segments ---------------------------------------------------------

cat("\t- df_nba_season_segments\n")
df_nba_season_segments <<- nba.dataRub::dh_getQuery(db_con, "sql/season_segments.sql") |>
  (\(df){
    dplyr::bind_rows(
      dplyr::filter(df, Sys.Date() > begin_date, Sys.Date() < end_date) |>
        dplyr::mutate(end_date = Sys.Date()),

      setdiff(df, dplyr::filter(df, Sys.Date() > begin_date, Sys.Date() < end_date))
    )
  })() |>
  dplyr::mutate(mid_date = begin_date + (end_date - begin_date) / 2)


# NBA schedule ------------------------------------------------------------

cat("\t- df_nba_schedule\n")
df_nba_schedule <<- nba.dataRub::dh_getQuery(db_con, "sql/nba_schedule.sql") |> 
  dplyr::mutate(scheduled_to_play = 1) # used in h2h calculations


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
  print(paste0(platform, "_", league_id))
  
  # Fantasy competitor -------------------------------------------------
  cat("\t- df_fantasy_competitor\n")
  df_fty_competitor <- nba.dataRub::dh_getQuery(db_con, glue::glue(readr::read_file(here::here("data/sql/fty_league_competitor.sql"))))
  
  # Fantasy league schedule -------------------------------------------------
  cat("\t- df_fantasy_schedule\n")
  df_fty_schedule <- nba.dataRub::dh_getQuery(db_con, glue::glue(readr::read_file(here::here("data/sql/fty_league_schedule.sql")))) |>
    dplyr::select(-tidyselect::ends_with("_name"))
  
  # Fantasy competitor roster -------------------------------------------------------
  cat("\t- df_fty_roster\n")
  df_fty_roster <- nba.dataRub::dh_getQuery(db_con,  glue::glue(readr::read_file(here::here("data/sql/fty_team_roster.sql")))) |>
    dplyr::select(-c(competitor_name, opponent_name)) |> 
    dplyr::left_join(
      df_nba_season_segments |> 
        dplyr::select(tidyr::starts_with("season"), begin_date, end_date) |> 
        dplyr::mutate(dplyr::across(tidyr::ends_with("date"), \(x) as.POSIXct(x, tz = "US/Eastern"))),
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


