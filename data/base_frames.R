

# News --------------------------------------------------------------------

cat("\t- df_news\n")
df_news <<- dh_getQuery(db_con, "SELECT * FROM nba.transaction_log WHERE date >= '{cur_date}'::DATE - 60") |>
  dplyr::mutate(dplyr::across(c(transaction_type, team, acc_req), as.factor))


# Player log --------------------------------------------------------------

cat("\t- df_player_log\n")
df_player_log <<- dh_getQuery(db_con, "sql/player_log.sql") |>
  dplyr::mutate(slug_season = ordered(slug_season)) |>
  dplyr::mutate(season_type = ordered(season_type, c("Pre Season", "Regular Season", "Playoffs"))) |>
  dplyr::mutate(year_season_type = forcats::fct_cross(season_type, stringr::str_sub(slug_season, start = 6), sep=" "))
  

# Season segments ---------------------------------------------------------

cat("\t- df_season_segments\n")
df_season_segments <<- dh_getQuery(db_con, "sql/season_segments.sql") |>
  (\(df){
    dplyr::bind_rows(
      dplyr::filter(df, Sys.Date() > begin_date, Sys.Date() < end_date) |>
        dplyr::mutate(end_date = Sys.Date()),

      setdiff(df, dplyr::filter(df, Sys.Date() > begin_date, Sys.Date() < end_date))
    )
  })() |>
  dplyr::mutate(mid_date = begin_date + (end_date - begin_date) / 2)


# NBA schedule ------------------------------------------------------------

cat("\t- df_schedule\n")
df_nba_schedule <<- dh_getQuery(db_con, "sql/nba_schedule.sql") |> 
  dplyr::mutate(scheduled_to_play = 1) # used in h2h calculations


# NBA team roster -------------------------------------------------------

cat("\t- df_nba_roster\n")
df_nba_roster <<- dh_getQuery(db_con, "sql/nba_team_roster.sql")


# Fantasy league schedule -------------------------------------------------

cat("\t- df_fantasy_schedule\n")
df_fty_schedule <<- dh_getQuery(db_con, "SELECT * FROM fty.league_schedule") |> 
  dplyr::select(-tidyselect::ends_with("_name"))


# Fantasy competitor roster -------------------------------------------------------

cat("\t- df_fty_roster\n")
df_fty_roster <<- dh_getQuery(db_con, "sql/fty_team_roster.sql") |> 
  dplyr::select(-c(competitor_name, opponent_name))


# Fantasy Box Score -------------------------------------------------------

cat("\t- df_fty_box_score\n")
df_fty_box_score <<- dh_getQuery(db_con, "sql/fty_box_score.sql") |> 
  dplyr::relocate(tidyselect::starts_with("competitor"), .before = matchup) |> 
  dplyr::select(-tidyselect::matches("r_name|r_abbrev")) 
