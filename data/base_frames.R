
suppressMessages({
  library(nba.dataRub)
  library(stringr)
})


# News --------------------------------------------------------------------

cat("\t- df_news\n")
df_news <<- dh_getQuery(db_con, "SELECT * FROM nba.transaction_log WHERE date >= '{cur_date}'::DATE - 60") |>
  mutate(across(c(transaction_type, team, acc_req), as.factor)) |>
  arrange(desc(date))


# Player log --------------------------------------------------------------

cat("\t- df_player_log\n")
df_player_log <<- dh_getQuery(db_con, "sql/player_log.sql") |>
  mutate(slug_season = ordered(slug_season)) |>
  mutate(season_type = ordered(season_type, c("Pre Season", "Regular Season", "Playoffs"))) |>
  mutate(year_season_type = forcats::fct_cross(season_type, str_sub(slug_season, start = 6), sep=" "))
  

# Season segments ---------------------------------------------------------

cat("\t- df_season_segments\n")
df_season_segments <<- dh_getQuery(db_con, "sql/season_segments.sql") |>
  (\(df){
    bind_rows(
      filter(df, Sys.Date() > begin_date, Sys.Date() < end_date) |>
        mutate(end_date = Sys.Date()),

      setdiff(df, filter(df, Sys.Date() > begin_date, Sys.Date() < end_date))
    )
  })() |>
  mutate(mid_date = begin_date + (end_date - begin_date) / 2)


# NBA schedule ------------------------------------------------------------

cat("\t- df_schedule\n")
df_nba_schedule <<- dh_getQuery(db_con, "sql/nba_schedule.sql") |> 
  group_by(slug_season) |> 
  mutate(season_week = ifelse(season_week < 30, season_week + 52, season_week))

minus_wk <- df_nba_schedule |> 
  filter(type_season == "Pre Season") |> 
  pull(season_week) |> 
  max()
  
df_nba_schedule <<- df_nba_schedule |> 
  mutate(season_week = case_when(
    type_season == "Pre Season" ~ 0,
    type_season == "Regular Season" ~ season_week - minus_wk
  )) |> 
  mutate(season_week = if_else(season_week > 17, season_week - 1, season_week)) |> # all star week
  group_by(season_week) |> 
  mutate(week_start = min(game_date), week_end = max(game_date)) |> 
  ungroup() |> 
  arrange(game_date) |> 
  mutate(scheduled_to_play = 1) # used in h2h calculations


# NBA team roster -------------------------------------------------------

cat("\t- df_nba_roster\n")
df_nba_roster <<- dh_getQuery(db_con, "sql/nba_team_roster.sql")


# Fantasy Base object -----------------------------------------------------

df_fty_base <- dh_getQuery(db_con, "sql/fty_league_info.sql") |> 
  mutate(across(
    matches("r_name|_abbrev"), 
    \(x){
      textclean::replace_emoji(x) |> 
        textclean::replace_emoticon() |> 
        str_remove_all("<.+>") |> 
        str_squish() 
    }, 
    .names = "{.col}_clean"
  )) 
  # select(
  #   season, 
  #   league_id, 
  #   competitor_id, 
  #   competitor_abbrev = competitor_abbrev_clean, 
  #   competitor_name = competitor_name_clean
  # )


# Fantasy league schedule -------------------------------------------------

cat("\t- df_fantasy_schedule\n")
df_fty_schedule <<- dh_getQuery(db_con, "SELECT * FROM fty.league_schedule") |> 
  select(-ends_with("_name")) 
  # left_join(df_fty_base, by = join_by(season, league_id, competitor_id)) |> 
  # left_join(
  #   rename(df_fty_base, opponent_abbrev = competitor_abbrev, opponent_name = competitor_name),
  #   by = join_by(season, league_id, opponent_id == competitor_id)
  # )


# Fantasy competitor roster -------------------------------------------------------

cat("\t- df_fty_roster\n")
df_fty_roster <<- dh_getQuery(db_con, "sql/fty_team_roster.sql") |> 
  select(-c(competitor_name, opponent_name))
  # left_join(df_fty_base, by = join_by(season, league_id, competitor_id)) |> 
  # left_join(
  #   rename(df_fty_base, opponent_abbrev = competitor_abbrev, opponent_name = competitor_name),
  #   by = join_by(season, league_id, opponent_id == competitor_id)
  # )


# Fantasy Box Score -------------------------------------------------------

cat("\t- df_fty_box_score\n")
df_fty_box_score <<- dh_getQuery(db_con, "sql/fty_box_score.sql") |> 
  dplyr::relocate(starts_with("competitor"), .before = matchup) |> 
  select(-matches("r_name|r_abbrev")) 
  # left_join(df_fty_base, by = join_by(season, league_id, competitor_id))
