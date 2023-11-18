
suppressMessages({
  library(nba.dataRub)
  library(dplyr)
  library(stringr)
})


# News --------------------------------------------------------------------

cat("\t- df_news\n")
df_news <<- dh_getQuery(db_con, "SELECT * FROM nba.transaction_log WHERE date >= '{cur_date}'::DATE - 15") |>
  mutate(across(c(transaction_type, team, acc_req), as.factor)) |> 
  arrange(desc(date))


# Player log --------------------------------------------------------------

cat("\t- df_player_log\n")
df_player_log <<- dh_getQuery(db_con, "player_log.sql") |> 
  mutate(slug_season = ordered(slug_season)) |> 
  mutate(season_type = ordered(season_type, c("Pre Season", "Regular Season", "Playoffs"))) |> 
  mutate(year_season_type = forcats::fct_cross(season_type, str_sub(slug_season, start = 6), sep=" "))
  

# Season segments ---------------------------------------------------------

cat("\t- df_season_segments\n")
df_season_segments <<- dh_getQuery(db_con, "season_segments.sql") |> 
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
df_schedule <<- dh_getQuery(db_con, "nba_schedule.sql") |> 
  group_by(slug_season) |> 
  mutate(season_week = if_else(season_week < 30, season_week + 52, season_week)) |> 
  (\(t_df) {
    mutate(t_df, season_week = case_when(
      type_season == "Pre Season" ~ 0,
      type_season == "Regular Season" ~ season_week - max(filter(t_df, type_season == "Pre Season")$season_week)
    ))
  })() |> 
  group_by(season_week) |> 
  mutate(week_start = min(game_date), week_end = max(game_date)) |> 
  ungroup() |> 
  arrange(season_week) |> 
  mutate(playing = 1) # used in h2h calculations


# Fantasy league schedule -------------------------------------------------

cat("\t- df_fantasy_schedule\n")
df_fantasy_schedule <<- dh_getQuery(db_con, "SELECT * FROM fty.league_schedule")

# Fantasy competitor roster -------------------------------------------------------

cat("\t- df_roster\n")
df_roster <<- dh_getQuery(db_con, "roster.sql") 