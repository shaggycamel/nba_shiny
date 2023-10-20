
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
  arrange(season_week)