
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


# Calculate games left this week variable
df_week_game_count <<- df_schedule |> 
  mutate(week_games_remaining = game_date >= cur_date) |> 
  summarise(
    week_games_remaining = sum(week_games_remaining), 
    week_games = n(), 
    .by = c(season_week, week_start, week_end, team)
  ) |> (\(t_df) {
    left_join(
      t_df,
      select(t_df, team, next_week = season_week, following_week_games = week_games),
      join_by(team, closest(season_week < next_week))
    ) |> 
    select(-next_week)
  })()