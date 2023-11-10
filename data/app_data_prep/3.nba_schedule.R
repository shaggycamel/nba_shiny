
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


tbl_week_games <<- df_schedule |> 
      mutate(game_date = paste0(weekdays(game_date, abbreviate = TRUE), " (", format(game_date, "%m/%d"), ")")) |> 
      select(slug_season, season_week, game_date, team, against) |> 
      nest_by(slug_season, season_week, .keep = TRUE) |> 
      mutate(data = list(
        pivot_wider(data, names_from = game_date, values_from = against, values_fn = list) |> 
        left_join(select(df_week_game_count, season_week, team, contains("games"),-week_games)) |> 
        select(-slug_season, -season_week) |> 
        arrange(desc(week_games_remaining), team) |> 
        rename_with(~ str_to_title(str_replace_all(.x, "_", " ")))
      ))
