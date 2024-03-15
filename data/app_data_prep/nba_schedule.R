

# Calculate games left this week variable
df_week_game_count <<- df_nba_schedule |> 
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


tbl_week_games <<- df_nba_schedule |> 
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