

# Calculate games left this week variable
df_week_game_count <<- df_nba_schedule |> 
  left_join(
    distinct(select(df_fty_schedule, starts_with("week"))),
    by = join_by(game_date >= week_start, game_date <= week_end)
  ) |> 
  mutate(week_games_remaining = game_date >= cur_date) |> 
  summarise(
    week_games_remaining = sum(week_games_remaining), 
    week_games = n(), 
    .by = c(week, week_start, week_end, team)
  ) |> (\(t_df) {
    left_join(
      t_df,
      select(t_df, team, next_week = week, following_week_games = week_games),
      join_by(team, closest(week < next_week))
    ) |> 
    select(-next_week)
  })()


tbl_week_games <<- df_nba_schedule |> 
  left_join(
    distinct(select(df_fty_schedule, starts_with("week"))),
    by = join_by(game_date >= week_start, game_date <= week_end)
  ) |> 
  mutate(game_day = ordered(
    weekdays(game_date, abbreviate = TRUE), 
    str_to_title(plotfunctions::move_n_point(bigD::names_wkdays(), -1, "sun"))
  )) |> 
  mutate(game_day_date = paste0(game_day, " (", format(game_date, "%d/%m"), ")")) |> 
  (\(t_df){
    bind_rows(
      t_df,
      inner_join(
        t_df,
        distinct(t_df, week, game_date) |> 
          slice_min(game_date, n = 2, by = week)
      ) |> 
      mutate(week = week - 1)
    )
  })() |> 
  arrange(game_date) |> 
  select(season, week, game_day_date, team, against) |> 
  nest_by(season, week, .keep = TRUE) |> 
  filter(week > 0) |> # Remove NULL and 0 weeks
  mutate(data = list(
    data |> 
      pivot_wider(names_from = game_day_date, values_from = against, values_fn = list) |> 
      left_join(select(df_week_game_count, week, team, contains("games"), -starts_with("week_games")), by = join_by(week, team)) |> 
      select(-season, -week) |> 
      rename_with(\(x) str_to_title(str_replace_all(x, "_", " ")))
  ))

