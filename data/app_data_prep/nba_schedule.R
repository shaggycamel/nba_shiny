

# Calculate games left this week variable
df_week_game_count <<- df_nba_schedule |> 
  mutate(week_games_remaining = game_date >= cur_date) |> 
  summarise(
    week_games_remaining = sum(week_games_remaining), 
    week_games = n(), 
    .by = c(fty_matchup_week, week_start, week_end, team)
  ) |> (\(t_df) {
    left_join(
      t_df,
      select(t_df, team, next_week = fty_matchup_week, following_week_games = week_games),
      join_by(team, closest(fty_matchup_week < next_week))
    ) |> 
    select(-next_week)
  })()


tbl_week_games <<- df_nba_schedule |> 
  mutate(game_day = ordered(
    weekdays(game_date, abbreviate = TRUE), 
    str_to_title(plotfunctions::move_n_point(bigD::names_wkdays(), -1, "sun"))
  )) |> 
  mutate(game_day_date = paste0(game_day, " (", format(game_date, "%d/%m"), ")")) |> 
  (\(t_df){
    bind_rows(
      t_df,
      filter(t_df, game_day %in% c("Mon", "Tue")) |> 
        mutate(fty_matchup_week = fty_matchup_week - 1)
    )
  })() |> 
  arrange(game_date) |> 
  select(slug_season, fty_matchup_week, game_day_date, team, against) |> 
  nest_by(slug_season, fty_matchup_week, .keep = TRUE) |> 
  filter(fty_matchup_week > 0) |>  # Remove NULL and 0 weeks
  mutate(data = list(
    data |> 
      pivot_wider(names_from = game_day_date, values_from = against, values_fn = list) |> 
      left_join(select(df_week_game_count, fty_matchup_week, team, contains("games"), -starts_with("week_games")), by = join_by(fty_matchup_week, team)) |> 
      select(-slug_season, -fty_matchup_week) |> 
      rename_with(\(x) str_to_title(str_replace_all(x, "_", " ")))
  ))

