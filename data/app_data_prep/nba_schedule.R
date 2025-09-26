# Calculate games left this matchup variable
df_matchup_game_count <<- df_nba_schedule |>
  left_join(
    distinct(select(df_fty_schedule, starts_with("matchup"))),
    by = join_by(game_date >= matchup_start, game_date <= matchup_end)
  ) |>
  mutate(matchup_games_remaining = game_date >= cur_date) |>
  summarise(
    matchup_games_remaining = sum(matchup_games_remaining),
    matchup_games = n(),
    .by = c(matchup_period, matchup_start, matchup_end, team)
  ) |>
  (\(t_df) {
    left_join(
      t_df,
      select(t_df, team, next_matchup_period = matchup_period, following_matchup_period_games = matchup_games),
      join_by(team, closest(matchup_period < next_matchup_period))
    ) |>
      select(-next_matchup_period)
  })()


tbl_matchup_games <<- df_nba_schedule |>
  left_join(
    distinct(select(df_fty_schedule, starts_with("matchup"))),
    by = join_by(game_date >= matchup_start, game_date <= matchup_end)
  ) |>
  mutate(
    game_day = ordered(
      weekdays(game_date, abbreviate = TRUE),
      str_to_title(plotfunctions::move_n_point(bigD::names_wkdays(), -1, "sun"))
    )
  ) |>
  mutate(game_day_date = paste0(game_day, " (", format(game_date, "%d/%m"), ")")) |>
  (\(t_df) {
    bind_rows(
      t_df,
      inner_join(
        t_df,
        distinct(t_df, matchup_period, game_date) |>
          slice_min(game_date, n = 2, by = matchup_period)
      ) |>
        mutate(matchup_period = matchup_period - 1)
    )
  })() |>
  arrange(game_date) |>
  select(season, matchup_period, game_day_date, team, against) |>
  nest_by(season, matchup_period, .keep = TRUE) |>
  filter(matchup_period > 0) |> # Remove NULL and 0 matchups
  mutate(
    data = list(
      data |>
        pivot_wider(names_from = game_day_date, values_from = against) |> # additional arg used to be: values_fn = list. dont know why
        left_join(
          select(df_matchup_game_count, matchup_period, team, contains("games"), -starts_with("matchup_games")),
          by = join_by(matchup_period, team)
        ) |>
        select(-season, -matchup_period) |>
        rename_with(\(x) str_to_title(str_replace_all(x, "_", " ")))
    )
  )
