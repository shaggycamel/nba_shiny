
df_competitor_roster_avg <<- dh_getQuery(db_con, "competitor_roster_avg.sql") |> 
  pivot_longer(any_of(anl_cols$stat_cols), names_to = "stat")


df_h2h <<- group_by(df_competitor_roster_avg, competitor_id, competitor_name, stat) |> 
  arrange(competitor_id, desc(value)) |> 
  ungroup() |> 
  left_join(
    summarise(df_schedule, game_count = n_distinct(game_id), .by = c(season_week, team)),
    by = join_by(player_team == team),
    relationship = "many-to-many"
  ) |> 
  (\(t_df){
    bind_rows(
      # fg_pct
      filter(t_df, stat %in%  c("fga", "fgm")) |> 
        pivot_wider(names_from = stat, values_from = value) |> 
        mutate(fgm = fgm * game_count, fga = fga * game_count) |> 
        mutate(fg_pct = round(fgm / fga, 3)) |> 
        summarise(
          competitor_roster = paste0(player_name, " ", fg_pct, " (", fgm, "/", fga, ")", collapse = "\n"),
          value = sum(fgm) / sum(fga),
          .by = c(competitor_id, competitor_name, season_week)
        ) |> 
        mutate(stat = "fg_pct"),
      
      #ft_pct
      filter(t_df, stat %in%  c("fta", "ftm")) |> 
        pivot_wider(names_from = stat, values_from = value) |>
        mutate(ftm = ftm * game_count, fta = fta * game_count) |> 
        mutate(ft_pct = round(ftm / fta, 3)) |> 
        summarise(
          competitor_roster = paste0(player_name, " ", ft_pct, " (", ftm, "/", fta, ")", collapse = "\n"),
          value = sum(ftm) / sum(fta),
          .by = c(competitor_id, competitor_name, season_week)
        ) |> 
        mutate(stat = "ft_pct"),
      
      # tov
      filter(t_df, stat == "tov") |> 
        arrange(value) |> 
        mutate(value = value * game_count) |> 
        summarise(
          competitor_roster = paste(player_name, round(value, 3), collapse = "\n"),
          value = sum(value),
          .by = c(competitor_id, competitor_name, season_week, stat)
        ),
      
      # the rest
      filter(t_df, !stat %in% c("fga", "fgm", "fta", "ftm", "tov")) |> 
        mutate(value = value * game_count) |> 
        summarise(
          competitor_roster = paste(player_name, round(value, 3), collapse = "\n"),
          value = sum(value),
          .by = c(competitor_id, competitor_name, season_week, stat)
        )
    )
  })() |> 
  (\(t_df){
    dh_getQuery(db_con, "SELECT week, competitor_id, opponent_id FROM fty.league_schedule WHERE season = '{cur_season}'") |> 
      left_join(rename(t_df, competitor_value = value), by = join_by(competitor_id, week == season_week), relationship = "many-to-many") |> 
      left_join(rename(t_df, opponent_id = competitor_id, opponent_name = competitor_name, opponent_value = value, opponent_roster = competitor_roster), by = join_by(opponent_id, stat, week == season_week))
  })() |> 
  select(week, starts_with("competitor"), stat, starts_with("opponent"))



df_competitor_game_count <<- dh_getQuery(db_con, "competitor_game_count.sql") |> 
  # This is a bandaid...for some reason the query gives different results in R and SQL
  mutate(league_week = if_else(origin == "past" & dow == 7, league_week - 1, league_week)) |> 
  left_join(
    dh_getQuery(db_con, "SELECT week, competitor_id, opponent_id, opponent_name FROM fty.league_schedule WHERE season = '{cur_season}'"),
    by = join_by(league_week == week, competitor_id)
  ) |> 
  mutate(playing = case_when(
    playing == 1 & player_injury_status == "OUT" ~ "1*",
    playing == 1 ~ "1",
    .default = NA_character_
  )) |>
  arrange(us_date, player_team, player_name) |> 
  nest_by(league_week, .keep = TRUE) |> 
  mutate(data = list(pivot_wider(data, id_cols = c(competitor_id, competitor_name, opponent_id, opponent_name, player_team, player_name), names_from = us_date, values_from = playing)))
  
