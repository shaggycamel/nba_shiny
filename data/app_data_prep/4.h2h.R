
df_h2h <<- dh_getQuery(db_con, "h2h.sql") |> 
    mutate(league_week = if_else(origin == "past" & dow == 7, league_week - 1, league_week)) |> 
    left_join(
      dh_getQuery(db_con, "SELECT week, competitor_id, opponent_id, opponent_name FROM fty.league_schedule WHERE season = '{cur_season}'"),
      by = join_by(league_week == week, competitor_id)
    ) |> 
    mutate(playing = case_when(
      playing == 1 & player_injury_status == "OUT" ~ "1*",
      playing == 1 ~ "1",
      .default = NA_character_
    ))

############### NEW


# rolling stats -----------------------------------------------------------

df_rolling <- select(df_player_log, player_id, fty_id, game_id, game_date) |> 
  mutate(game_date_15 = (game_date - 15)) |> 
  left_join({
    filter(df_player_log, slug_season >= prev_season) |> 
      select(player_id, roll_date=game_date, any_of(anl_cols$stat_cols))
    },
    by = join_by(player_id, game_date > roll_date, game_date_15 <= roll_date)
  ) |> 
  summarise(
    across(any_of(anl_cols$stat_cols), \(x) coalesce(mean(x, na.rm = TRUE), 0)),
    .by = c(player_id, fty_id, game_date)
  )

df_rolling_schedule <- select(df_schedule, game_date, team, playing) |> 
  left_join(
    slice_max(df_player_log, order_by = game_date, by = player_id) |> 
      select(player_id, team_slug),
    by = join_by(team == team_slug),
    relationship = "many-to-many"
  ) |>
  left_join(df_rolling)



# past --------------------------------------------------------------------

df_past_pre <- dh_getQuery(db_con, "roster.sql") |>
  filter(as.Date(timestamp) < cur_date) |> 
  select(
    us_date = timestamp,
    league_week,
    competitor_id,
    competitor_name,
    player_fantasy_id,
    nba_id,
    player_name,
    player_team,
    player_injury_status,
    opponent_id,
    opponent_name
  ) |> 
  mutate(ts = format(us_date, "%H:%M"), us_date = as.Date(us_date) - 1, dow = lubridate::wday(us_date, week_start = 1), .after = us_date) |>
  mutate(league_week = if_else(dow == 7, league_week - 1, league_week)) |> 
  mutate(origin = "past") |> 
  slice_max(ts, by = c(us_date, competitor_id)) |> 
  select(-ts)


# future ------------------------------------------------------------------

df_future_pre <- slice_max(df_past, us_date, by = player_id) |> 
  mutate(origin = "future")


# Bring together ----------------------------------------------------------

df_past <- left_join(
  df_past_pre, 
  select(filter(df_rolling_schedule, game_date < cur_date), -c(team, playing)),
  by = join_by(player_fantasy_id == fty_id, us_date == game_date)
) |> 
  select(-nba_id)


# This is where ammendment to player roles needs to take place
# NEED TO FIX DOW, LEAGUE WEEK AND COMPETITOR DATA
df_future <- left_join(
  select(filter(df_rolling_schedule, game_date >= cur_date), us_date=game_date, player_id),
  select(df_future_pre, -us_date),
  by = join_by(player_id),
  relationship = "many-to-many"
)


