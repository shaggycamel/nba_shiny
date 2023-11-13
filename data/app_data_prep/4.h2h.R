
df_roster <- dh_getQuery(db_con, "roster.sql") 

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


mutate(df_roster, eq = force_tz(timestamp, tzone = "America/Los_Angeles")) |> View("<")

# past --------------------------------------------------------------------

df_past_pre <- df_roster |> 
  mutate(us_date = with_tz(timestamp, tzone = "EST")) |> 
  filter(us_date < cur_date) |> 
  select(
    us_date,
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
  mutate(ts = format(us_date, "%H:%M"), us_date = as.Date(us_date), dow = lubridate::wday(us_date, week_start = 1), .after = us_date) |>
  # mutate(league_week = if_else(dow == 7, league_week - 1, league_week)) |> 
  mutate(origin = "past") |> 
  slice_max(ts, by = c(us_date, competitor_id)) |> 
  select(-ts)


# future ------------------------------------------------------------------

# This is where ammendment to player roles needs to take place
df_future_pre <- select(
  df_roster,
  us_date = timestamp,
  competitor_id,
  competitor_name,
  player_fantasy_id,
  nba_id,
  player_name,
  player_team,
  player_injury_status
) |> 
  mutate(ts = format(us_date, "%H:%M"), us_date = with_tz(us_date, tzone = "EST"), .after = us_date) |>
  mutate(origin = "future") |> 
  slice_max(paste(as.Date(us_date), ts), by = competitor_id) |> 
  select(-c(ts, us_date)) |> 
  left_join(
    select(filter(df_schedule, game_date >= cur_date), us_date=game_date, team, season_week), # dow
    by = join_by(player_team == team),
    relationship = "many-to-many"
  ) |> 
  mutate(dow = lubridate::wday(us_date, week_start = 1))
  
  
  


# Bring together ----------------------------------------------------------

df_past <- left_join(
  df_past_pre, 
  select(filter(df_rolling_schedule, game_date < cur_date), -c(team, playing)),
  by = join_by(player_fantasy_id == fty_id, us_date == game_date)
) |> 
  select(-nba_id)


df_future <- left_join(
  df_future_pre,
  select(slice_max(df_rolling, game_date, by = player_id), player_id, any_of(anl_cols$stat_cols)),
  by = join_by(nba_id == player_id),
  relationship = "many-to-many"
) |> 
  left_join(
    dh_getQuery(db_con, "SELECT week, competitor_id, opponent_id, opponent_name FROM fty.league_schedule"),
    by = join_by(competitor_id, season_week == week) 
  ) |> 
  rename(league_week = season_week, player_id = nba_id) |> 
  select(all_of(colnames(df_past)))

df_h2h <<- bind_rows(df_past, df_future)