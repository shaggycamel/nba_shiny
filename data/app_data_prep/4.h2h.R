
df_roster <- dh_getQuery(db_con, "roster.sql") 

# rolling stats -----------------------------------------------------------

df_rolling <- df_player_log |> 
  arrange(game_date) |> 
  mutate(across(any_of(anl_cols$stat_cols), \(x) slider::slide_period_dbl(x, game_date, "day", ~ mean(.x, na.rm = TRUE), .before = 15, .after = -1)), .by = player_id) |> 
  mutate(across(any_of(anl_cols$stat_cols), \(x) coalesce(x, 0))) |> 
  select(-c(slug_season, year_season, season_type, year_season_type, team_slug, free_agent_status, game_id, wl)) # cols not needed
  
df_rolling_schedule <- select(df_schedule, game_date, team, playing) |> 
  left_join(
    slice_max(df_player_log, order_by = game_date, by = player_id) |> 
      select(player_id, team_slug),
    by = join_by(team == team_slug),
    relationship = "many-to-many"
  ) |>
  left_join(df_rolling)


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
  mutate(origin = "past") |> 
  slice_max(ts, by = c(us_date, competitor_id)) |> 
  select(-ts)


df_past <- df_past_pre |> 
  left_join(
    select(filter(df_rolling_schedule, game_date < cur_date), -c(team, playing)),
    by = join_by(player_name, player_fantasy_id == fty_id, us_date == game_date)
  ) |> 
  select(-nba_id)


# future ------------------------------------------------------------------

# This is where ammendment to player roles needs to take place
df_future_pre_prepare <- function(competitor=NULL, exclude=NULL, add=NULL){
  
  df_t <- filter(df_roster, !player_name %in% exclude)
  
  if(!is.null(add)) df_t <- df_t |> 
      bind_rows({
        t_id <- unique(filter(df_roster, competitor_name == competitor)$competitor_id)
        
        t <- slice_max(filter(df_player_log, player_name %in% add), game_date, by = player_id) |> 
          select(player_fantasy_id=fty_id, nba_id=player_id, player_name, player_team=team_slug) |> 
          mutate(player_injury_status="ACTIVE", origin="future") |> 
          mutate(competitor_id=t_id, competitor_name=competitor, .before = everything())
      })
  
  select(
    df_t,
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
  
}

df_future_pre <- df_future_pre_prepare()

df_future <- df_future_pre |> 
  left_join(
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
  

# Bring together ----------------------------------------------------------

df_h2h <<- bind_rows(df_past, df_future)
