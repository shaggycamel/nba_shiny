
# rolling stats -----------------------------------------------------------

df_rolling <<- df_player_log |> 
  arrange(game_date) |> 
  mutate(across(any_of(anl_cols$stat_cols), \(x) slider::slide_period_dbl(x, game_date, "day", ~ mean(.x, na.rm = TRUE), .before = 15, .after = -1)), .by = player_id) |> 
  mutate(across(any_of(anl_cols$stat_cols), \(x) coalesce(x, 0))) |> 
  select(-c(slug_season, year_season, season_type, year_season_type, free_agent_status, game_id, wl)) # cols not needed

  
# This is where amendment to player roles needs to take place
df_h2h_prepare <<- function(competitor=NULL, exclude=NULL, add=NULL, from_tomorrow=NULL){
  
  # Add future averages here
  df_rolling_schedule <- df_rolling |> 
    bind_rows(
      slice_max(df_rolling, order_by = game_date, by = player_id) |> 
        select(-game_date) |>
        left_join(
          select(df_schedule, team, game_date), 
          by = join_by(team_slug == team),
          relationship = "many-to-many"
        ) |>
        filter(game_date > cur_date)
    )
  
  
# past --------------------------------------------------------------------
  
  df_past <- df_roster |> 
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
    select(-ts) |> 
    left_join(
      select(filter(df_rolling_schedule, game_date < cur_date), -team_slug),
      by = join_by(player_name, player_fantasy_id == fty_id, us_date == game_date)
    ) |> 
    select(-nba_id)


# Future data prep --------------------------------------------------------
  
  df_future <- if(!is.null(add)){
    df_roster |> 
      bind_rows({
        t_id <- unique(filter(df_roster, competitor_name == competitor)$competitor_id)
        
        slice_max(filter(df_player_log, player_name %in% add), game_date, by = player_id) |> 
          select(player_fantasy_id=fty_id, nba_id=player_id, player_name, player_team=team_slug) |> 
          mutate(player_injury_status="ACTIVE", origin="future") |> 
          mutate(competitor_id=t_id, competitor_name=competitor, .before = everything())
      })
  } else df_roster
  
  df_future <- mutate(df_future, timestamp = replace_na(timestamp, max(timestamp, na.rm = TRUE)))
  
  # FUTURE DATAFRAME --- IMPORTANT PART
  df_future <- select(
    df_future,
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
  mutate(dow = lubridate::wday(us_date, week_start = 1)) |> 
  left_join(
    select(slice_max(df_rolling, game_date, by = player_id), player_id, any_of(anl_cols$stat_cols)),
    by = join_by(nba_id == player_id),
    relationship = "many-to-many"
  ) |> 
  left_join(df_fantasy_schedule, by = join_by(competitor_id, competitor_name, season_week == week)) |> 
  rename(league_week = season_week, player_id = nba_id) |> 
  select(all_of(colnames(df_past)))
  
  # if tomorrow: remove add players from today
  df_future <- if(!is.null(from_tomorrow) && from_tomorrow){
    df_future |> 
      anti_join(filter(df_future, player_name %in% add & us_date == cur_date & competitor_name == competitor)) |> 
      anti_join(filter(df_future, player_name %in% exclude & us_date > cur_date & competitor_name == competitor))
  } else if(!is.null(from_tomorrow) && !from_tomorrow){
    df_future |> 
      anti_join(filter(df_future, player_name %in% exclude & us_date > cur_date & competitor_name == competitor))
  } else df_future
      
  bind_rows(df_past, df_future)
  
}

df_h2h <<- df_h2h_prepare()
