
# rolling stats -----------------------------------------------------------

# Doesn't account for situations where a player is traded.
# Assigns stats to player's most recent team (within one season)
df_rolling_stats <<- df_player_log |> 
  arrange(game_date) |> 
  mutate(across(any_of(anl_cols$stat_cols), \(x) slider::slide_period_dbl(x, game_date, "day", ~ mean(.x, na.rm = TRUE), .before = 15, .after = -1)), .by = player_id) |> 
  mutate(across(any_of(anl_cols$stat_cols), \(x) coalesce(x, 0))) |> 
  select(-c(slug_season, year_season, season_type, year_season_type, free_agent_status, game_id, wl)) |> 
  mutate(origin = "past") |> 
  (\(df_t){
    bind_rows(
      df_t,
      df_nba_schedule |> 
        filter(game_date >= cur_date) |> 
        left_join(
          select(df_nba_roster, player_id, fty_id, player_name=player, team_slug), 
          by = join_by(team == team_slug),
          relationship = "many-to-many"
        ) |> 
        select(player_id, fty_id, player_name, team_slug=team, game_date) |> 
        left_join(
          slice_max(df_t, order_by = game_date, by = player_id) |> 
            select(player_id, any_of(anl_cols$stat_cols)),
          by = join_by(player_id),
          relationship = "many-to-many"
        ) |> 
        mutate(origin = if_else(game_date == cur_date, "today", "future"))
    )
  })()

# PAST
df_past <- df_fty_roster |> 
  mutate(us_date = with_tz(timestamp, tzone = "EST"), .before = timestamp) |> 
  filter(us_date < cur_date) |> 
  mutate(
    ts = format(us_date, "%H:%M"), 
    us_date = as.Date(us_date),
    dow = lubridate::wday(us_date, week_start = 1),
    .after = timestamp
  ) |> 
  slice_max(ts, by = c(us_date, competitor_id)) |> 
  select(-c(ts, timestamp)) |> 
  left_join(
    select(df_nba_schedule, team, game_date, scheduled_to_play),
    by = join_by(player_team == team, us_date == game_date)    
  )


# FUTURE PRE | ADD STEP

competitor = "senor_cactus"
c_id <- unique(filter(df_fty_schedule, competitor_name == competitor)$competitor_id)
add = "Alex Caruso"

df_future_pre <- df_fty_roster |> 
  mutate(
    us_date = as.Date(with_tz(timestamp, tzone = "EST")), 
    ts = format(with_tz(timestamp, tzone = "EST"), "%H:%M"), 
    dow = lubridate::wday(us_date, week_start = 1),
    .after = timestamp
  ) |> 
  slice_max(paste(us_date, ts), by = competitor_id) |> 
  select(-ts) |> 
  select(-c(timestamp, us_date, dow, league_week, starts_with("opponent"))) |> 
  bind_rows(
    filter(df_player_log, player_name %in% add) |>
      slice_max(order_by = game_date) |>
      select(player_id, player_fantasy_id = fty_id, player_name, player_team = team_slug) |> 
      mutate(season = cur_season, competitor_id = c_id, competitor_name = competitor)
  )


# FUTURE
df_future <- left_join(
    df_future_pre,
    filter(df_nba_schedule, game_date >= cur_date) |> 
      select(game_date, season_week, team),
    by = join_by(player_team == team),
    relationship = "many-to-many"
  ) |> 
  left_join(
    select(df_fty_schedule, -c(season, league_id)),
    by = join_by(competitor_id, competitor_name, season_week == week),
    relationship = "many-to-many"
  ) |> 
  rename(us_date = game_date, league_week = season_week) |> 
  mutate(dow = lubridate::wday(us_date, week_start = 1)) |> 
  left_join(
    select(df_nba_schedule, team, game_date, scheduled_to_play),
    by = join_by(player_team == team, us_date == game_date)    
  ) |> 
  select(all_of(colnames(df_past)))




# FILTRATION HERE!!!!
exclude = "Ausar Thompson"
from_tomorrow = TRUE

df_h2h <- bind_rows(df_past, df_future) |> 
  left_join(
    select(df_rolling_stats, -c(fty_id, player_name, team_slug)),
    by = join_by(player_id, us_date == game_date)
  )
 

if(from_tomorrow){
  df_h2h <- df_h2h |> 
    anti_join(
      filter(df_h2h, competitor_id == c_id, player_name %in% add, origin == "today"),
      by = join_by(competitor_id, player_id, us_date)
    ) |> 
    anti_join(
      filter(df_h2h, competitor_id == c_id, player_name %in% exclude, origin == "future"),
      by = join_by(competitor_id, player_id, us_date)
    )
} else {
  df_h2h <- df_h2h |> 
    anti_join(
      filter(df_h2h, competitor_id == c_id, player_name %in% exclude, origin != "past"),
      by = join_by(competitor_id, player_id, us_date)
    )
}

  
df_h = df_h2h

df_h2h_week_game_count <- bind_rows(
    filter(df_h, competitor_name == competitor, league_week == 4),
    filter(df_h, competitor_name == opp_name, league_week == 4)
  ) |> 
  mutate(play_status = case_when(
    scheduled_to_play == 1 & player_injury_status == "OUT" ~ "1*",
     scheduled_to_play == 1 ~ "1",
    .default = NA_character_
  )) |> 
  # UPTO HERE
  pivot_wider(id_cols = c(competitor_id, competitor_name, opponent_id, opponent_name, player_team, player_name), names_from = us_date, values_from = play_status) |> 
  (\(df){

    inner_func <- function(x, nm) filter(x, competitor_name == nm) |>
      mutate(player_team = "Total", player_name = str_trim(nm)) |>
      summarise(across(starts_with("20"), \(x) as.character(sum(x == "1", na.rm = TRUE))), .by = c(player_team, player_name))

    bind_rows(
      inner_func(df, opp_name),
      inner_func(df, input$h2h_competitor),
      setNames(as.data.frame(matrix(rep(NA, length(colnames(df))), nrow = 1)), colnames(df)),
      select(filter(df, competitor_name == input$h2h_competitor), starts_with(c("player", "20"))) |> 
        arrange(player_team, player_name)
    )
  })() |>
  select(-starts_with(c("competitor", "opponent"))) |>
  (\(df){
    Ttl = as.data.frame(t(df)) |>
      mutate(across(everything(), \(x) ifelse(is.na(as.numeric(x)) | as.numeric(x) <= 10, as.numeric(x), 10))) |>
      summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |>
      t()

    mutate(df, Total = Ttl)
  })() |>
  mutate(Total = if_else(Total == 0 & is.na(player_team), NA, Total)) |>
  mutate(season_week = as.numeric(input$h2h_week)) |>
  left_join(
    select(df_week_game_count, season_week, team, following_week_games),
    by = join_by(player_team == team, season_week)
  ) |>
  select(-season_week, next_week = following_week_games)
  df_h2h_week_game_count <- select(df_h2h_week_game_count, starts_with("player"), all_of(sort(str_subset(colnames(df_h2h_week_game_count), "\\d"))), Total, next_week)
  
  gt(df_h2h_week_game_count, rowname_col = "info") |>
    sub_missing(missing_text = "") |>
    (\(t){
      if(any(str_detect(colnames(df_h2h_week_game_count), as.character(cur_date))))
        tab_style(
          t,
          style = list(cell_fill(color = "lightblue1"), cell_text(weight = "bold"), cell_borders(sides = c("left", "right"))),
          locations = cells_body(columns = as.character(cur_date))
        )
      else t
    })() |>
    tab_style_body(
      style = cell_fill(color = "pink", alpha = 0.5),
      columns = starts_with("20"),
      fn = \(x) str_detect(x, "\\*") | as.numeric(x) > 10
    ) |>
    tab_style(style = cell_fill(color = "pink", alpha = 0.5), locations = cells_body(columns = next_week, rows = next_week < 3)) |> 
    tab_style(style = cell_borders(sides = c("left", "right")), locations = cells_body(columns = c(starts_with("20"), Total))) |>
    tab_style(
      style = list(cell_text(weight = "bold"), cell_borders(sides = c("left", "right"))),
      locations = cells_body(columns = Total)
    ) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = player_name, rows = player_name == input$h2h_competitor)) |>
    tab_style(
      style = cell_fill(color = "lightgreen"),
      locations = cells_body(columns = Total, rows = Total == max(Total, na.rm = TRUE))
    ) |>
    tab_style(
      style = list(cell_fill(color = "grey", alpha = 0.5), cell_borders(sides = c("top", "bottom"))),
      locations = cells_body(rows = 3)
    ) |>
    tab_style(style = cell_text(align = "center"), locations = cells_body(c(starts_with("20"), Total))) |>
    cols_label_with(columns = starts_with("20"), fn = \(x) weekdays(as.Date(x))) |>
    tab_options(column_labels.background.color = "blue")
