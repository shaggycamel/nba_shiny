
# Libraries ---------------------------------------------------------------

library(rlang)
library(dplyr)
library(tidyr)
library(tidyselect)
library(purrr)
library(ggplot2)
library(plotly)
library(stringr)
library(gt)
library(here)
library(nba.dataRub)
library(shinycssloaders)


# Initialisation files ----------------------------------------------------

source(here("_proj_useful.R"))
if(Sys.info()["user"] == "shiny") source(here("_proj_python.R")) # only init python if running in shiny


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

# Constants & Datasets ----------------------------------------------------

  data_collection_caption <- "Collecting data, give it a minute (literally)."
  showPageSpinner(type = 6, caption = data_collection_caption)
  
  # Variables
  prev_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$previous_season
  cur_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$current_season
  cur_date <<- as.POSIXct(Sys.time(), tz="America/New_York")
  db_con <- if(Sys.info()["nodename"] == "Olivers-MacBook-Pro.local") dh_createCon("postgres") else dh_createCon("cockroach") 
  
  # Datasets
  .load_datasets <- function(){
    df_player_log <<- dh_getQuery(db_con, "player_log.sql") |> 
      mutate(slug_season = ordered(slug_season)) |> 
      mutate(season_type = ordered(season_type, c("Pre Season", "Regular Season", "Playoffs"))) |> 
      mutate(year_season_type = forcats::fct_cross(season_type, str_sub(slug_season, start = 6), sep=" "))
  
    df_schedule <<- dh_getQuery(db_con, "league_schedule.sql") |> 
      group_by(slug_season) |> 
      mutate(season_week = if_else(season_week < 30, season_week + 52, season_week)) |> 
      (\(t_df) {
        mutate(t_df, season_week = case_when(
          type_season == "Pre Season" ~ 0,
          type_season == "Regular Season" ~ season_week - max(filter(t_df, type_season == "Pre Season")$season_week)
        ))
      })() |> 
      group_by(season_week) |> 
      mutate(week_start = min(game_date), week_end = max(game_date)) |> 
      ungroup() |> 
      arrange(season_week)
    
    df_season_segments <<- dh_getQuery(db_con, "season_segments.sql") |> 
      mutate(mid_date = begin_date + (end_date - begin_date) / 2)
    
    df_competitor_roster_avg <<- dh_getQuery(db_con, "competitor_roster_avg.sql") |> 
      pivot_longer(any_of(anl_cols$stat_cols), names_to = "stat") 
  }
  
  .load_datasets()
  hidePageSpinner()
  


# Set Server Side Dynamic Menus -------------------------------------------

  observe({
    # Player overview tab
    min_range <- summarise(group_by(df_player_log, slug_season, player_id), min = sum(min))
    updateSliderTextInput(session, "overview_minute_filter", choices = seq(from = max(min_range$min), to = min(min_range$min)), selected = round(quantile(min_range$min)[["75%"]]))
    
    # Player performance tab
    updateSelectizeInput(session, "performance_select_player", choices = sort(unique(df_player_log$player_name)), server = TRUE)
    
    # Player trend tab
    updateSelectInput(session, "trend_select_player", choices = sort(unique(df_player_log$player_name)))
    
    # H2H tab
    updateSelectInput(session, "h2h_competitor", choices = unique(df_competitor_roster_avg$competitor_name), selected = "senor_cactus")
    updateSelectInput(session, "h2h_week", choices = unique(df_schedule$season_week), selected = unique(filter(df_schedule, week_start <= cur_date, week_end >= cur_date)$season_week))
  })
  

# Head to Head -----------------------------------------------------------

  output$h2h_plot <- renderPlotly({
    
    df_h2h <- group_by(df_competitor_roster_avg, competitor_id, competitor_name, stat) |> 
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
    
    
    h2h_plt <- filter(df_h2h, competitor_name == input$h2h_competitor, week == input$h2h_week) |>
      select(-c(week, ends_with("id"))) |>
      (\(t_df){
        bind_rows(
          select(t_df, name = competitor_name, stat, value = competitor_value, roster = competitor_roster),
          select(t_df, name = opponent_name, stat, value = opponent_value, roster = opponent_roster)
        )
      })()
    
    plt <- ggplot(h2h_plt, aes(x = stat, y = value, fill = name, text = paste(round(value, 2), "\n\n", roster))) +
      geom_col(position = "fill") +
      geom_hline(yintercept = 0.5) +
      labs(title = paste0("Week ", input$h2h_week, ":", str_trim(unique(h2h_plt$name)[1]), " vs ", unique(h2h_plt$name)[2]), x = NULL, y = NULL, fill = NULL) +
      theme_bw()
    
    ggplotly(plt, tooltip = "text")
        
  })
  
  output$game_count_table <- render_gt({
    
    df_h2h_week_game_count <- select(df_schedule, season_week, game_date, team, game_id) |> 
      left_join(
        distinct(select(df_competitor_roster_avg, -c(stat, value))),
        by = join_by(team == player_team),
        relationship = "many-to-many"
      ) |> 
      summarise(
        competitor_game_count = n_distinct(player_name),
        competitor_playing = paste(player_name, collapse = "\n"),
        .by = c(season_week, game_date, competitor_id, competitor_name)
      ) |> 
      left_join(
        distinct(df_h2h, week, competitor_id, opponent_id, opponent_name),
        by = join_by(season_week == week, competitor_id)
      ) |> 
      (\(t_df){
        left_join(
          t_df,
          select(t_df, season_week, game_date, opponent_id = competitor_id, opponent_game_count = competitor_game_count, opponent_playing = competitor_playing),
        )
      })() |> 
      mutate(game_day = lubridate::wday(game_date, label = TRUE, week_start = 1))

    h2h_table_game_count <- filter(df_h2h_week_game_count, competitor_name == input$h2h_competitor, season_week == input$h2h_week) |>
      select(ends_with(c("name", "playing")), game_day) |> 
      arrange(game_day) |> 
      (\(t_df){
        bind_rows(
          select(t_df, name = competitor_name, playing = competitor_playing, game_day),
          select(t_df, name = opponent_name, playing = opponent_playing, game_day)
        )
      })() |> 
      pivot_wider(names_from = game_day, values_from = playing) |> 
      rowwise() |> 
      mutate(Total = sum(c_across(where(is.numeric)))) # NOT WORKING
      
      
      gt(h2h_table_game_count, rowname_col = "name") |> 
        text_transform(\(x) (str_count(x,"\\n") + 1))
  })

    
# Player Overview Analysis ------------------------------------------------
# Uses df_overview
  
  # Code to render plot
  output$player_overview_plot <- renderPlotly({
    
    # This season only filter (uses df_player_log)
    df_overview_plt <- if(!input$this_season_overview_switch) df_player_log
      else filter(df_player_log, slug_season == cur_season)
    
    # Non-injured Free Agent filter (if selected)
    df_overview_plt <- if(!input$overview_free_agent_filter) df_overview_plt
      else filter(df_overview_plt, free_agent_status == "ACTIVE")
    
    # Stat summation
    df_overview_plt <- df_overview_plt |> 
      summarise(across(any_of(anl_cols$stat_cols), \(x) sum(x)), .by = c(player_id, player_name)) |> 
      calc_pcts()
    
    # Minute filter
    df_overview_plt <- filter(df_overview_plt, min >= as.numeric(input$overview_minute_filter))
    
    # Scale by minutes (if selected)
    if(input$overview_scale_by_minutes) df_overview_plt <- mutate(df_overview_plt, across(all_of(stat_selection$database_name), ~ .x / min))
    
    # Create df for plot
    df_overview_plt <- map(stat_selection$database_name, ~ {
      
      col = sym(.x)
      
      # NEED TO SOMEHOW FIGURE TO INCLUE ZSCORE HERE FOR PCT
      if(col == sym("tov")){
        slice_max(df_overview_plt, order_by = min, prop = 0.35) |> 
          select(player_name, {{ col }}) |>
          arrange({{ col }}) |>
          slice_head(n = input$overview_slider_top_n) |> 
          set_names(c("player_name", "value"))
      } else {
        select(df_overview_plt, player_name, {{ col }}) |> 
          arrange(desc({{ col }})) |>
          slice_head(n = input$overview_slider_top_n) |> 
          set_names(c("player_name", "value"))
      }
      
    }) |> 
      set_names(stat_selection$formatted_name) |> 
      bind_rows(.id = "stat") |> 
      mutate(top_cat_count = n(), .by = player_name) |> 
      mutate(top_cats = paste(stat, collapse = ", "), .by = player_name)
    
    # Stat selection and render plot
    plt <- filter(df_overview_plt, stat == input$overview_select_stat) |> 
      ggplot(aes(x = value, y = if(input$overview_select_stat == "Turnovers") reorder(player_name, -value) else reorder(player_name, value), fill = ordered(top_cat_count), text = top_cats)) +
      geom_col() +
      guides(fill = guide_legend(title = "Other Category Count", reverse=TRUE)) +
      labs(title = input$overview_select_stat, x = NULL, y = NULL) +
      theme_bw()

    ggplotly(plt, tooltip = "text") |> 
      reverse_legend_labels()
    
  })
  

# Player Performance ------------------------------------------------------
# Uses df_player_log
  
  # Reactively filter player selection list
  observe({
    
    fa <- unique(filter(df_player_log, free_agent_status == "ACTIVE")$player_name)
    xl_at <- if(length(input$excels_at_filter) > 0)
      filter(df_perf_tab, stringr::str_detect(`Excels At`, paste0(filter(stat_selection, formatted_name %in% input$excels_at_filter)$database_name, collapse = "|")))$Player
    else unique(df_player_log$player_name) # base event, when no xl_at is selected

    chs <- if(!input$performance_free_agent_filter) xl_at
      else intersect(xl_at, fa)

    updateSelectizeInput(session, "performance_select_player", choices = sort(chs), server = TRUE)
  })

  # Count how many events a player excels given the selection
  .calc_xl_at_count <- function(df){
    df$xl_at_count <- 0
    for(category in filter(stat_selection, formatted_name %in% input$excels_at_filter)$database_name){
      df <- mutate(df, xl_at_count = str_detect(`Excels At`, category) + xl_at_count)
    }
    df
  }

  # Build table
  output$player_performance_table <- render_gt({
    
    df_perf_tab <<- df_player_log |> 
      filter(
        game_date <= cur_date, 
        game_date >= cur_date - if_else(input$date_range_switch == "Two Weeks", 15, 30)
      ) |>
      group_by(player_id, player_name) |> 
      summarise(
        across(all_of(stat_selection$database_name), ~ mean(.x)),
        across(c(ftm, fta, fgm, fga), ~ sum(.x)),
        .groups = "drop"
      ) |>
      mutate(pct_ft = ftm / fta, pct_fg = fgm / fga) |>
      mutate(across(where(is.numeric), ~ replace_na(.x, 0L))) |> 
      (\(t_df) {
        left_join(
          t_df,
          {
            select(t_df, player_id, player_name, all_of(stat_selection$database_name), -min) |> 
              mutate(across(any_of(stat_selection$database_name[stat_selection$database_name != "tov"]), ~ round(scales::rescale(.x), 2))) |>
              mutate(tov = round((((tov * -1) - min(tov)) / (max(tov) - min(tov))) + 1, 2)) |>
              pivot_longer(cols = any_of(stat_selection$database_name), names_to = "stat") |>
              (\(t_df) {
                bind_rows(
                  mutate(slice_max(t_df, value, n = 3, by = c(player_id, player_name), with_ties = FALSE), performance = "Excels At") |> filter(value > 0),
                  mutate(slice_min(t_df, value, n = 3, by = c(player_id, player_name)), performance = "Weak At")
                )
              })() |> 
              mutate(stat_value = paste0(stat, " (", value, ")")) |> 
              group_by(player_id, player_name, performance) |> 
              summarise(stat_value = paste(stat_value, collapse = "<br>"), .groups = "drop") |> 
              pivot_wider(names_from = performance, values_from = stat_value)
          },
          by = join_by(player_name, player_id)
        )
      })() |> 
      select(player_name, all_of(stat_selection$database_name), contains("at")) |> 
      arrange(player_name) |> 
      rename(all_of(setNames(stat_selection$database_name, stat_selection$formatted_name)), Player = player_name) |> 
      .calc_xl_at_count()
    
    filter(df_perf_tab, Player %in% input$performance_select_player) |> 
      gt() |> 
      tab_header(title = paste("Player Average Performance Over Last", input$date_range_switch)) |>
      sub_values(values = NA, replacement = 0) |> 
      tab_style(
        style = cell_fill(color = "azure1"),
        locations = cells_body(columns = Player)
      ) |>
      tab_style(
        style = cell_fill(color = "darkolivegreen1"),
        locations = lapply(
          c("Minutes", "3-pointers", "Points", "Field Goal %", "Free Throw %", "Rebounds", "Assists", "Steals", "Blocks", "Turnovers"), 
          \(x) cells_body(columns = !!sym(x), rows = !!sym(x) == if_else(x == "Turnovers", min(!!sym(x)), max(!!sym(x))))
        )
      ) |> 
      (\(gt_t){
        cls <- c("lightblue1", "dodgerblue")
        for(x in 2:3) gt_t <- tab_style(gt_t, style = cell_fill(color = cls[x-1]), locations = cells_body(columns = "Excels At", rows = xl_at_count == x))
        gt_t
      })() |> 
      cols_hide(xl_at_count) |>
      gtExtras::gt_add_divider(columns = everything(), sides = "all", include_labels = TRUE) |> 
      tab_options(column_labels.background.color = "blue") |> 
      fmt_markdown() |>  # render linebreak in excel/weak at cells
      fmt_number(decimals = 2) |> 
      fmt_percent(columns = ends_with("%"))
      
    })


# Player Trend Analysis ---------------------------------------------------
# Uses df_player_log
  
  # Code to render plot
  output$player_trend_plot <- renderPlot({
    
    trend_selected_stat <- sym(filter(stat_selection, formatted_name == input$trend_select_stat)$database_name)
    
    filter(df_player_log, player_name %in% input$trend_select_player) |>
      ggplot(aes(x = game_date, y = {{ trend_selected_stat }}, colour = player_name)) +
      geom_point(alpha = 0.2) +
      geom_line(alpha = 0.2) +
      stat_smooth(na.rm = TRUE, show.legend = FALSE, se = FALSE) +
      scale_x_date(name = NULL, breaks = df_season_segments$mid_date, labels = df_season_segments$year_season_type) +
      geom_vline(xintercept = df_season_segments$begin_date, colour = "grey") +
      ylim(0, NA) +
      labs(title = input$trend_select_stat, x = NULL, y = NULL) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  })
  

# League Game Schedule Analysis -------------------------------------------
# Uses df_schedule 
  
  # Drop box choices
  week_drop_box_choices <- unique(paste0("Week: ", df_schedule$season_week, " (", df_schedule$week_start, " to ", df_schedule$week_end, ")"))
  
  # Update drop box values
  observe({
    updateSelectInput(
      session, 
      "week_selection", 
      choices = week_drop_box_choices,
      selected = week_drop_box_choices[
        distinct(df_schedule, pick(contains("week"))) |>
          filter(week_start <= cur_date, week_end >= cur_date) |>
          pull(season_week) + 1 # plus one because index starts at 1
      ]
    )
  })
  
  
  output$schedule_table <- render_gt({
    
    # Calculate games left this week variable
    week_game_count <- df_schedule |> 
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
        
    # Prepare tables to be presented
    tbl_week_games <- df_schedule |> 
      mutate(game_date = paste0(weekdays(game_date, abbreviate = TRUE), " (", format(game_date, "%m/%d"), ")")) |> 
      select(slug_season, season_week, game_date, team, against) |> 
      nest_by(slug_season, season_week, .keep = TRUE) |> 
      mutate(data = list(
        pivot_wider(data, names_from = game_date, values_from = against, values_fn = list) |> 
        left_join(select(week_game_count, season_week, team, contains("games"),-week_games)) |> 
        select(-slug_season, -season_week) |> 
        arrange(desc(week_games_remaining), team) |> 
        rename_with(~ str_to_title(str_replace_all(.x, "_", " ")))
      ))
    
    # Present table
    tbl_week_games$data[[match(input$week_selection, week_drop_box_choices)]] |> 
      gt() |> 
      tab_header(title = input$week_selection) |>
      sub_missing(missing_text = "") |> 
      tab_style(style = cell_fill(color = "azure1"), locations = cells_body(columns = Team)) |> 
      tab_style(
        style = cell_fill(color = "darkseagreen1"),
        locations = lapply(
          c("Following Week Games", "Week Games Remaining"), 
          \(x) cells_body(columns = !!sym(x), rows = !!sym(x) == max(!!sym(x)))
        )
      ) |> 
      tab_style(
        style = cell_fill(color = "darkseagreen3"),
        locations = lapply(
          c("Following Week Games", "Week Games Remaining"), 
          \(x) cells_body(columns = !!sym(x), rows = !!sym(x) == max(!!sym(x)) - 1)
        )
      ) |> 
      gtExtras::gt_add_divider(columns = everything(), sides = "all", include_labels = TRUE) |> 
      tab_options(column_labels.background.color = "blue")
    
  }, height = "600px")
  


# Update watch list & notepad ---------------------------------------------

  # Update watch list & notepad
  observe({
    updateSelectInput(
      session, 
      "watch_list", 
      selected = dh_getQuery(db_con, "SELECT * FROM fty.watch_list")$nba_name,
      choices = sort(unique(df_player_log$player_name))
    )
    
    # Notepad
    updateTextAreaInput(session, "notepad", value = dh_getQuery(db_con, "SELECT * FROM fty.notepad")$note)
  })
  
  # Write changes to db
  observe({
    # Watch list
    if(nrow(dh_getQuery(db_con, "SELECT * FROM fty.watch_list")) != length(input$watch_list)){
      watched <- paste(input$watch_list, collapse = "', '")
      DBI::dbSendQuery(db_con, "TRUNCATE fty.watch_list")
      DBI::dbSendQuery(db_con, glue::glue(readr::read_file(here("queries", "update_watch_list.sql"))))
    }
    
    # Notepad
    if(input$notepad != dh_getQuery(db_con, "SELECT * FROM fty.notepad")$note){
      note <- input$notepad
      DBI::dbSendQuery(db_con, "TRUNCATE fty.notepad")
      DBI::dbSendQuery(db_con, glue::glue("INSERT INTO fty.notepad (note) SELECT '{note}'"))
    }
  })

}


