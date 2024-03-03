

# Libraries ---------------------------------------------------------------

library(DT)
library(here)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)
library(nba.dataRub)
library(tibble)
library(ggplot2)
library(plotly)

# Init files --------------------------------------------------------------

source(here("_proj_useful.R"))


# Server code -------------------------------------------------------------

server <- function(input, output, session) {
  
  # Variables
  cur_date <<- as.Date(str_extract(as.POSIXct(Sys.time(), tz="NZ"), "\\d{4}-\\d{2}-\\d{2}")) - 1
  cur_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$current_season
  prev_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$previous_season
  db_con <<- dh_createCon("postgres")
  
  # Data Frames
  source(here("data", "base_frames.R"))
  .load_datasets <- function() walk(list.files(here("data", "app_data_prep"), full.names = TRUE), \(x) source(x, local = TRUE))
  .load_datasets()
  
  # Extra variable that relies on datasets
  cur_week <<- df_week_game_count |>
    mutate(week_end = if_else(week_end - week_start < 6, week_start + 6, week_end)) |>
    filter(cur_date >= week_start, cur_date <= week_end) |>
    pull(season_week) |>
    unique()
  
  # Maybe get to the point where I place free agents at top of list
  active_players <<- as.tibble(df_player_log) |> 
    summarise(avg_min = median(min), .by = c(player_name, free_agent_status)) |> 
    arrange(desc(avg_min)) |>
    pull(player_name)
    
  observe({
    # Player overview tab
    # t_df <- df_player_log
    # if(input$overview_free_agent_filter) t_df <- filter(t_df, free_agent_status == "ACTIVE")
    # if(input$this_season_overview_switch) t_df <- filter(t_df, slug_season == cur_season)
    # min_range <- if(input$tot_avg_toggle) round(quantile(summarise(group_by(t_df, player_id), min = sum(min, na.rm = TRUE))$min))
    #   else round(quantile(summarise(group_by(t_df, player_id), min = mean(min, na.rm = TRUE))$min))
    # updateSliderTextInput(session, "overview_minute_filter", choices = seq(from = min_range[["100%"]], to = min_range[["0%"]]), selected = min_range[["75%"]])
    
    # Player performance tab
    # updateSelectizeInput(session, "performance_select_player", choices = active_players, server = TRUE)
    # updatePickerInput(session, "team_filter", choices = sort(unique(df_player_log$team_slug)))
    
    # Player trend tab
    updateSelectInput(session, "trend_select_player", choices = active_players)
    
    # H2H tab
    # updateSelectInput(session, "h2h_competitor", choices = unique(df_fty_schedule$competitor_name), selected = "senor_cactus")
    # updateSelectInput(session, "h2h_week", choices = unique(df_nba_schedule$season_week), selected = cur_week)
  })
  

# NBA Player Trend --------------------------------------------------------

  output$player_trend_plot <- plotly::renderPlotly({
    
    trend_selected_stat <- sym(filter(stat_selection, formatted_name == input$trend_select_stat)$database_name)
    
    if(is.null(input$trend_select_player)){
      plt <- ggplot() +
        theme_void() +
        geom_text(aes(x = 0, y = 0, label = "Select players"))
      
      ggplotly(plt)
    } else {
      
      df_trend <- 
        as.tibble(
          if(!input$this_season_trend_switch) df_player_log
            else filter(df_player_log, slug_season == cur_season)
        ) |> 
        filter(player_name %in% input$trend_select_player) |>
        arrange(game_date) |> 
        mutate(
          day_sequence = as.integer((game_date - min(game_date)) + 1),
          smooth = loess(replace_na({{ trend_selected_stat }}, 0) ~ day_sequence)$fitted, 
          .by = c(year_season, player_name)
        ) |> 
        (\(df) bind_rows(df, summarise(df, game_date = max(game_date) + 1, .by = c(player_name, year_season))))() |> 
        mutate(player_name = factor(player_name, input$trend_select_player))
      
      plt <- ggplot(df_trend, aes(x = game_date, colour = player_name)) +
        geom_point(aes(y = {{ trend_selected_stat }}), alpha = 0.2) +
        geom_line(aes(y = smooth)) +
        scale_x_date(name = NULL, breaks = df_season_segments$mid_date, labels = df_season_segments$year_season_type) +
        geom_vline(xintercept = as.numeric(df_season_segments$begin_date), colour = "grey") +
        ylim(0, NA) +
        labs(title = input$trend_select_stat, x = NULL, y = NULL) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
      
        ggplotly(plt)
    }
  })


# NBA Schedule Table ------------------------------------------------------

  # Drop box choices
  ss_week <- as.tibble(select(df_nba_schedule, season_week, week_start, week_end))
  week_drop_box_choices <- unique(paste0("Week: ", ss_week$season_week, " (", ss_week$week_start, " to ", ss_week$week_end, ")"))
  
  # Update drop box values
  observe({
    updateSelectInput(
      session, 
      "week_selection", 
      choices = week_drop_box_choices,
      selected = week_drop_box_choices[
        as.tibble(df_nba_schedule) |> 
          distinct(pick(contains("week"))) |> 
          filter(season_week == cur_week) |>
          pull(season_week) + 2 # plus for correct index
      ]
    )
  })  

  output$schedule_table <- renderDT({

    # Prepare tables to be presented
    tbl_schedule <<- tbl_week_games$data[[match(input$week_selection, week_drop_box_choices)]] |>
      mutate(across(ends_with(")"), \(x) as.factor(if_else(as.character(x) == "NULL", ".", "1")))) |>
      mutate(across(c(contains("games"), Team), as.factor))

    ts_names <- str_subset(colnames(tbl_schedule), "\\(")
    names(ts_names) <- str_sub(ts_names, end = 3)
    names(ts_names)[length(ts_names)] <- paste0("2_", names(ts_names)[length(ts_names)])
    names(ts_names)[length(ts_names)-1] <- paste0("2_", names(ts_names)[length(ts_names)-1])
    ts_names <- discard(ts_names[c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "2_Mon", "2_Tue")], is.na)
    tbl_schedule <- select(tbl_schedule, Team, contains("games"), all_of(as.vector(ts_names)))

    # Present table
    DT::datatable(
      tbl_schedule,
      rownames = FALSE,
      class = "cell-border stripe",
      options = list(paging = FALSE, autoWidth = TRUE, dom = 't', scrollX = TRUE),
      filter = list(position = "top", clear = FALSE),
    ) |> 
    formatStyle(columns = "Team", backgroundColor = "lightblue") |>
    (\(tb){
      lvl <- 0:length(unique(tbl_schedule$`Week Games Remaining`))
      col <- c("white", rev(RColorBrewer::brewer.pal(5, "Greens")))[lvl + 1]
      formatStyle(tb, columns = "Week Games Remaining", backgroundColor = styleEqual(levels = lvl, values = col))
    })() |>
    formatStyle(
      columns = "Following Week Games",
      backgroundColor = styleEqual(levels = 0:tail(levels(tbl_schedule$`Following Week Games`), 1), values = rev(RColorBrewer::brewer.pal(length(0:tail(levels(tbl_schedule$`Following Week Games`), 1)), "Greens")))
    ) |> 
    formatStyle(columns = (length(tbl_schedule)-1):length(tbl_schedule), backgroundColor = "lightgrey")
    
  })
  
}
