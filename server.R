
# Libraries ---------------------------------------------------------------

library(rlang)
library(dplyr)
library(tidyr)
library(tidyselect)
library(lubridate)
library(purrr)
library(ggplot2)
library(plotly)
library(stringr)
library(gt)
library(DT)
library(here)
library(nba.dataRub)
library(shinycssloaders)


# Initialisation files ----------------------------------------------------

source(here("_proj_useful.R"))
if(Sys.info()["user"] == "shiny") source(here("_proj_python.R")) # only init python if running in shiny


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

# Constants & Datasets ----------------------------------------------------

  # Start loading page
  data_collection_caption <- "Processing data, two minutes..."
  showPageSpinner(type = 6, caption = data_collection_caption)
  
  # Variables
  prev_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$previous_season
  cur_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$current_season
  cur_date <<- as.Date(str_extract(as.POSIXct(Sys.time(), tz="NZ"), "\\d{4}-\\d{2}-\\d{2}")) - 1
  db_con <- if(Sys.info()["nodename"] == "Olivers-MacBook-Pro.local") dh_createCon("postgres") else dh_createCon("cockroach") 
  
  # Creates & updates datasets:
  # df_player_log
  # df_schedule
  # df_season_segments
  # df_competitor_roster_avg
  # df_h2h
  .load_datasets <- function() walk(list.files(here("data", "app_data_prep"), full.names = TRUE), \(x) source(x, local = TRUE))
  .load_datasets()
  
  # Stop loading page
  hidePageSpinner()


# Set Server Side Dynamic Menus -------------------------------------------

  observe({
    # Player overview tab
    t_df <- df_player_log
    if(input$overview_free_agent_filter) t_df <- filter(t_df, free_agent_status == "ACTIVE")
    if(input$this_season_overview_switch) t_df <- filter(t_df, slug_season == cur_season)
    min_range <- if(input$tot_avg_toggle) round(quantile(summarise(group_by(t_df, player_id), min = sum(min, na.rm = TRUE))$min))
      else round(quantile(summarise(group_by(t_df, player_id), min = mean(min, na.rm = TRUE))$min))
    updateSliderTextInput(session, "overview_minute_filter", choices = seq(from = min_range[["100%"]], to = min_range[["0%"]]), selected = min_range[["75%"]])
    
    # Player performance tab
    updateSelectizeInput(session, "performance_select_player", choices = sort(unique(df_player_log$player_name)), server = TRUE)
    updatePickerInput(session, "team_filter", choices = sort(unique(df_player_log$team_slug)))
    
    # Player trend tab
    updateSelectInput(session, "trend_select_player", choices = sort(unique(df_player_log$player_name)))
    
    # H2H tab
    updateSelectInput(session, "h2h_competitor", choices = unique(df_h2h$competitor_name), selected = "senor_cactus")
    updateSelectInput(session, "h2h_week", choices = unique(df_schedule$season_week), selected = unique(filter(df_schedule, week_start <= cur_date, week_end >= cur_date)$season_week))
  })
  


# News Transactions -------------------------------------------------------

  output$news_transactions <- renderDT({
    DT::datatable(
      df_news,
      rownames = FALSE,
      class = "cell-border stripe",
      filter = list(position = "top", clear = FALSE),
      options = list(paging = FALSE, autoWidth = TRUE, dom = 't', scrollX = TRUE)
    ) |> 
    formatStyle(columns = colnames(df_news), background = "white", color = "black")
  })
  

# Head to Head -----------------------------------------------------------

  output$h2h_plot <- renderPlotly({
    
    opp_name <- filter(df_h2h, league_week == input$h2h_week, competitor_name == input$h2h_competitor)$opponent_name[1]
    
    h2h_plt <- bind_rows(
        filter(df_h2h, competitor_name == input$h2h_competitor, league_week == input$h2h_week),
        filter(df_h2h, competitor_name == opp_name, league_week == input$h2h_week)
      ) |> 
      filter(!is.na(player_id)) |> 
      pivot_longer(cols = c(ast, stl, blk, tov, pts, ftm, fta, fgm, fga, fg3_m, reb), names_to = "stat", values_to = "value") |> 
      select(competitor_name, player_name, stat, value) |> 
      summarise(value = sum(value, na.rm = TRUE), .by = c(competitor_name, player_name, stat)) |> 
      (\(t_df){
        bind_rows(
          # fg_pct
          filter(t_df, stat %in%  c("fga", "fgm")) |> 
            pivot_wider(names_from = stat, values_from = value) |> 
            arrange(desc(fgm)) |> 
            mutate(fg_pct = round(fgm / fga, 3)) |> 
            summarise(
              competitor_roster = paste0(player_name, " ", fg_pct, " (", round(fgm, 2), "/", round(fga, 2), ")", collapse = "\n"),
              value = sum(fgm) / sum(fga),
              .by = competitor_name
            ) |> 
            mutate(stat = "fg_pct"),
          
          #ft_pct
          filter(t_df, stat %in%  c("fta", "ftm")) |> 
            pivot_wider(names_from = stat, values_from = value) |>
            arrange(desc(ftm)) |> 
            mutate(ft_pct = round(ftm / fta, 3)) |> 
            summarise(
              competitor_roster = paste0(player_name, " ", ft_pct, " (", round(ftm, 2), "/", round(fta, 2), ")", collapse = "\n"),
              value = sum(ftm) / sum(fta),
              .by = competitor_name
            ) |> 
            mutate(stat = "ft_pct"),
          
          # tov
          filter(t_df, stat == "tov") |> 
            arrange(value) |> 
            summarise(
              competitor_roster = paste(player_name, round(value, 3), collapse = "\n"),
              value = sum(value),
              .by = c(competitor_name, stat)
            ),
          
          # the rest
          filter(t_df, !stat %in% c("fga", "fgm", "fta", "ftm", "tov")) |> 
            arrange(desc(value)) |> 
            summarise(
              competitor_roster = paste(player_name, round(value, 3), collapse = "\n"),
              value = sum(value),
              .by = c(competitor_name, stat)
            )
        )
      })()
    
    plt <- ggplot(h2h_plt, aes(x = stat, y = value, fill = competitor_name, text = paste(round(value, 2), "\n\n", competitor_roster))) +
      geom_col(position = "fill") +
      geom_hline(yintercept = 0.5) +
      labs(title = paste0("Week ", input$h2h_week, ": ", str_trim(input$h2h_competitor), " vs ", str_trim(opp_name), x = NULL, y = NULL, fill = NULL)) +
      theme_bw()
    
    ggplotly(plt, tooltip = "text") |> 
      layout(hovermode = "x")
        
  })
  
  output$game_count_table <- render_gt({
    
    opp_name <- filter(df_h2h, competitor_name == input$h2h_competitor, league_week == input$h2h_week)$opponent_name[1]

    df_h2h_week_game_count <- bind_rows(
        filter(df_h2h, competitor_name == input$h2h_competitor, league_week == input$h2h_week),
        filter(df_h2h, competitor_name == opp_name, league_week == input$h2h_week)
      ) |> 
      arrange(us_date, player_team, player_name) |> 
      mutate(playing = case_when(
        !is.na(player_id) & player_injury_status == "OUT" ~ "1*",
        !is.na(player_id) ~ "1",
        .default = NA_character_
      )) |> 
      pivot_wider(id_cols = c(competitor_id, competitor_name, opponent_id, opponent_name, player_team, player_name), names_from = us_date, values_from = playing) |> 
      (\(df){

        inner_func <- function(x, nm) filter(x, competitor_name == nm) |>
          mutate(player_team = "Total", player_name = str_trim(nm)) |>
          summarise(across(starts_with("20"), \(x) as.character(sum(x == "1", na.rm = TRUE))), .by = c(player_team, player_name))

        bind_rows(
          inner_func(df, opp_name),
          inner_func(df, input$h2h_competitor),
          setNames(as.data.frame(matrix(rep(NA, length(colnames(df))), nrow = 1)), colnames(df)),
          select(filter(df, competitor_name == input$h2h_competitor), starts_with(c("player", "20")))
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

  }, align = "left")

    
# Player Overview Analysis ------------------------------------------------
# Uses df_overview
  
  # Code to render plot
  output$player_overview_plot <- renderPlotly({
    
    # Non-injured Free Agent filter (if selected)
    df_overview <- if(!input$overview_free_agent_filter) df_player_log
      else filter(df_player_log, free_agent_status == "ACTIVE")
    
    # This season only filter (uses df_player_log)
    if(input$this_season_overview_switch) df_overview <- filter(df_overview, slug_season == cur_season)
    
    # Stat calc
    stat_calc <- if(input$tot_avg_toggle) getFunction("sum") else getFunction("mean")
    df_overview <- df_overview |> 
      summarise(across(any_of(anl_cols$stat_cols), \(x) stat_calc(x, na.rm = TRUE)), .by = c(player_id, player_name)) |> 
      calc_z_pcts()
    
    # Minute filter
    df_overview <- filter(df_overview, min >= as.numeric(input$overview_minute_filter))
    
    # Scale by minutes (if selected)
    if(input$overview_scale_by_minutes) df_overview <- mutate(df_overview, across(all_of(stat_selection$database_name), ~ .x / min))
    
    # Create df for plot
    df_overview <- map(str_subset(stat_selection$database_name, "_pct", negate = TRUE), ~ {
      
      col = sym(.x)
      
      if(col == sym("tov")){
        slice_max(df_overview, order_by = min, prop = 0.35) |> 
          select(player_name, {{ col }}) |>
          arrange({{ col }}) |>
          slice_head(n = input$overview_slider_top_n) |>
          set_names(c("player_name", "value"))
      } else {
        select(df_overview, player_name, {{ col }}) |> 
          arrange(desc({{ col }})) |>
          slice_head(n = input$overview_slider_top_n) |>
          set_names(c("player_name", "value"))
      }
      
    }) |> 
      set_names(filter(stat_selection, !str_detect(formatted_name, "%"))$formatted_name) |> 
      bind_rows(.id = "stat") |> 
      mutate(top_cat_count = n(), .by = player_name) |> 
      mutate(top_cats = paste(stat, collapse = ", "), .by = player_name)
    
    # Stat selection and render plot
    plt <- filter(df_overview, stat == input$overview_select_stat) |> 
      ggplot(aes(x = value, y = if(input$overview_select_stat == "Turnovers") reorder(player_name, -value) else reorder(player_name, value), fill = ordered(top_cat_count), text = top_cats)) +
      geom_col() +
      guides(fill = guide_legend(title = "Other Category Count", reverse=TRUE)) +
      labs(title = input$overview_select_stat, x = NULL, y = NULL) +
      theme_bw()

    ggplotly(plt, tooltip = "text") |> 
      reverse_legend_labels()
    
  })
  

# Player Comparison ------------------------------------------------------
# Uses df_player_log
  
  # Reactively filter player selection list
  observe({
    
    fa <- unique(filter(df_player_log, free_agent_status == "ACTIVE")$player_name)
    xl_at <- if(length(input$excels_at_filter) > 0)
      filter(df_perf_tab, stringr::str_detect(`Excels At`, paste0(filter(stat_selection, formatted_name %in% input$excels_at_filter)$database_name, collapse = "|")))$Player
    else unique(df_player_log$player_name) # base event, when no xl_at is selected

    chs <- if(!input$performance_free_agent_filter) xl_at
      else intersect(xl_at, fa)
    
    chs <- if(length(input$team_filter) == 0) chs
      else intersect(filter(df_player_log, team_slug %in% input$team_filter)$player_name, chs)

    updateSelectizeInput(session, "performance_select_player", choices = sort(chs), server = TRUE)
  })

  # Count how many events a player excels given the selection
  .calc_xl_at_count <- function(df){
    df$xl_at_count <- 0
    for(category in filter(stat_selection, formatted_name %in% input$excels_at_filter, !str_detect(formatted_name, "%"))$database_name){
      df <- mutate(df, xl_at_count = str_detect(`Excels At`, category) + xl_at_count)
    }
    df
  }

  # Build table
  output$player_comparison_table <- render_gt({
    
    df_perf_tab <<- df_player_log |> 
      filter(
        game_date <= cur_date, 
        # game_date >= cur_date - days(15)
        game_date >= cur_date - if_else(input$date_range_switch == "Two Weeks", days(15), days(30))
      ) |>
      summarise(across(any_of(anl_cols$stat_cols), ~ mean(.x)), .by = c(player_id, player_name)) |>
      calc_z_pcts() |> 
      select(-ends_with("_pct")) |> 
      mutate(across(where(is.numeric), ~ replace_na(.x, 0L))) |> 
      (\(t_df) {
        left_join(
          t_df,
          {
            select(t_df, player_id, player_name, any_of(stat_selection$database_name), -min) |> 
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
      select(player_name, any_of(stat_selection$database_name), contains("at"), -ends_with("pct")) |> 
      arrange(player_name) |> 
      rename(any_of(setNames(stat_selection$database_name, stat_selection$formatted_name)), Player = player_name) |> 
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
          c("Minutes", "3-pointers", "Points", "Field Goal Z", "Free Throw Z", "Rebounds", "Assists", "Steals", "Blocks", "Turnovers"), 
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
    
    if(is.null(input$trend_select_player)){
      ggplot() +
        theme_void() +
        geom_text(aes(x = 0, y = 0, label = "Select players"))
    } else {
      
      df_trend <- if(!input$this_season_trend_switch) df_player_log
        else filter(df_player_log, slug_season == cur_season) 
      
      df_trend |> 
        filter(player_name %in% input$trend_select_player) |>
        arrange(game_date) |> 
        mutate(
          day_sequence = as.integer((game_date - min(game_date)) + 1),
          smooth = loess(replace_na({{ trend_selected_stat }}, 0) ~ day_sequence)$fitted, 
          .by = c(year_season, player_name)
        ) |> 
        (\(df) bind_rows(df, summarise(df, game_date = max(game_date) + 1, .by = c(player_name, year_season))))() |> 
        ggplot(aes(x = game_date, colour = player_name)) +
        geom_point(aes(y = {{ trend_selected_stat }}), alpha = 0.2) +
        geom_line(aes(y = smooth)) +
        scale_x_date(name = NULL, breaks = df_season_segments$mid_date, labels = df_season_segments$year_season_type) +
        geom_vline(xintercept = df_season_segments$begin_date, colour = "grey") +
        ylim(0, NA) +
        labs(title = input$trend_select_stat, x = NULL, y = NULL) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    }
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
  
  
  output$schedule_table <- renderDT({
    
    # Prepare tables to be presented
    tbl_schedule <- tbl_week_games$data[[match(input$week_selection, week_drop_box_choices)]] |>
      mutate(across(ends_with(")"), \(x) as.factor(if_else(as.character(x) == "NULL", "", "1")))) |>
      mutate(across(c(contains("games"), Team), as.factor))
    
    # Present table
    DT::datatable(
      tbl_schedule,
      # caption = input$week_selection,
      rownames = FALSE,
      class = "cell-border stripe",
      options = list(paging = FALSE, autoWidth = TRUE, dom = 't', scrollX = TRUE),
      filter = list(position = "top", clear = FALSE)
    ) |> 
    formatStyle(columns = "Team", backgroundColor = "lightblue") |> 
    # FIX THIS SECTION: COLOUR SELECTION DOESN"T WORK WITH VECTORS LESS THAN 3 IN LENGTH
    formatStyle(
      columns = "Week Games Remaining",
      backgroundColor = styleEqual(levels = 0:length(unique(tbl_schedule$`Week Games Remaining`)), values = rev(RColorBrewer::brewer.pal(length(0:length(unique(tbl_schedule$`Week Games Remaining`))), "Greens")))
    ) |>
    formatStyle(
      columns = "Following Week Games",
      backgroundColor = styleEqual(levels = 0:tail(levels(tbl_schedule$`Following Week Games`), 1), values = rev(RColorBrewer::brewer.pal(length(0:tail(levels(tbl_schedule$`Following Week Games`), 1)), "Greens")))
    )
    
  })
  


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


