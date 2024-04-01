

# Libraries ---------------------------------------------------------------

library(nba.dataRub)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)
library(ggplot2)

library(magrittr) # DELETE


# Server code -------------------------------------------------------------

server <- function(input, output, session) {

  # Variables
  # cur_date <<- as.Date(str_extract(as.POSIXct(Sys.time(), tz="NZ"), "\\d{4}-\\d{2}-\\d{2}")) - 1
  cur_date <<- as.Date("2024-02-26")
  cur_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$current_season
  prev_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$previous_season
  db_con <<- dh_createCon("postgres")
  
  # Data Frames
  source(here("data", "base_frames.R"))
  .load_datasets <- function() walk(list.files(here("data", "app_data_prep"), full.names = TRUE), \(x) source(x, local = TRUE))
  .load_datasets()
  
  # Extra variable that relies on datasets
  # cur_week <<- df_week_game_count |>
  #   mutate(week_end = if_else(week_end - week_start < 6, week_start + 6, week_end)) |>
  #   filter(cur_date >= week_start, cur_date <= week_end) |>
  #   pull(season_week) |>
  #   unique()
  cur_week <<- 18
  
  teams <<- df_player_log |> 
    pull(team_slug) |> 
    unique() |> 
    sort()
  
  sort_players_by_min_desc <- function(df) {
    summarise(df, avg_min = median(min), .by = player_name) |> 
      arrange(desc(avg_min)) |>
      pull(player_name)
  }
  
  # Maybe get to the point where I place free agents at top of list
  active_players <<- sort_players_by_min_desc(df_player_log)
  free_agents <<- sort_players_by_min_desc(filter(df_player_log, free_agent_status == "ACTIVE"))
  

  observe({
    # Draft Assistance tab
    min_range <- if(input$draft_tot_avg_toggle) round(quantile(summarise(group_by(df_player_log, player_id), min = sum(min, na.rm = TRUE))$min))
      else round(quantile(summarise(group_by(df_player_log, player_id), min = mean(min, na.rm = TRUE))$min))
    updateSliderTextInput(session, "draft_min_filter", choices = seq(from = min_range[["100%"]], to = min_range[["0%"]]), selected = min_range[["75%"]])
    
    # Player Comparison tab
    updatePickerInput(session, "comparison_team_filter", choices = teams)
    
    # Player trend tab
    updateSelectInput(session, "trend_select_player", choices = active_players)
    
    # H2H tab
    updateSelectInput(session, "h2h_competitor", choices = unique(df_fty_schedule$competitor_name), selected = "senor_cactus")
    updateSelectInput(session, "h2h_week", choices = unique(df_nba_schedule$season_week), selected = cur_week)
  })
  
  # Additional H2H filter alteration
  observe({
    competitor_players <- sort(filter(slice_max(df_h2h_og, us_date, by = competitor_id), competitor_name == input$h2h_competitor)$player_name)
    updateSelectInput(session, "h2h_ex_player", choices = competitor_players)
    updateSelectInput(session, "h2h_add_player", choices = setdiff(active_players, competitor_players))
    updateSelectInput(session, "h2h_hl_player", choices = competitor_players)
  })
  

# FTY League Overview -------------------------------------------------


  

# FTY Head to Head --------------------------------------------------------

  # Reactive H2H data creation
  df_h2h <- reactive(df_h2h_prepare(input$h2h_competitor, input$h2h_ex_player, input$h2h_add_player, input$h2h_future_from_tomorrow)) |> 
    bindEvent(input$h2h_competitor, input$h2h_ex_player, input$h2h_add_player, input$h2h_future_from_tomorrow)
  
  output$h2h_stat_plot <- renderPlotly({
    
    if(input$h2h_week < cur_week & input$h2h_future_only){
      plt <- ggplot() +
        theme_void() +
        geom_text(aes(x = 0, y = 0, label = "Future only dumbass..."))
      ggplotly(plt)
    } else {
      
      df_h <- df_h2h()
      if(input$h2h_future_only) df_h <- filter(df_h, origin == "future")
      opp_name <- filter(df_h, league_week == input$h2h_week, competitor_name == input$h2h_competitor)$opponent_name[1]

      h2h_plt <<- bind_rows(
        filter(df_h, competitor_name == input$h2h_competitor, league_week == input$h2h_week),
        filter(df_h, competitor_name == opp_name, league_week == input$h2h_week)
      ) |>
      filter(!is.na(player_id), player_injury_status %in% c("ACTIVE", "DAY_TO_DAY") | is.na(player_injury_status)) |>
      pivot_longer(cols = c(ast, stl, blk, tov, pts, ftm, fta, fgm, fga, fg3_m, reb), names_to = "stat", values_to = "value") |>
      select(competitor_name, player_name, stat, value) |>
      summarise(value = sum(value, na.rm = TRUE), .by = c(competitor_name, player_name, stat)) |>
      (\(t_df){
        bind_rows(
          # fg_pct
          filter(t_df, stat %in%  c("fga", "fgm")) |>
            pivot_wider(names_from = stat, values_from = value) |>
            arrange(desc(fgm)) |>
            mutate(fg_pct = round(fgm / fga, 2)) |>
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
            mutate(ft_pct = round(ftm / fta, 2)) |>
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
              competitor_roster = paste(player_name, round(value), collapse = "\n"),
              value = sum(value),
              .by = c(competitor_name, stat)
            ),

          # the rest
          filter(t_df, !stat %in% c("fga", "fgm", "fta", "ftm", "tov")) |>
            arrange(desc(value)) |>
            summarise(
              competitor_roster = paste(player_name, round(value), collapse = "\n"),
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

    }
        
  }) 
  
  
  output$h2h_game_table <- renderDT({
    
    if(input$h2h_week < cur_week & input$h2h_future_only){
  
      datatable(
        as.data.frame("Future only dumbass..."),
        rownames = FALSE,
        colnames = "",
        options = lst(dom = "t", paging = FALSE), 
      )
  
    } else {
  
      df_h <- df_h2h()
      if(input$h2h_future_from_tomorrow) df_h <- mutate(df_h, origin = if_else(us_date == cur_date, "past", origin))
      if(input$h2h_future_only) df_h <- filter(df_h, origin == "future")
      opp_name <- filter(df_h, competitor_name == input$h2h_competitor, league_week == input$h2h_week)$opponent_name[1]
      
      df_h2h_week_game_count <- bind_rows(
        filter(df_h, competitor_name == input$h2h_competitor, league_week == input$h2h_week),
        filter(df_h, competitor_name == opp_name, league_week == input$h2h_week)
      ) |> 
      mutate(play_status = case_when(
        scheduled_to_play == 1 & player_injury_status == "OUT" ~ "1*",
         scheduled_to_play == 1 ~ "1",
        .default = NA_character_
      )) |> 
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
            arrange(player_name) |>
            mutate(across(starts_with("20"), \(x) as.character(x)))
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
      
      df_h2h_week_game_count <- select(df_h2h_week_game_count, starts_with("player"), all_of(sort(str_subset(colnames(df_h2h_week_game_count), "\\d"))), Total, `Next Week` = next_week, Team = player_team, Player = player_name) |> 
        rename_with(.fn = \(x) format(as.Date(x), "%a (%m/%d)"), .cols = starts_with("20"))
      
      max_game_count <- max(df_h2h_week_game_count$Total, na.rm = TRUE)
      tibble::rowid_to_column(df_h2h_week_game_count) |> 
        datatable(
          rownames = FALSE, 
          escape = FALSE,
          style = "default",
          options = lst(
            dom = "t", 
            paging = FALSE, 
            ordering = FALSE,
            columnDefs = list(list(visible = FALSE, targets = "rowid")),
            initComplete = JS(
              "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': 'blue', 'color': 'white'});",
              "}"
            )
          )
        ) |> 
        formatStyle(colnames(df_h2h_week_game_count), border = "1px solid #000") |> 
        formatStyle("rowid", target = "row", backgroundColor = styleEqual(3, "grey")) |> 
        formatStyle("Total", target = "cell", backgroundColor = styleEqual(max_game_count, "lightgreen")) |> 
        (\(dt){
          cols <- str_subset(colnames(df_h2h_week_game_count), "\\(")
          
          for(col in cols){
            dt <- formatStyle(
              dt,
              columns = col,
              target = "cell",
              backgroundColor = styleInterval(10, c(NA, "pink"))
            ) |> 
            formatStyle(
              columns = col, 
              target = "cell",
              backgroundColor = styleEqual("1*", "pink")
            )
          }
          
          if (format(cur_date, "%a (%m/%d)") %in% cols) dt <- formatStyle(dt, format(cur_date, "%a (%m/%d)"), target = "cell", backgroundColor = styleEqual("1*", "pink", default = "lightyellow"))
          if (length(input$h2h_hl_player) > 0) dt <- formatStyle(dt, "Player", target = "row", backgroundColor = styleEqual(input$h2h_hl_player, rep("slategray1", length(input$h2h_hl_player))))

          dt
        })() 

      }

  })
  

# NBA Player Comparison -------------------------------------------------------

  # Count how many events a player excels given the selection
  .calc_xl_at_count <- function(df){
    df$xl_at_count <- 0
    for(category in filter(stat_selection, formatted_name %in% input$comparison_excels_at_filter, !str_detect(formatted_name, "%"))$database_name){
      df <- mutate(df, xl_at_count = str_detect(`Excels At`, category) + xl_at_count)
    }
    df
  } 
  
  output$player_comparison_table <- renderDT({
    
    df_comparison <<- df_player_log |> 
      filter(
        game_date <= cur_date, 
        # game_date >= cur_date - days(15)
        game_date >= cur_date - case_when(input$date_range_switch == "Two Weeks" ~ days(15), input$date_range_switch == "One Month" ~ days(30), .default = days(7))
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
              summarise(stat_value = paste(stat_value, collapse = "\n"), .groups = "drop") |> 
              pivot_wider(names_from = performance, values_from = stat_value)
          },
          by = join_by(player_name, player_id)
        )
      })() |> 
      select(player_name, any_of(stat_selection$database_name), contains("at"), -ends_with("pct")) |> 
      rename(any_of(setNames(stat_selection$database_name, stat_selection$formatted_name)), Player = player_name) |> 
      left_join(
        slice_max(df_player_log, game_date, by = player_name) |> 
          select(Player = player_name, Team = team_slug, free_agent_status),
        by = join_by(Player)
      ) |> 
      mutate(play_status = str_extract(free_agent_status, "[OUT|DAY|SUS].*")) |> 
      mutate(play_status = case_when(
        play_status == "OUT" ~ "(out)",
        play_status == "SUSPENSION" ~ "(sus)",
        play_status == "DAY_TO_DAY" ~ "(d2d)",
        .default = ""
      )) |> 
      mutate(
        Player = paste(Player, play_status),
        player_colour = case_when(
          play_status == "(out)" ~ "red",
          play_status == "(sus)" ~ "pink",
          play_status == "(d2d)" ~ "pink",
          .default = "azure"
        )
      ) |> 
      relocate(Team, .after = Player) |> 
      .calc_xl_at_count()
    
    df_comparison_table <- filter(df_comparison, Minutes >= input$comparison_minute_filter)
    if(input$comparison_free_agent_filter) df_comparison_table <- filter(df_comparison_table, !is.na(free_agent_status))
    if(!is_null(input$comparison_team_filter)) df_comparison_table <- filter(df_comparison_table, Team %in% input$comparison_team_filter)
    if(!is_null(input$comparison_excels_at_filter)) df_comparison_table <- filter(df_comparison_table, str_detect(`Excels At`, paste0(filter(stat_selection, formatted_name %in% input$comparison_excels_at_filter)$database_name, collapse = "|")))
    df_comparison_table <- select(df_comparison_table, -ends_with("_status")) |> 
      arrange(desc(Minutes))
    
    df_comparison_table |> 
      datatable(
        rownames = FALSE, 
        escape = FALSE,
        style = "default",
        options = lst(
          dom = "t",
          paging = FALSE,
          columnDefs = list(list(visible = FALSE, targets = str_which(colnames(df_comparison_table), "_count|_colour") - 1)),
          initComplete = JS(
            "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': 'blue', 'color': 'white'});",
            "}"
          )
        )
      ) |> 
      formatStyle(columns = "Player", valueColumns = "player_colour", backgroundColor = styleEqual(c("red", "pink", "lightblue"), c("red", "pink", "lightblue"))) |> 
      formatStyle(columns = colnames(df_comparison_table), border = "1px solid #000000") |> 
      formatCurrency(c(3:5, 8:12), "", digits=1) |>
      formatCurrency(6:7, "") |>
      formatStyle(
        "Turnovers",
        backgroundColor = styleInterval(min(df_comparison_table$Turnovers) + 0.01, c("lightgreen", "white")),
      ) |>
      (\(dt){
          cols <- c("Minutes", "3-pointers", "Points", "Field Goal Z", "Free Throw Z", "Rebounds", "Assists", "Steals", "Blocks")
          for(col in cols){
            dt <- formatStyle(
              dt,
              columns = col,
              backgroundColor = styleInterval(max(df_comparison_table[[col]]) - 0.01, c('white', 'lightgreen'))
            )
          }
          dt
        })() |> 
        # NOT WORKING !!! ???
        formatStyle(
          columns = "Excels At", 
          valueColumns = "xl_at_count",
          backgroundColor = styleEqual(0:3, c("white", "lightblue1", "dodgerblue", "blue"))
        )
  })


# NBA Schedule Table ------------------------------------------------------
  
  # Drop box choices
  ss_week <- select(df_nba_schedule, season_week, week_start, week_end)
  week_drop_box_choices <- unique(paste0("Week: ", ss_week$season_week, " (", ss_week$week_start, " to ", ss_week$week_end, ")"))
  
  # Update drop box values
  observe({
    updateSelectInput(
      session, 
      "week_selection", 
      choices = week_drop_box_choices,
      selected = week_drop_box_choices[
        df_nba_schedule |> 
          distinct(pick(contains("week"))) |> 
          filter(season_week == cur_week) |>
          pull(season_week) + 1 # plus for correct index
      ]
    )
  })
  
  observeEvent(input$week_selection, {
    
    selected_week_dates <<- as.vector(str_extract_all(input$week_selection, "\\d{4}-\\d{2}-\\d{2}", simplify = TRUE))
    date_input_value <- cur_date
    if(input$pin_date < selected_week_dates[1]) date_input_value <- selected_week_dates[1]
    if(input$pin_date > selected_week_dates[2]) date_input_value <- selected_week_dates[2]
    updateDateInput(session, "pin_date", value = date_input_value, min = selected_week_dates[1], max = selected_week_dates[2])
    
  })

  output$schedule_table <- renderDT({

    # Prepare tables to be presented
    # tbl_schedule <<- tbl_week_games$data[[24]] |>
    tbl_schedule <<- tbl_week_games$data[[match(input$week_selection, week_drop_box_choices)+1]] |>
      mutate(across(ends_with(")"), \(x) if_else(as.character(x) == "NULL", 0, 1))) |>
      mutate(across(c(contains("games"), Team), as.factor))
   
    ts_names <- tmp_names <- str_subset(colnames(tbl_schedule), "\\(")
    names(ts_names) <- names(tmp_names) <- str_sub(ts_names, end = 3)

    # Add condition for where years aren't equal
    for(ix in 1:length(ts_names)){
      nm <- table(names(tmp_names)[1:ix])[names(tmp_names)[ix]]
      names(ts_names)[ix] <- paste0(nm[[1]], "_", names(nm))
      if(as.Date(paste0(year(selected_week_dates[1]), "/", str_extract(tmp_names[ix], "\\d{2}/\\d{2}"))) == selected_week_dates[2]){
        wk_th <- ix
      }
    }
    ts_names <- discard(ts_names[c("1_Mon", "1_Tue", "1_Wed", "1_Thu", "1_Fri", "1_Sat", "1_Sun", "2_Mon", "2_Tue")], is.na)
    
    tbl_schedule <- tbl_schedule |> 
      rowwise() |> 
      mutate(
        `Games From Pin` = sum(c_across(str_subset(ts_names, format(input$pin_date, "%m/%d")):ts_names[wk_th])), 
        .before = "Following Week Games"
      ) |> 
      relocate(contains("games"), .after = wk_th + 1) |> 
      mutate(across(ends_with(")"), \(x) as.factor(if_else(x == 0, ".", "1"))))

    # Present table
    datatable(
      tbl_schedule,
      rownames = FALSE,
      class = "cell-border stripe",
      style = "default",
      options = list(
        paging = FALSE, 
        autoWidth = TRUE, 
        dom = 't', 
        scrollX = TRUE,
        initComplete = JS(
          "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': 'blue', 'color': 'white'});",
          "}"
        )
      ),
      filter = list(position = "top", clear = FALSE),
    ) |> 
    formatStyle(columns = "Team", backgroundColor = "azure") |>
    formatStyle(columns = str_subset(ts_names, format(input$pin_date, "%m/%d")), backgroundColor = "lightyellow") |>
    (\(tb){
      lvl <- 0:length(unique(tbl_schedule$`Games From Pin`))
      col <- c("white", rev(RColorBrewer::brewer.pal(5, "Greens")))[lvl + 1]
      formatStyle(tb, columns = "Games From Pin", backgroundColor = styleEqual(levels = lvl, values = col))
    })() |>
    formatStyle(
      columns = "Following Week Games",
      backgroundColor = styleEqual(levels = 0:tail(levels(tbl_schedule$`Following Week Games`), 1), values = rev(RColorBrewer::brewer.pal(length(0:tail(levels(tbl_schedule$`Following Week Games`), 1)), "Greens")))
    ) |> 
    formatStyle(columns = (length(tbl_schedule)-1):length(tbl_schedule), backgroundColor = "lightgrey")
    
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
      
      df_trend <- (
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


# Draft Assistance --------------------------------------------------------
  
  output$draft_stat_plot <- renderPlotly({

    # Stat calc
    stat_calc <- if(input$draft_tot_avg_toggle) getFunction("sum") else getFunction("mean")
    df_overview <- df_player_log |> 
      filter(
        slug_season == prev_season, 
        !(is.na(player_id) | is.na(player_name))
      ) |>
      summarise(across(any_of(anl_cols$stat_cols), \(x) stat_calc(x, na.rm = TRUE)), .by = c(player_id, player_name)) |>
      calc_z_pcts()

    # Minute filter
    df_overview <- filter(df_overview, min >= as.numeric(input$draft_min_filter))

    # Scale by minutes (if selected)
    if(input$draft_scale_minutes) df_overview <- mutate(df_overview, across(all_of(stat_selection$database_name), ~ .x / min))
    # Create df for plot
    df_overview <- map(str_subset(stat_selection$database_name, "_pct", negate = TRUE), ~ {

      col = sym(.x)

      if(col == sym("tov")){
        slice_max(df_overview, order_by = min, prop = 0.35) |>
          select(player_name, {{ col }}) |>
          arrange({{ col }}) |>
          slice_head(n = input$draft_top_n) |>
          set_names(c("player_name", "value"))
      } else {
        select(df_overview, player_name, {{ col }}) |>
          arrange(desc({{ col }})) |>
          slice_head(n = input$draft_top_n) |>
          set_names(c("player_name", "value"))
      }

    }) |>
      set_names(filter(stat_selection, !str_detect(formatted_name, "%"))$formatted_name) |>
      bind_rows(.id = "stat") |>
      mutate(top_cat_count = n(), .by = player_name) |>
      mutate(top_cats = paste(stat, collapse = ", "), .by = player_name)

    # Stat selection and render plot
    plt <- filter(df_overview, stat == input$draft_stat) |>
      ggplot(aes(x = value, y = if(input$draft_stat == "Turnovers") reorder(player_name, -value) else reorder(player_name, value), fill = ordered(top_cat_count), text = top_cats)) +
      geom_col() +
      guides(fill = guide_legend(title = "Other Category Count", reverse=TRUE)) +
      labs(title = paste0("Previous Seasion (", prev_season, "): ", ifelse(input$draft_tot_avg_toggle, "Total", "Average"), " ", input$draft_stat, ifelse(input$draft_scale_minutes, " Scaled", "")), x = NULL, y = NULL) +
      theme_bw()

    ggplotly(plt, tooltip = "text") |>
      reverse_legend_labels()

  })

  
# News Transactions -------------------------------------------------------

    output$news_transactions <- renderDT(
    datatable(
      df_news,
      rownames = FALSE,
      class = "cell-border stripe",
      style = "default",
      filter = list(position = "top", clear = FALSE),
      options = list(
        paging = FALSE, 
        autoWidth = TRUE, 
        dom = 't', 
        scrollX = TRUE,
        initComplete = JS(
          "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': 'blue', 'color': 'white'});",
          "}"
        )
      )
    ) |> 
    formatStyle(columns = colnames(df_news), background = "white", color = "black")
  )

}


                                   