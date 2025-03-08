
# Libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(shinycssloaders)


# Set timezone ------------------------------------------------------------

Sys.setenv(TZ = "EST")

# Initialisation files ----------------------------------------------------

# only init python if running in shiny
if(Sys.info()["user"] == "shiny") source(here("_proj_python.R")) 
source(here("_proj_useful.R"))


# Server code -------------------------------------------------------------

server <- function(input, output, session) {
  
# Variables ---------------------------------------------------------------

  fty_parameters_met <- reactiveVal(FALSE)
  
  db_con <<- if(Sys.info()["user"] == "fred") dh_createCon("postgres") else dh_createCon("cockroach")
  cur_date <<- strptime(Sys.time(), "%Y", tz = "EST")
  cur_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$current_season
  prev_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$previous_season
  df_fty_base <<- readRDS("fty_base.RDS")
  ls_fty_base <<- magrittr::`%$%`(distinct(df_fty_base, platform, league_id, league_name), purrr::map(setNames(paste0(platform, "_", league_id), league_name), \(x) as.vector(x)))

# Login -------------------------------------------------------------------
  
  bindEvent(observe({
    if(input$fty_league_select != "")
      load(here(paste0("fty_", ls_fty_base[input$fty_league_select], ".RData")), envir = globalenv())
      updateSelectInput(inputId = "fty_competitor_select", choices = filter(df_fty_base, league_name == input$fty_league_select)$competitor_name)
      platform_selected <<- str_split(ls_fty_base[input$fty_league_select], "_")[[1]][1]
      league_selected <<- str_split(ls_fty_base[input$fty_league_select], "_")[[1]][2]
  }), input$fty_league_select, ignoreNULL = TRUE)
  
  bindEvent(observe({
    if(input$fty_competitor_select == "" && input$fty_league_select == "")
      output$login_messages <- renderText("Select a league, then select a competitor.")
    else if(input$fty_competitor_select != ""){
      fty_parameters_met(TRUE)
      removeModal()
      output$login_messages <- NULL
    } else
      output$login_messages <- renderText("Select a competitor")
  }), input$fty_dash_init)
  
  bindEvent(observe({
    if(!fty_parameters_met())
      output$login_messages <- renderText("You gotta go fill the form at least once!")
    else {
      removeModal()
      output$login_messages <- NULL
    }
  }), input$fty_abort)
  
  bindEvent(observe(login_modal()), input$fty_league_competitor_switch)
  
  login_modal <- function() {
    showModal(
      modalDialog(
        tags$head(tags$style(HTML(".selectize-dropdown-content{min-width: 100%; box-sizing: border-box;}"))),
        selectizeInput(
          "fty_league_select", 
          label = NULL, 
          choices = unique(df_fty_base$league_name),
          options = list(
            placeholder = "Select Fantasy League", 
            onInitialize = I("function(){this.setValue('');}")
          ),
          width = "100%"
        ),
        selectizeInput(
          "fty_competitor_select", 
          label = NULL, 
          choices = character(0),
          options = list(placeholder = "Select Fantasy Competitor", onInitialize = I("function(){this.setValue('');}")),
          width = "100%"
        ),
        span(textOutput("login_messages"), style="color:red"),
        footer = tagList(
          actionButton("fty_abort", label = NULL, icon = icon("square-xmark"), style="color:#FFF; background-color:#CD3333; border-color:#2E6DA4"), 
          actionButton("fty_dash_init", "Kobeee!", style="color:#FFF; background-color:#337AB7; border-color:#2E6DA4")
        ),
        size = "m"
      )
    )
  }
  login_modal()
  

# Load datasets -----------------------------------------------------------

  observe({
    req(fty_parameters_met())
    
    # Start loading page
    data_collection_caption <- "Processing data, one minute..."
    showPageSpinner(type = 6, caption = data_collection_caption)
    
    # Data Frames
    if(Sys.info()["user"] == "fred") source(here("data", "base_frames.R"))
    load("nba_base.RData", envir = globalenv())
    .load_datasets <- function() walk(list.files(here("data", "app_data_prep"), full.names = TRUE), \(x) source(x, local = TRUE))
    .load_datasets()
     source(here("data", "nba_fty_stitch_up.R"))
    
    # Extra variable that relies on datasets
    print(cur_date)
    cur_date <<- if(cur_date > max(df_fty_schedule$week_end)) max(df_fty_schedule$week_end) else cur_date
    print(cur_date)
    cur_week <<- df_week_game_count |>
      mutate(week_end = if_else(week_end - week_start < 6, week_start + 6, week_end)) |>
      filter(cur_date >= week_start, cur_date <= week_end) |>
      pull(week) |>
      unique()
    # cur_week <<- 18
    teams <<- sort(unique(df_nba_roster$team_slug))
    
  
# Set Server Side Dynamic Menus -------------------------------------------
 
    sort_players_by_min_desc <- function(df) {
      df |> 
        filter(min > 0) |> 
        summarise(avg_min = median(min), .by = player_name) |> 
        arrange(desc(avg_min)) |>
        pull(player_name)
    }
    
    # Maybe get to the point where I place free agents at top of list
    active_players <<- sort_players_by_min_desc(filter(df_nba_player_box_score, player_injury_status != "NA" | is.na(player_injury_status)))
    free_agents <<- sort_players_by_min_desc(filter(df_nba_player_box_score, player_availability == "free_agent"))
    ls_fty_name_to_cid <<- magrittr::`%$%`(df_fty_competitor, map(setNames(competitor_id, competitor_name), \(x) as.vector(x)))
    ls_fty_cid_to_name <<- magrittr::`%$%`(df_fty_competitor, map(setNames(competitor_name, competitor_id), \(x) as.vector(x)))
    ls_log_config <<- list("reset" = paste0("h2h_competitor=", ls_fty_name_to_cid[input$fty_competitor_select],";h2h_week=", cur_week, ";h2h_ex_player=;h2h_add_player=;h2h_future_only=FALSE;h2h_future_from_tomorrow=FALSE;h2h_hl_player="))
    ss_week <- select(df_fty_schedule, week, week_start, week_end)
    week_drop_box_choices <<- unique(paste0("Week ", ss_week$week, ": (", ss_week$week_start, " to ", ss_week$week_end, ")"))
    week_drop_box_choices <<- sort(setNames(str_extract(week_drop_box_choices, "\\d{4}.*-\\d{2}"), week_drop_box_choices))
  

# Init app filter list creation -------------------------------------------
  
    # Player Comparison tab
    updateSelectInput(session, "comparison_team_filter", choices = teams)
    
    # Player trend tab
    updateSelectInput(session, "trend_select_player", choices = active_players)
    
    # H2H tab
    updateSelectInput(session, "h2h_competitor", choices = ls_fty_name_to_cid, selected = ls_fty_name_to_cid[input$fty_competitor_select])
    updateSelectInput(session, "h2h_week", choices = sort(unique(df_fty_schedule$week)), selected = if(cur_week > max(df_fty_schedule$week)) max(df_fty_schedule$week) else cur_week)  
    updateSelectInput(session, "h2h_log_config", choices = ls_log_config)
    
    # NBA schedule tab
    updateSelectInput(session, "week_selection", choices = week_drop_box_choices, selected = week_drop_box_choices[[cur_week]])
    
    update
   
    # Stop loading page
    hidePageSpinner() 
    
  }) |> 
    bindEvent(input$fty_dash_init)

# On-going filter list alteration -----------------------------------------
  
  observe({
    # Draft Assistance tab
    req(fty_parameters_met, exists("df_nba_player_box_score"))
    
    stat_calc <- if(input$draft_tot_avg_toggle) getFunction("sum") else getFunction("mean")
    min_range <- df_nba_player_box_score |> 
      summarise(min = stat_calc(min, na.rm=TRUE), .by = player_id) |> 
      pull(min) |> 
      quantile(na.rm = TRUE) |> 
      round()
  
    updateSliderTextInput(session, "draft_min_filter", choices = seq(from = min_range[["100%"]], to = min_range[["0%"]]), selected = min_range[["50%"]])
  }) |> 
    bindEvent(input$draft_tot_avg_toggle)
 
  # Additional H2H filter alteration
  observe({
    req(fty_parameters_met, exists("df_h2h_og"))
    competitor_players <- sort(filter(slice_max(df_h2h_og, game_date, by = competitor_id), competitor_id == input$h2h_competitor)$player_name)
    updateSelectInput(session, "h2h_ex_player", choices = competitor_players)
    updateSelectInput(session, "h2h_add_player", choices = setdiff(active_players, competitor_players))
    updateSelectInput(session, "h2h_hl_player", choices = competitor_players)
    # Is it possible to highlight newly added players? - Can they be included in list?
  }) |> 
    bindEvent(input$h2h_competitor)
  

# FTY League Overview -------------------------------------------------

  # Reactive H2H data creation
  df_lo <- reactive(df_fty_league_overview_prepare(platform_selected, league_selected)) |> 
    bindEvent(input$fty_dash_init)
  
  output$fty_league_overview_rank_plot <- renderPlotly({
    req(fty_parameters_met(), exists("df_fty_base"))
    
    plot_col <- input$fty_lg_ov_cat
    if(input$fty_lg_ov_rank_toggle) plot_col <- paste0(plot_col, "_rank")
    df_fty_league_overview <- df_lo()
    df_point <- filter(df_fty_league_overview, as.integer(matchup_sigmoid) == matchup_sigmoid)

    plt <- df_fty_league_overview |>
        ggplot(aes(x = matchup_sigmoid, y = !!sym(plot_col), colour = competitor_name)) +
        geom_line(linewidth = 0.5) +
        geom_point(data = df_point, size = 2) +
        scale_x_continuous(breaks = sort(unique(df_point$matchup)), labels = sort(unique(df_point$matchup))) +
        labs(title = paste("Competitor Category Ranking:", db_to_fmt_stat_name[[input$fty_lg_ov_cat]]), x = "Matchup Period", y = input$fty_lg_ov_cat) +
        theme_bw()
    
    if(input$fty_lg_ov_rank_toggle) plt <- plt + scale_y_reverse(n.breaks = length(ls_fty_name_to_cid))

    ggplotly(plt) |>
      # Remove hover for line traces: 0:8 for each competitor
      style(hoverinfo = "none", traces = 0:length(unique(df_point$competitor_id))) |>
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) |> 
      rangeslider(
        start = max(df_point$matchup) - 5.1,
        end = max(df_point$matchup) + 0.1,
        range = list(min(df_point$matchup) - 0.2, max(df_point$matchup) + 0.2)
      ) |> 
      config(displayModeBar = FALSE)

  })


# FTY Head to Head --------------------------------------------------------
  
  # Reactive H2H data creation
  df_h2h <- reactive(df_h2h_prepare(as.numeric(input$h2h_competitor), input$h2h_ex_player, input$h2h_add_player, input$h2h_future_from_tomorrow)) |> 
    bindEvent(input$h2h_competitor, input$h2h_ex_player, input$h2h_add_player, input$h2h_future_from_tomorrow)
  
  observe({
    
    t_ls_pre <- map(str_split(input$h2h_log_config, ";"), \(x) str_split(x, "="))[[1]]
    t_ls <- lst()
    for(el in t_ls_pre) t_ls[el[1]] <- el[2]
    for(el in names(t_ls)){
      if(str_detect(t_ls[[el]], "TRUE|FALSE"))
        t_ls[[el]] = as.logical(t_ls[[el]])
      else if(str_detect(t_ls[[el]], "\\d+"))
        t_ls[[el]] = as.numeric(t_ls[[el]])
      else if(str_detect(t_ls[[el]], ", "))
        t_ls[[el]] = str_split_1(t_ls[[el]], ", ")
    } 
    
    chk_boxes <- c("h2h_future_only", "h2h_future_from_tomorrow")
    walk(chk_boxes, \(x) updateCheckboxInput(session, x, value = t_ls[[x]]))
    walk(setdiff(names(t_ls), chk_boxes), \(x) updateSelectInput(session, x, selected = t_ls[[x]]))
    
  }) |> 
    bindEvent(input$h2h_log_config, ignoreInit = TRUE)
  
  observe({
    el_cfg <- paste0("h2h_competitor=", str_squish(input$h2h_competitor), ";h2h_week=", input$h2h_week, ";h2h_ex_player=", toString(input$h2h_ex_player), ";h2h_add_player=", toString(input$h2h_add_player), ";h2h_future_only=", input$h2h_future_only, ";h2h_future_from_tomorrow=", input$h2h_future_from_tomorrow, ";h2h_hl_player=", toString(input$h2h_hl_player))
    ls_log_config[paste0("config_", input$h2h_snapshot_config)] <<- el_cfg
    ls_log_config <<- ls_log_config[!duplicated(unlist(ls_log_config, use.names = FALSE))]
    updateSelectInput(session, "h2h_log_config", choices = ls_log_config)
  }) |> 
    bindEvent(input$h2h_snapshot_config, ignoreInit = TRUE)
  
  output$h2h_stat_plot <- renderPlotly({

    if(input$h2h_week < cur_week & input$h2h_future_only){
      ggplotly((
        ggplot() +
          theme_void() +
          geom_text(aes(x = 0, y = 0, label = "Future only dumbass..."))
      ))
    } else {

      df_h <<- df_h2h() 
      if(input$h2h_future_from_tomorrow) df_h <- mutate(df_h, origin = if_else(origin == "today", "past", origin))
      if(input$h2h_future_only) df_h <- filter(df_h, origin != "past")
      opp_id <- filter(df_h, league_week == input$h2h_week, competitor_id == as.numeric(input$h2h_competitor))$opponent_id[1]
      
      h2h_plt <- df_h |> 
        # Need to join player injury status and coalesce it for add players (player_injury_status_temp).
        # Initially I tried doing this in h2h prep file, but it kept breaking other things
        left_join(
          select(df_stitch, player_fantasy_id, player_injury_status_temp=player_injury_status), 
          by = join_by(player_fantasy_id)
        ) |> 
        mutate(player_injury_status = coalesce(player_injury_status, player_injury_status_temp)) |> 
        select(-player_injury_status_temp) |> 
        filter(
          competitor_id %in% c(as.numeric(input$h2h_competitor), opp_id),
          league_week == input$h2h_week,
          scheduled_to_play == 1, 
          player_injury_status %in% c("ACTIVE", "DAY_TO_DAY")
        ) |> 
        pivot_longer(cols = c(ast, stl, blk, tov, pts, ftm, fta, fgm, fga, fg3_m, reb), names_to = "stat", values_to = "value") |>  
        select(competitor_id, player_name, stat, value) |> 
        summarise(value = sum(value, na.rm = TRUE), .by = c(competitor_id, player_name, stat)) |> 
        (\(df_tmp){
          bind_rows(
            # fg_pct
            filter(df_tmp, stat %in%  c("fga", "fgm")) |>
              pivot_wider(names_from = stat, values_from = value) |>
              arrange(desc(fgm)) |>
              mutate(fg_pct = round(fgm / fga, 2)) |>
              summarise(
                competitor_roster = paste0(player_name, " ", fg_pct, " (", round(fgm, 2), "/", round(fga, 2), ")", collapse = "\n"),
                value = sum(fgm) / sum(fga),
                .by = competitor_id
              ) |>
              mutate(stat = "fg_pct"),
  
            #ft_pct
            filter(df_tmp, stat %in%  c("fta", "ftm")) |>
              pivot_wider(names_from = stat, values_from = value) |>
              arrange(desc(ftm)) |>
              mutate(ft_pct = round(ftm / fta, 2)) |>
              summarise(
                competitor_roster = paste0(player_name, " ", ft_pct, " (", round(ftm, 2), "/", round(fta, 2), ")", collapse = "\n"),
                value = sum(ftm) / sum(fta),
                .by = competitor_id
              ) |>
              mutate(stat = "ft_pct"),
  
            # tov
            filter(df_tmp, stat == "tov") |>
              arrange(value) |>
              summarise(
                competitor_roster = paste(player_name, round(value), collapse = "\n"),
                value = sum(value),
                .by = c(competitor_id, stat)
              ),
  
            # the rest
            filter(df_tmp, !stat %in% c("fga", "fgm", "fta", "ftm", "tov")) |>
              arrange(desc(value)) |>
              summarise(
                competitor_roster = paste(player_name, round(value), collapse = "\n"),
                value = sum(value),
                .by = c(competitor_id, stat)
              )
          )
        })() |> 
        left_join(
          filter(df_fty_base, platform == platform_selected, league_id == as.numeric(league_selected)) |> 
            select(competitor_id, competitor_name),
          by = join_by(competitor_id)
        ) |> 
        mutate(competitor_name = ordered(competitor_name, c(ls_fty_cid_to_name[as.character(opp_id)], ls_fty_cid_to_name[input$h2h_competitor])))
        
      (
        ggplot(h2h_plt, aes(x = stat, y = value, fill = competitor_name, text = paste(round(value, 2), "\n\n", competitor_roster))) +
          geom_col(position = "fill") +
          geom_hline(yintercept = 0.5) +
          scale_fill_brewer(type = "qual", palette = "Set2") +
          labs(title = paste0("Week ", input$h2h_week, ": ", ls_fty_cid_to_name[input$h2h_competitor], " vs ", ls_fty_cid_to_name[as.character(opp_id)], x = NULL, y = NULL, fill = NULL)) +
          theme_bw()
      ) |>
        ggplotly(tooltip = "text") |>
        layout(hovermode = "x") |> 
        config(displayModeBar = FALSE)

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

      df_h <<- df_h2h()
      if(input$h2h_future_from_tomorrow) df_h <- mutate(df_h, origin = if_else(origin == "today", "past", origin))
      if(input$h2h_future_only) df_h <- filter(df_h, origin != "past")
      opp_id <- filter(df_h, competitor_id == as.numeric(input$h2h_competitor), league_week == input$h2h_week)$opponent_id[1]
      
      
      df_h2h_week_game_count <<- df_h |> 
        # Need to join player injury status and coalesce it for add players (player_injury_status_temp).
        # Initially I tried doing this in h2h prep file, but it kept breaking other things
        left_join(
          select(df_stitch, player_fantasy_id, player_injury_status_temp=player_injury_status), 
          by = join_by(player_fantasy_id)
        ) |> 
        mutate(player_injury_status = coalesce(player_injury_status, player_injury_status_temp)) |> 
        select(-player_injury_status_temp) |> 
        filter(
          competitor_id %in% c(as.numeric(input$h2h_competitor), opp_id), 
          league_week == input$h2h_week 
        ) |> 
        mutate(inj_status = case_when(
          scheduled_to_play == 1 & str_detect(player_injury_status, "^O|INJ") ~ "1*",
           scheduled_to_play == 1 ~ "1",
          .default = NA_character_
        )) |> 
        arrange(game_date) |> 
        pivot_wider(id_cols = c(competitor_id, opponent_id, player_team, player_name), names_from = game_date, values_from = inj_status) |> 
        (\(df){
  
          inner_func <- function(x, nm) filter(x, competitor_id == nm) |>
            mutate(player_team = "Total", player_name = str_trim(nm)) |>
            summarise(across(starts_with("20"), \(x) as.character(sum(x == "1", na.rm = TRUE))), .by = c(player_team, player_name))
  
          bind_rows(
            inner_func(df, opp_id),
            inner_func(df, as.numeric(input$h2h_competitor)),
            setNames(as.data.frame(matrix(rep(NA, length(colnames(df))), nrow = 1)), colnames(df)),
            select(filter(df, competitor_id == as.numeric(input$h2h_competitor)), starts_with(c("player", "20"))) |>
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
        mutate(fty_matchup_week = as.numeric(input$h2h_week)) |>
        left_join(
          select(df_week_game_count, week, team, following_week_games),
          by = join_by(player_team == team, fty_matchup_week == week)
        ) |> 
        select(-fty_matchup_week, next_week = following_week_games) |> 
        distinct()
      
      df_h2h_week_game_count_tbl <<- select(df_h2h_week_game_count, starts_with("player"), all_of(sort(str_subset(colnames(df_h2h_week_game_count), "\\d"))), Total, `Next Week` = next_week, Team = player_team, Player = player_name) |> 
        rename_with(.fn = \(x) format(as.Date(x), "%a (%d/%m)"), .cols = starts_with("20")) |> 
        mutate(Player = str_replace_all(Player, setNames(unlist(ls_fty_cid_to_name), map_chr(names(ls_fty_cid_to_name), \(x) paste0("^", x, "$")))))
      
      max_game_count <- max(df_h2h_week_game_count_tbl$Total, na.rm = TRUE)
      min_next_week_game_count <- min(df_h2h_week_game_count_tbl$`Next Week`, na.rm = TRUE)

      tibble::rowid_to_column(df_h2h_week_game_count_tbl) |>
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
        formatStyle(colnames(df_h2h_week_game_count_tbl), border = "1px solid #000") |>
        formatStyle("rowid", target = "row", backgroundColor = styleEqual(3, "grey")) |>
        formatStyle("Total", target = "cell", backgroundColor = styleEqual(max_game_count, "lightgreen")) |>
        formatStyle(c("Team", "Player"), backgroundColor = "azure") |>
        formatStyle("Next Week", target = "cell", backgroundColor = styleEqual(min_next_week_game_count, "#FFBBFF")) |>
        (\(dt){
          cols <- str_subset(colnames(df_h2h_week_game_count_tbl), "\\(")
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

          if (format(cur_date, "%a (%d/%m)") %in% cols) dt <- formatStyle(dt, format(cur_date, "%a (%d/%m)"), target = "cell", backgroundColor = styleEqual("1*", "pink", default = "lightyellow"))
          if (length(input$h2h_hl_player) > 0) dt <- formatStyle(dt, "Player", target = "row", backgroundColor = styleEqual(input$h2h_hl_player, rep("#54FF9F", length(input$h2h_hl_player))))

          dt
        })()

      }
  })
  

# NBA Player Comparison -------------------------------------------------------

  # Count how many events a player excels given the selection
  .calc_xl_at_count <- function(df){
    df$xl_at_count <- 0
    for(category in filter(stat_selection, database_name %in% input$comparison_excels_at_filter, !str_detect(formatted_name, "%"))$database_name){
      df$xl_at_count <- str_detect(df$`Excels At`, category) + df$xl_at_count
    }
    df
  } 
  
  output$player_comparison_table <- renderDT({

    # Some players have teams missing | Some players are missing (eg filter to one team)
    df_comparison <<- df_nba_player_box_score |>
      filter(
        game_date <= cur_date,
        # game_date >= cur_date - days(15)
        game_date >= cur_date - case_when(input$date_range_switch == "Two Weeks" ~ days(15), input$date_range_switch == "One Month" ~ days(30), .default = days(7))
      ) |> 
      summarise(across(any_of(anl_cols$stat_cols), \(x) mean(x)), .by = c(player_id, player_name)) |>
      mutate(across(where(is.numeric), \(x) replace_na(x, 0L))) |>
      calc_z_pcts() |>
      select(-ends_with("_pct")) |>
      (\(df_tmp) {
        left_join(
          df_tmp,
          {
            select(df_tmp, player_id, player_name, any_of(stat_selection$database_name), -min) |>
              mutate(across(any_of(stat_selection$database_name[stat_selection$database_name != "tov"]), ~ round(scales::rescale(.x), 2))) |>
              mutate(tov = round((((tov * -1) - min(tov)) / (max(tov) - min(tov))) + 1, 2)) |>
              pivot_longer(cols = any_of(stat_selection$database_name), names_to = "stat") |>
              (\(df_tmp) {
                bind_rows(
                  mutate(slice_max(df_tmp, value, n = 3, by = c(player_id, player_name), with_ties = FALSE), performance = "Excels At") |> filter(value > 0),
                  mutate(slice_min(df_tmp, value, n = 3, by = c(player_id, player_name)), performance = "Weak At")
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
        slice_max(df_nba_player_box_score, game_date, by = player_name) |>
          select(Player = player_name, Team = team_slug, player_availability, player_injury_status),
        by = join_by(Player)
      ) |>
      mutate(inj_status = str_extract(player_injury_status, "[O|INJ|DAY|SUS|NA].*")) |>
      mutate(inj_status = case_when(
        str_detect(inj_status, "^O|INJ$|NA$") ~ "(out)",
        inj_status == "SUSPENSION" ~ "(sus)",
        str_detect(inj_status, "DAY_TO_DAY|GTD") ~ "(d2d)",
        .default = ""
      )) |>
      mutate(
        Player = paste(Player, inj_status),
        player_colour = case_when(
          inj_status == "(out)" ~ "red",
          inj_status == "(sus)" ~ "pink",
          inj_status == "(d2d)" ~ "pink",
          .default = "azure"
        )
      ) |>
      relocate(Team, .after = Player) |>
      .calc_xl_at_count()

    df_comparison_table <- filter(df_comparison, Minutes >= input$comparison_minute_filter)
    if(input$comparison_free_agent_filter) df_comparison_table <- filter(df_comparison_table, player_availability == "free_agent")
    if(!is_null(input$comparison_team_filter)) df_comparison_table <- filter(df_comparison_table, Team %in% input$comparison_team_filter)
    if(!is_null(input$comparison_excels_at_filter)) df_comparison_table <- filter(df_comparison_table, str_detect(`Excels At`, paste0(filter(stat_selection, database_name %in% input$comparison_excels_at_filter)$database_name, collapse = "|")))
    df_comparison_table <- select(df_comparison_table, -c(player_availability, ends_with("_status"))) |>
      arrange(desc(Minutes))

    df_comparison_table |>
      datatable(
        rownames = FALSE,
        escape = FALSE,
        style = "default",
        options = lst(
          dom = "t",
          paging = FALSE,
          fixerHeader = TRUE,
          columnDefs = list(list(visible = FALSE, targets = str_which(colnames(df_comparison_table), "_count|_colour") - 1)),
          initComplete = JS(
            "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': 'blue', 'color': 'white'});",
            "}"
          )
        )
      ) |>
      formatStyle(columns = "Player", valueColumns = "player_colour", backgroundColor = styleEqual(c("red", "pink", "azure"), c("red", "pink", "azure"))) |>
      formatStyle(columns = colnames(df_comparison_table), border = "1px solid #000000") |>
      formatCurrency(c(3:5, 8:12), "", digits=1) |>
      formatCurrency(6:7, "") |>
      formatStyle(
        "Turnovers",
        backgroundColor = styleInterval(min(df_comparison_table$Turnovers) + 0.01, c("lightgreen", "white")),
      ) |>
      formatStyle(columns = c("Excels At", "Weak At"), fontSize = "80%") |> 
      # NOT WORKING... WHY???
      # formatStyle(
      #   columns = "Excels At",
      #   valueColumns = "xl_at_count",
      #   backgroundColor = styleEqual(0:3, c("black", "cornsilk2", "cornsilk1", "wheat"))
      # ) |>
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
        })()
  })


# NBA Schedule Table ------------------------------------------------------
  
  observe({
    
    selected_week_dates <<- as.Date(str_split_1(input$week_selection, " to "))
    date_input_value <- cur_date
    if(input$pin_date < selected_week_dates[1]) date_input_value <- selected_week_dates[1]
    if(input$pin_date > selected_week_dates[2]) date_input_value <- selected_week_dates[2]
    updateDateInput(session, "pin_date", value = date_input_value, min = selected_week_dates[1], max = selected_week_dates[2])
    
  }) |> 
    bindEvent(input$week_selection)
  
  observe({
    tms <- tbl_schedule_grid[input$schedule_table_rows_current, ]$Team
    updateSelectInput(session, "comparison_team_filter", selected = tms)
  }) |> 
    bindEvent(input$copy_teams, ignoreInit = TRUE)

  output$schedule_table <- renderDT({
    
    # Prepare tables to be presented
    # tbl_schedule <<- tbl_week_games$data[[6]] |>
    tbl_schedule <- tbl_week_games$data[[match(input$week_selection, week_drop_box_choices)]] |>
      mutate(across(ends_with(")"), \(x) if_else(as.character(x) == "NULL", 0, 1))) |>
      mutate(across(c(contains("games"), Team), as.factor))

    ts_names <- tmp_names <- str_subset(colnames(tbl_schedule), "\\(")
    names(ts_names) <- names(tmp_names) <- str_sub(ts_names, end = 3)
    
    # Add condition for where years aren't equal
    for(ix in 1:length(ts_names)){
      nm <- table(names(tmp_names)[1:ix])[names(tmp_names)[ix]]
      names(ts_names)[ix] <- paste0(nm[[1]], "_", names(nm))
      
      if(as.Date(paste0(year(selected_week_dates[1]), "/", str_extract(tmp_names[ix], "\\d{2}/\\d{2}")), "%Y/%d/%m") == selected_week_dates[2])
        wk_th <<- ix
      
      if(!exists("wk_th") && as.Date(paste0(year(selected_week_dates[1]), "/", str_extract(tmp_names[ix], "\\d{2}/\\d{2}")), "%Y/%d/%m") == selected_week_dates[2] + days(1))
        wk_th <<- ix - 1
      
    }
    expected_elements <- c("1_Mon", "1_Tue", "1_Wed", "1_Thu", "1_Fri", "1_Sat", "1_Sun", "2_Mon", "2_Tue")
    ts_names <- discard(ts_names[expected_elements], is.na)
    
    # If a day is missing from ts_names add it in
    if(length(keep_at(ts_names, \(x) str_detect(x, "1_"))) < 7){
      ms_dt_nm <- setdiff(keep(expected_elements, \(x) str_detect(x, "1_")), names(ts_names))
      ms_dt_ix <- which(expected_elements == ms_dt_nm) - 1
      ms_dt <- (selected_week_dates[1] + days(ms_dt_ix)) |> 
        format("%a (%d/%m)") |> 
        setNames(ms_dt_nm)
      
      ts_names <- append(ts_names, ms_dt, after = ms_dt_ix)
      tbl_schedule <- mutate(tbl_schedule, !!ms_dt := NA_real_, .after = ms_dt_ix + 1)
      wk_th <<- wk_th + 1
    }
    
    
    pin_index <- match(str_subset(colnames(tbl_schedule), format(input$pin_date, "%d/%m")), colnames(tbl_schedule))
    pin_sum_cols <- if(input$pin_dir == "+") pin_index:(wk_th+1) else 2:pin_index
    
    tbl_schedule_grid <<- tbl_schedule |>
      rowwise() |>
      mutate(
        # `Games From Pin` = factor(sum(c_across(str_subset(ts_names, format(input$pin_date, "%d/%m")):ts_names[wk_th]), na.rm = TRUE)),
        `Games From Pin` = factor(sum(c_across(pin_sum_cols), na.rm = TRUE)),
        .before = "Following Week Games"
      ) |>
      relocate(contains("games"), .after = wk_th + 1) |>
      mutate(across(ends_with(")"), \(x) as.factor(if_else(x == 0, ".", "1"))))
    
    

    # Present table -- TRY disabling vertical scroll on card and activating
    # on table instead...
    datatable(
      tbl_schedule_grid,
      rownames = FALSE,
      class = "cell-border stripe",
      style = "default",
      options = list(
        paging = FALSE,
        fixerHeader = TRUE,
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
    formatStyle(columns = str_subset(ts_names, format(input$pin_date, "%d/%m")), backgroundColor = "lightyellow") |>
    (\(tb){
      
      lvl <- 0:length(unique(tbl_schedule_grid$`Games From Pin`))
      col <- c("white", rev(RColorBrewer::brewer.pal(5, "Greens")))[lvl + 1]
      tb <- formatStyle(tb, columns = "Games From Pin", backgroundColor = styleEqual(levels = lvl, values = col))
      
      if(match(input$week_selection, week_drop_box_choices) < length(week_drop_box_choices)){
        tb <- 
          formatStyle(
            tb,
            columns = "Following Week Games",
            backgroundColor = styleEqual(levels = 0:tail(levels(tbl_schedule_grid$`Following Week Games`), 1), values = rev(RColorBrewer::brewer.pal(length(0:tail(levels(tbl_schedule_grid$`Following Week Games`), 1)), "Greens")))
          ) |>
          formatStyle(columns = (ncol(tbl_schedule_grid)-1):ncol(tbl_schedule_grid), backgroundColor = "lightgrey")
      } else 
        tb <- formatStyle(tb, columns = "Following Week Games", backgroundColor = "lightgrey")
      
      tb # return
      
    })()
  })


# NBA Player Trend --------------------------------------------------------

  output$player_trend_plot <- renderPlotly({

    trend_selected_stat <- sym(input$trend_select_stat)

    if(is.null(input$trend_select_player)){
      ggplotly((
        ggplot() +
          theme_void() +
          geom_text(aes(x = 0, y = 0, label = "Select players"))
      ))
    } else {

      df_trend <- (
          if(!input$this_season_trend_switch) df_nba_player_box_score
            else filter(df_nba_player_box_score, season == cur_season)
        ) |>
        filter(player_name %in% input$trend_select_player) |>
        arrange(game_date) |>
        mutate(
          day_sequence = as.integer((game_date - min(game_date)) + 1),
          smooth = loess(replace_na({{ trend_selected_stat }}, 0) ~ day_sequence)$fitted,
          .by = c(season, player_name)
        ) |>
        (\(df) bind_rows(df, summarise(df, game_date = max(game_date) + 1, .by = c(player_name, year_season_type))))() |>
        mutate(player_name = factor(player_name, input$trend_select_player))

      ggplotly((
        ggplot(df_trend, aes(x = game_date, colour = player_name)) +
          geom_point(aes(y = {{ trend_selected_stat }}), alpha = 0.2) +
          geom_line(aes(y = smooth)) +
          scale_x_datetime(name = NULL, breaks = df_nba_season_segments$mid_date, labels = df_nba_season_segments$year_season_type) +
          geom_vline(xintercept = as.numeric(df_nba_season_segments$begin_date), colour = "grey") +
          ylim(0, NA) +
          labs(title = db_to_fmt_stat_name[[input$trend_select_stat]], x = NULL, y = NULL) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
      ))
    }
  })


# Draft Assistance --------------------------------------------------------
  
  # MEAN VIEW ISN'T WORKING - check quantile stuff
  output$draft_stat_plot <- renderPlotly({

    # Stat calc
    stat_calc <- if(input$draft_tot_avg_toggle) getFunction("sum") else getFunction("mean")
    df_overview <- df_nba_player_box_score |>
      filter(season == prev_season, !(is.na(player_id) | is.na(player_name))) |>
      summarise(across(any_of(anl_cols$stat_cols), \(x) stat_calc(x, na.rm = TRUE)), .by = c(player_id, player_name)) |> 
      calc_z_pcts()

    # Minute filter
    df_overview <- filter(df_overview, min >= as.numeric(input$draft_min_filter))

    # Scale by minutes (if selected)
    if(input$draft_scale_minutes) df_overview <- mutate(df_overview, across(all_of(stat_selection$database_name[-c(1,2)]), ~ .x / min))

    # Create df for plot
    df_overview <- map(str_subset(stat_selection$database_name, "_pct|_cat", negate = TRUE), \(x){

      col = sym(x)

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
      set_names(filter(stat_selection, !str_detect(database_name, "_pct|_cat"))$formatted_name) |>
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
      reverse_legend_labels() |>
      config(displayModeBar = FALSE)

  })

  
  
# News Transactions -------------------------------------------------------

    output$news_transactions <- renderDT({
      df_t <- df_nba_news |> 
        arrange(desc(date)) |> 
        rename_with(\(x) str_to_title(str_replace(x, "_", " ")))
      
      datatable(
        df_t,
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
      formatStyle(columns = colnames(df_t), background = "white", color = "black")
  })

}
