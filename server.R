# Libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(jsonlite)
library(magrittr, include.only = "%$%")

# Initialisation files ----------------------------------------------------

# only init python if running in shiny
# fmt: skip
if(Sys.info()["user"] == "shiny") source(here("_proj_python.R"))
source(here("_proj_useful.R"))


# Server code -------------------------------------------------------------

server <- function(input, output, session) {
  # Variables ---------------------------------------------------------------

  nba_api <- reticulate::import("nba_api")
  espn_api <- reticulate::import("espn_api.basketball")

  db_con <<- if (Sys.info()["user"] == "fred") dh_createCon("postgres") else dh_createCon("cockroach")
  # cur_date <<- strptime(Sys.time(), "%Y", tz = "EST")
  cur_date <<- as.Date("2025-12-01") # DELETE
  cur_season <<- nba_api$stats$library$parameters$Season$current_season
  prev_season <<- nba_api$stats$library$parameters$Season$previous_season
  df_fty_base <<- readRDS("fty_base.RDS")
  ls_fty_base <<- distinct(df_fty_base, platform, league_id, league_name) %$% map(setNames(paste0(platform, "_", league_id), league_name), \(x) as.vector(x))
  vec_player_log_stream <<- dh_getQuery(db_con, "SELECT * FROM util.draft_player_log")

  # Login -------------------------------------------------------------------

  # Reactive values
  fty_parameters_met <- reactiveVal(FALSE)
  platform_selected <- reactiveVal()
  league_selected <- reactiveVal()
  competitor_selected <- reactiveVal()

  # Assign values to platform and league selected
  observe({
    updateSelectInput(inputId = "fty_competitor_select", choices = filter(df_fty_base, league_name == input$fty_league_select)$competitor_name)
    platform_selected(str_split(ls_fty_base[input$fty_league_select], "_")[[1]][1])
    league_selected(str_split(ls_fty_base[input$fty_league_select], "_")[[1]][2])
  }) |>
    bindEvent(input$fty_league_select, ignoreNULL = TRUE)

  # Login modal error hadling
  observe({
    if (input$fty_competitor_select == "" && input$fty_league_select == "") {
      output$login_messages <- renderText("Select a league, then select a competitor.")
    } else if (input$fty_competitor_select != "") {
      fty_parameters_met(TRUE)
      removeModal()
      output$login_messages <- NULL
    } else {
      output$login_messages <- renderText("Select a competitor")
    }
  }) |>
    bindEvent(input$fty_dash_init)

  # Login modal progress into app
  observe({
    if (!fty_parameters_met()) {
      output$login_messages <- renderText("You gotta go fill the form at least once!")
    } else {
      removeModal()
      output$login_messages <- NULL
    }
  }) |>
    bindEvent(input$fty_abort)

  # In the case league is switched mid-use of app
  bindEvent(observe(login_modal()), input$fty_league_competitor_switch)

  # Login modal object
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
          options = list(
            placeholder = "Select Fantasy Competitor",
            onInitialize = I("function(){this.setValue('');}")
          ),
          width = "100%"
        ),
        span(textOutput("login_messages"), style = "color:red"),
        footer = tagList(
          actionButton(
            "fty_abort",
            label = NULL,
            icon = icon("square-xmark"),
            style = "color:#FFF; background-color:#CD3333; border-color:#2E6DA4"
          ),
          actionButton(
            "fty_dash_init",
            "Kobeee!",
            style = "color:#FFF; background-color:#337AB7; border-color:#2E6DA4"
          )
        ),
        size = "m"
      )
    )
  }
  login_modal()
  output$navbar_title <- renderUI(span(input$fty_league_select))

  # Load datasets -----------------------------------------------------------

  observe({
    req(fty_parameters_met())

    # Start loading page
    data_collection_caption <- "Processing data, one minute..."
    showPageSpinner(type = 6, caption = data_collection_caption)

    # Data Frames
    # fmt: skip
    if (Sys.info()["user"] == "fred") source(here("data", "base_frames.R"))
    load("nba_base.RData", envir = globalenv())
    load(here(paste0("fty_", ls_fty_base[input$fty_league_select], ".RData")), envir = globalenv())

    .load_datasets <- function() {
      walk(list.files(here("data", "app_data_prep"), full.names = TRUE), \(x) {
        cat(paste("Sourcing:", str_extract(x, "app_data_prep\\/\\w+.R"), "\n"))
        source(x, local = TRUE)
      })
    }
    .load_datasets()
    # fmt: skip
    if (nrow(df_fty_roster) == 0) fty_parameters_met(FALSE)

    # Extra variables that relies on datasets
    cur_date <<- if (
      !is.infinite(max(df_fty_schedule$matchup_end)) &&
        cur_date > max(df_fty_schedule$matchup_end)
    ) {
      max(df_fty_schedule$matchup_end)
    } else {
      cur_date
    }
    cur_matchup <<- df_matchup_game_count |>
      mutate(matchup_end = if_else(matchup_end - matchup_start < 6, matchup_start + 6, matchup_end)) |>
      filter(cur_date >= matchup_start, cur_date <= matchup_end) |>
      pull(matchup_period) |>
      unique()
    # fmt: skip
    if (length(cur_matchup) == 0) cur_matchup <<- 1
    # cur_matchup <<- 18
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
    ls_fty_name_to_cid <<- df_fty_competitor %$% map(setNames(competitor_id, competitor_name), \(x) as.vector(x))
    ls_fty_cid_to_name <<- df_fty_competitor %$% map(setNames(competitor_name, competitor_id), \(x) as.vector(x))
    ss_matchup <<- distinct(df_fty_schedule, matchup_period, matchup_start, matchup_end) |> arrange(matchup_period)
    matchup_drop_box_choices <<- str_c("Matchup ", ss_matchup$matchup_period, ": (", ss_matchup$matchup_start, ")")
    matchup_drop_box_choices <<- setNames(str_extract(matchup_drop_box_choices, "\\d{4}.*-\\d{2}"), matchup_drop_box_choices)
    competitor_selected(ls_fty_name_to_cid[[input$fty_competitor_select]])

    # Init app filter list creation -------------------------------------------

    # Player Comparison tab
    updateSelectInput(session, "comparison_team_or_player_filter", choices = teams)
    updateSelectizeInput(
      session,
      "comparison_excels_at_filter",
      choices = cat_specs(incl_nba_cat = c("min", "fg_z", "ft_z"), excl_nba_cat = c("fg_pct", "ft_pct"))
    )

    # Player trend tab
    updateSelectInput(session, "trend_select_player", choices = active_players)
    updateSelectInput(session, "trend_select_stat", choices = cat_specs(incl_nba_cat = "min"))

    # H2H tab
    updateSelectInput(session, "h2h_competitor", choices = ls_fty_name_to_cid, selected = ls_fty_name_to_cid[input$fty_competitor_select])
    updateSelectInput(session, "h2h_log_config", choices = h2h_configurations())
    updateSelectInput(
      session,
      "h2h_matchup",
      choices = ss_matchup$matchup_period,
      selected = if (
        !is.infinite(max(df_fty_schedule$matchup_period)) &&
          cur_matchup > max(df_fty_schedule$matchup_period)
      ) {
        max(df_fty_schedule$matchup_period)
      } else {
        cur_matchup
      }
    )

    # NBA schedule tab
    updateSelectInput(session, "matchup_selection", choices = matchup_drop_box_choices, selected = pluck(matchup_drop_box_choices, cur_matchup))

    # Draft tab
    updateSelectInput(session, "draft_player_log", choices = active_players, selected = vec_player_log_stream$player_name)
    updateSelectInput(session, "draft_stat", choices = cat_specs(incl_nba_cat = c("min", "fg_z", "ft_z"), excl_nba_cat = c("fg_pct", "ft_pct")))

    # League Overview tab
    updateSelectInput(
      session,
      "fty_lg_ov_cat",
      choices = list(
        "Overall" = cat_specs(h2h = FALSE)["All Categories"],
        "Categories" = cat_specs(),
        "Z Scores" = keep(cat_specs(h2h = FALSE), \(x) str_detect(x, "_z"))
      )
    )

    # Stop loading page
    hidePageSpinner()
  }) |>
    bindEvent(input$fty_dash_init, fty_parameters_met(), ignoreInit = TRUE)

  # On-going filter list alteration -----------------------------------------

  observe({
    # Draft Assistance tab
    req(fty_parameters_met, exists("df_nba_player_box_score"))
    stat_calc <- if (input$draft_tot_avg_toggle) getFunction("sum") else getFunction("mean")

    # Minute Range Filter
    min_range <- filter(df_nba_player_box_score, season == prev_season) |>
      summarise(min = stat_calc(min, na.rm = TRUE), .by = player_id) |>
      pull(min) |>
      quantile(na.rm = TRUE) |>
      round()

    updateSliderInput(session, "draft_min_filter", max = min_range[["100%"]], value = min_range[["75%"]])

    # Variance Coefficient filter
    cov_quantiles <- df_nba_player_box_score |>
      filter(season == prev_season) |>
      summarise(
        cov = sd(!!sym(str_replace(input$draft_stat, "_z", "_pct")), na.rm = TRUE) /
          mean(!!sym(str_replace(input$draft_stat, "_z", "_pct")), na.rm = TRUE),
        .by = c(player_id, player_name)
      ) |>
      pull(cov) |>
      quantile(na.rm = TRUE)

    updateSliderInput(
      session,
      "draft_cov_filter",
      min = floor(cov_quantiles[["0%"]] * 10^2) / 10^2,
      max = round(cov_quantiles[["100%"]], 2),
      value = cov_quantiles[["50%"]]
    )
  }) |>
    bindEvent(input$draft_tot_avg_toggle, input$draft_stat)

  # Additional Team or Player comparison filter alteration
  observe({
    req(exists("teams"))
    updateSelectInput(
      session,
      "comparison_team_or_player_filter",
      choices = if (input$comparison_team_or_player) {
        teams
      } else if (!input$comparison_team_or_player & input$comparison_free_agent_filter) {
        intersect(
          free_agents,
          df_comparison |>
            filter(Minutes >= input$comparison_minute_filter) |>
            pull(Player) |>
            str_remove("\\(.*\\)") |>
            str_squish()
        )
      } else {
        intersect(
          active_players,
          df_comparison |>
            filter(Minutes >= input$comparison_minute_filter) |>
            pull(Player) |>
            str_remove("\\(.*\\)") |>
            str_squish()
        )
      }
    )
  }) |>
    bindEvent(input$comparison_team_or_player, input$comparison_free_agent_filter)

  # FTY League Overview -------------------------------------------------

  # Reactive H2H data creation
  # df_lo <-
  df_fty_league_overview <- reactive(df_fty_league_overview_prepare()) |>
    bindEvent(input$fty_dash_init)

  output$fty_league_overview_rank_plot <- renderPlotly({
    req(fty_parameters_met(), exists("df_fty_base"), exists("ls_fty_name_to_cid"))

    plot_col <- input$fty_lg_ov_cat
    if (input$fty_lg_ov_rank_toggle) {
      plot_col <- str_c(plot_col, "_rank")
    }

    df_point <- filter(df_fty_league_overview(), as.integer(matchup_sigmoid) == matchup_sigmoid)

    plt <- if (input$fty_lg_ov_cum_toggle) {
      df_fty_league_overview() |>
        ggplot(aes(x = matchup_sigmoid, y = !!sym(plot_col), colour = competitor_name)) +
        geom_line(linewidth = 0.5) +
        geom_point(data = df_point, size = 2) +
        scale_x_continuous(breaks = sort(unique(df_point$matchup)), labels = sort(unique(df_point$matchup))) +
        labs(
          title = paste("Competitor Category Ranking:", names(keep(cat_specs(h2h = FALSE), \(x) x == input$fty_lg_ov_cat))),
          x = "Matchup Period",
          y = input$fty_lg_ov_cat
        ) +
        theme_bw()
    } else {
      df_point |>
        arrange(matchup) |>
        group_by(competitor_id) |>
        mutate(across(c(all_of(cat_specs(vec = TRUE)), matches("cat$|rank$")), \(x) cumsum(x))) |>
        ggplot(aes(x = matchup_sigmoid, y = !!sym(plot_col), colour = competitor_name)) +
        geom_path() +
        scale_x_continuous(breaks = sort(unique(df_point$matchup)), labels = sort(unique(df_point$matchup))) +
        labs(
          title = paste("Competitor Category Ranking:", names(keep(cat_specs(h2h = FALSE), \(x) x == input$fty_lg_ov_cat))),
          x = "Matchup Period",
          y = input$fty_lg_ov_cat
        ) +
        theme_bw()
    }

    if (input$fty_lg_ov_rank_toggle) {
      plt <- plt + scale_y_reverse(n.breaks = length(ls_fty_name_to_cid))
    }

    plt <- ggplotly(plt) |>
      # Remove hover for line traces: 0:8 for each competitor
      style(hoverinfo = "none", traces = 0:length(unique(df_point$competitor_id))) |>
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) |>
      rangeslider(
        start = ifelse(!input$fty_lg_ov_cum_toggle, 1, max(df_point$matchup) - 5.1),
        end = max(df_point$matchup) + 0.1,
        range = list(min(df_point$matchup) - 0.2, max(df_point$matchup) + 0.2)
      ) |>
      config(displayModeBar = FALSE)

    if (input$fty_lg_ov_just_h2h) {
      plt <- {
        c_id <- ls_fty_name_to_cid[input$fty_competitor_select]
        o_id <- df_fty_schedule |>
          filter(competitor_id == c_id, matchup_period == cur_matchup) |>
          pull(opponent_id)

        ts <- map_int(1:length(plt$x$data), \(x) unlist(ls_fty_name_to_cid[pluck(plt$x$data, x, "name")]))
        ts_vis <- c(which(ts == c_id), which(ts == o_id))

        style(plt, visible = "legendonly", traces = setdiff(1:length(plt$x$data), ts_vis))
      }
    }
    plt
  })

  # # NOT FINISHED -- MIGHT SCRAP
  # output$competitor_snapshot <- renderPlotly({
  #   req(fty_parameters_met(), exists("df_fty_base"))

  #   df_fty_roster |>
  #     filter(date == max(date), player_injury_status %in% c("ACTIVE", "DAY_TO_DAY")) |>
  #     left_join(
  #       slice_max(df_rolling_stats, game_date, by = player_id) |>
  #         select(player_id, all_of(cat_specs(vec = TRUE, h2h = FALSE, excl_nba_cat = c("ft_z", "fg_z")))),
  #     ) |>
  #     summarise(
  #       across(all_of(cat_specs(vec = TRUE, incl_nba_cat = c("ftm", "fta", "fgm", "fga"), excl_nba_cat = c("fg_pct", "ft_pct"))), \(x) sum(x)),
  #       .by = competitor_id
  #     ) |>
  #     mutate(ft_pct = ftm / fta, fg_pct = fgm / fga) |>
  #     mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  #     select(-matches("(ft|fg)(m|a)$")) |>
  #     left_join(df_fty_competitor) |>
  #     pivot_longer(all_of(cat_specs(vec = TRUE)), names_to = "category") |>
  #     mutate(
  #       rank = if_else(category == "tov", rank(value, ties.method = "first"), rank(value * -1, ties.method = "first")),
  #       .by = category
  #     ) |>
  #     mutate(competitor_cat = category) |>
  #     pivot_wider(id_cols = rank, values_from = c(value, competitor_name), names_from = category) |>
  #     rename_with(\(x) str_remove(x, "value_")) |>
  #     arrange(rank) |>
  #     (\(x) {
  #       reactable(
  #         x,
  #         columns = list(
  #           # Hide these cols
  #           colnames(x) |>
  #             str_subset("competitor_name_") |>
  #             set_names() |>
  #             map(\(col) colDef(show = FALSE)),

  #           # Colour these cells - NOT WORKING YET
  #           colnames(x) |>
  #             str_subset("competitor_name_|rank", negate = TRUE) |>
  #             set_names() |>
  #             map(\(col) {
  #               colDef(
  #                 cell = function(value, index) {
  #                   competitor <- x[paste0("competitor_name_", col), index]
  #                   color <- if (score >= 90) "green" else "red"
  #                   htmltools::tags$span(style = paste("color:", color), value)
  #                 }
  #               )
  #             }),
  #         ),
  #         pagination = FALSE
  #       )
  #     })()
  # })

  # FTY Head to Head --------------------------------------------------------

  # Reactive vals for H2H
  player_transactions <- reactiveVal(tibble())
  h2h_configurations <- reactiveVal(list("reset" = toJSON(list("h2h_competitor" = isolate(input$h2h_competitor), "h2h_matchup" = isolate(input$h2h_matchup)))))

  # Reactive H2H data creation -  NEEDS WORK!!!!
  df_h2h <- reactive(df_h2h_prepare(c_id = isolate(input$h2h_competitor))) |>
    bindEvent(input$h2h_window, input$h2h_competitor, input$h2h_dl_player_transaction, input$h2h_add_player_transaction, ignoreInit = TRUE)

  #---------- Observe Events for H2H
  # Additional H2H filter alteration
  observe({
    req(fty_parameters_met, exists("df_h2h"))

    competitor_players <- slice_max(df_h2h(), game_date, by = competitor_id) |>
      filter(competitor_id == input$h2h_competitor) |>
      pull(player_name) |>
      sort()

    #   updateSelectInput(session, "h2h_hl_player", choices = competitor_players)
    #   updateSelectInput(
    #     session,
    #     "transaction_player_name",
    #     choices = if (input$transaction_type) setdiff(active_players, competitor_players) else competitor_players
    #   )
  }) |>
    bindEvent(df_h2h())

  # # Add element to h2h_configurations reactiveValues list
  # observe({
  #   h2h_configurations[paste0("config_", input$h2h_snapshot_config)] <- toJSON(list(
  #     "h2h_competitor" = input$h2h_competitor,
  #     "h2h_matchup" = input$h2h_matchup,
  #     "h2h_future_only" = input$h2h_future_only,
  #     "h2h_hl_player" = input$h2h_hl_player,
  #     "team_transaction_table" = input$team_transaction_table
  #   ))
  #   h2h_configurations <- h2h_configurations[!duplicated(h2h_configurations)]
  #   updateSelectInput(session, "h2h_log_config", choices = h2h_configurations)
  # }) |>
  #   bindEvent(input$h2h_snapshot_config, ignoreInit = TRUE)

  # # Repopulate all widgets with logged configurations
  # observe({
  #   cfg <- h2h_configurations[paste0("config_", input$h2h_log_config)]
  #   updateSelectInput(session, "h2h_competitor", selected = cfg$h2h_competitor)
  #   updateSelectInput(session, "h2h_matchup", selected = cfg$h2h_matchup)
  #   updateSelectInput(session, "h2h_hl_player", cfg$h2h_hl_player)
  #   updateCheckboxInput(session, "h2h_future_only", value = cfg$h2h_future_only)
  #   player_transactions(cfg$team_transaction_table)
  # }) |>
  #   bindEvent(input$h2h_log_config, ignoreInit = TRUE)

  # # Add row to transaction table
  # observe({
  #   player_transaction <- tibble(
  #     transaction = factor(if_else(input$transaction_type, "Add", "Drop"), levels = c("Drop", "Add")),
  #     player = input$transaction_player_name,
  #     date = input$transaction_date
  #   )
  #   player_transactions(distinct(bind_rows(player_transactions(), player_transaction)))
  # }) |>
  #   bindEvent(input$h2h_add_player_transaction, ignoreInit = TRUE)

  # # Remove row from transaction table
  # observe({
  #   if (!is.null(input$team_transaction_table_rows_selected)) {
  #     player_transactions(player_transactions()[-input$team_transaction_table_rows_selected, ])
  #   }
  # }) |>
  #   bindEvent(input$h2h_dl_player_transaction, ignoreInit = TRUE)

  # #------- Output
  # output$team_transaction_table <- renderDT(datatable(
  #   player_transactions(),
  #   rownames = FALSE,
  #   colnames = NULL,
  #   filter = "none",
  #   selection = "single",
  #   options = lst(dom = "t", paging = FALSE, ordering = FALSE),
  # ))

  # output$h2h_stat_plot <- renderPlotly({
  #   req(fty_parameters_met(), exists("df_fty_base"))

  #   if (input$h2h_matchup < cur_matchup & input$h2h_future_only) {
  #     ggplotly(
  #       (ggplot() +
  #         theme_void() +
  #         geom_text(aes(x = 0, y = 0, label = "Future only dumbass...")))
  #     )
  #   } else {
  #     df_h <<- df_h2h()
  #     if (input$h2h_future_from_tomorrow) {
  #       df_h <- mutate(df_h, origin = if_else(origin == "today", "past", origin))
  #     }
  #     if (input$h2h_future_only) {
  #       df_h <- filter(df_h, origin != "past")
  #     }
  #     opp_id <- filter(df_h, matchup_period == input$h2h_matchup, competitor_id == as.numeric(input$h2h_competitor))$opponent_id[1]

  #     h2h_plt <- df_h |>
  #       # Need to join player injury status and coalesce it for add players (player_injury_status_temp).
  #       # Initially I tried doing this in h2h prep file, but it kept breaking other things
  #       left_join(
  #         select(df_stitch, player_fantasy_id, player_injury_status_temp = player_injury_status),
  #         by = join_by(player_fantasy_id)
  #       ) |>
  #       mutate(player_injury_status = coalesce(player_injury_status, player_injury_status_temp)) |>
  #       select(-player_injury_status_temp) |>
  #       filter(
  #         competitor_id %in% c(as.numeric(input$h2h_competitor), opp_id),
  #         matchup_period == input$h2h_matchup,
  #         scheduled_to_play == 1,
  #         player_injury_status %in% c("ACTIVE", "DAY_TO_DAY")
  #       ) |>
  #       pivot_longer(
  #         cols = cat_specs(vec = TRUE, incl_nba_cat = c("fta", "ftm", "fga", "fgm"), excl_nba_cat = c("fg_pct", "ft_pct")),
  #         names_to = "stat",
  #         values_to = "value"
  #       ) |>
  #       select(competitor_id, player_name, stat, value) |>
  #       summarise(value = sum(value, na.rm = TRUE), .by = c(competitor_id, player_name, stat)) |>
  #       (\(df_tmp) {
  #         bind_rows(
  #           # fg_pct
  #           filter(df_tmp, stat %in% c("fga", "fgm")) |>
  #             pivot_wider(names_from = stat, values_from = value) |>
  #             arrange(desc(fgm)) |>
  #             mutate(fg_pct = round(fgm / fga, 2)) |>
  #             summarise(
  #               # fmt: skip
  #               competitor_roster = str_c(player_name, " ", fg_pct, " (", round(fgm, 2), "/", round(fga, 2), ")", collapse = "\n"),
  #               value = sum(fgm) / sum(fga),
  #               .by = competitor_id
  #             ) |>
  #             mutate(stat = "fg_pct"),

  #           #ft_pct
  #           filter(df_tmp, stat %in% c("fta", "ftm")) |>
  #             pivot_wider(names_from = stat, values_from = value) |>
  #             arrange(desc(ftm)) |>
  #             mutate(ft_pct = round(ftm / fta, 2)) |>
  #             summarise(
  #               # fmt: skip
  #               competitor_roster = str_c(player_name, " ", ft_pct, " (", round(ftm, 2), "/", round(fta, 2), ")", collapse = "\n"),
  #               value = sum(ftm) / sum(fta),
  #               .by = competitor_id
  #             ) |>
  #             mutate(stat = "ft_pct"),

  #           # tov
  #           filter(df_tmp, stat == "tov") |>
  #             arrange(value) |>
  #             summarise(
  #               competitor_roster = paste(player_name, round(value), collapse = "\n"),
  #               value = sum(value),
  #               .by = c(competitor_id, stat)
  #             ),

  #           # the rest
  #           filter(df_tmp, !stat %in% c("fga", "fgm", "fta", "ftm", "tov")) |>
  #             arrange(desc(value)) |>
  #             summarise(
  #               competitor_roster = paste(player_name, round(value), collapse = "\n"),
  #               value = sum(value),
  #               .by = c(competitor_id, stat)
  #             )
  #         )
  #       })() |>
  #       left_join(
  #         filter(df_fty_base, platform == platform_selected, league_id == as.numeric(league_selected)) |>
  #           select(competitor_id, competitor_name),
  #         by = join_by(competitor_id)
  #       ) |>
  #       mutate(
  #         competitor_name = ordered(competitor_name, c(ls_fty_cid_to_name[as.character(opp_id)], ls_fty_cid_to_name[input$h2h_competitor])),
  #         stat = factor(stat, levels = cat_specs(vec = TRUE), ordered = TRUE)
  #       )

  #     (ggplot(h2h_plt, aes(x = stat, y = value, fill = competitor_name, text = paste(round(value, 2), "\n\n", competitor_roster))) +
  #       geom_col(position = "fill") +
  #       geom_hline(yintercept = 0.5) +
  #       scale_fill_brewer(type = "qual", palette = "Set2") +
  #       labs(
  #         # fmt: skip
  #         title = str_c(
  #           input$h2h_matchup, ": ",
  #           ls_fty_cid_to_name[input$h2h_competitor], " vs ", ls_fty_cid_to_name[as.character(opp_id)],
  #           x = NULL,
  #           y = NULL,
  #           fill = NULL
  #         )
  #       ) +
  #       theme_bw()) |>
  #       ggplotly(tooltip = "text") |>
  #       layout(hovermode = "x", legend = list(x = 100, y = 0.5)) |>
  #       config(displayModeBar = FALSE)
  #   }
  # })

  # output$h2h_game_table <- renderDT({
  #   req(fty_parameters_met(), exists("df_fty_base"))

  #   if (input$h2h_matchup < cur_matchup & input$h2h_future_only) {
  #     datatable(
  #       as.data.frame("Future only dumbass..."),
  #       rownames = FALSE,
  #       colnames = "",
  #       options = lst(dom = "t", paging = FALSE),
  #     )
  #   } else {
  #     df_h <<- df_h2h()

  #     if (input$h2h_future_from_tomorrow) {
  #       df_h <- mutate(df_h, origin = if_else(origin == "today", "past", origin))
  #     }
  #     if (input$h2h_future_only) {
  #       df_h <- filter(df_h, origin != "past")
  #     }
  #     opp_id <- filter(df_h, competitor_id == as.numeric(input$h2h_competitor), matchup_period == input$h2h_matchup)$opponent_id[1]

  #     df_h2h_matchup_game_count <<- df_h |>
  #       # Need to join player injury status and coalesce it for add players (player_injury_status_temp).
  #       # Initially I tried doing this in h2h prep file, but it kept breaking other things
  #       left_join(
  #         select(df_stitch, player_fantasy_id, player_injury_status_temp = player_injury_status),
  #         by = join_by(player_fantasy_id)
  #       ) |>
  #       mutate(player_injury_status = coalesce(player_injury_status, player_injury_status_temp)) |>
  #       select(-player_injury_status_temp) |>
  #       filter(competitor_id %in% c(as.numeric(input$h2h_competitor), opp_id), matchup_period == input$h2h_matchup) |>
  #       mutate(
  #         inj_status = case_when(
  #           scheduled_to_play == 1 &
  #             str_detect(player_injury_status, "^O|INJ") ~
  #             "1*",
  #           scheduled_to_play == 1 ~ "1",
  #           .default = NA_character_
  #         )
  #       ) |>
  #       arrange(game_date) |>
  #       pivot_wider(
  #         id_cols = c(competitor_id, opponent_id, player_team, player_name),
  #         names_from = game_date,
  #         values_from = inj_status
  #       ) |>
  #       (\(df) {
  #         inner_func <- function(x, nm) {
  #           filter(x, competitor_id == nm) |>
  #             mutate(player_team = "Total", player_name = str_trim(nm)) |>
  #             summarise(across(starts_with("20"), \(x) as.character(sum(x == "1", na.rm = TRUE))), .by = c(player_team, player_name))
  #         }

  #         bind_rows(
  #           inner_func(df, opp_id),
  #           inner_func(df, as.numeric(input$h2h_competitor)),
  #           setNames(as.data.frame(matrix(rep(NA, length(colnames(df))), nrow = 1)), colnames(df)),
  #           select(
  #             filter(df, competitor_id == as.numeric(input$h2h_competitor)),
  #             starts_with(c("player", "20"))
  #           ) |>
  #             arrange(player_name) |>
  #             mutate(across(starts_with("20"), \(x) as.character(x)))
  #         )
  #       })() |>
  #       select(-starts_with(c("competitor", "opponent"))) |>
  #       (\(df) {
  #         Ttl = as.data.frame(t(df)) |>
  #           mutate(across(everything(), \(x) ifelse(is.na(as.numeric(x)) | as.numeric(x) <= 10, as.numeric(x), 10))) |>
  #           summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |>
  #           t()

  #         mutate(df, Total = Ttl)
  #       })() |>
  #       mutate(Total = if_else(Total == 0 & is.na(player_team), NA, Total)) |>
  #       mutate(matchup_period = as.numeric(input$h2h_matchup)) |>
  #       left_join(
  #         select(df_matchup_game_count, matchup_period, team, following_matchup_period_games),
  #         by = join_by(player_team == team, matchup_period)
  #       ) |>
  #       select(-matchup_period, following_matchup_period_games) |>
  #       distinct()

  #     df_h2h_matchup_game_count_tbl <<- df_h2h_matchup_game_count |>
  #       select(
  #         starts_with("player"),
  #         all_of(sort(str_subset(colnames(df_h2h_matchup_game_count), "\\d"))),
  #         Total,
  #         `Next Matchup Period` = following_matchup_period_games,
  #         Team = player_team,
  #         Player = player_name
  #       ) |>
  #       rename_with(.fn = \(x) format(as.Date(x), "%a (%d/%m)"), .cols = starts_with("20")) |>
  #       mutate(
  #         Player = str_replace_all(
  #           Player,
  #           setNames(
  #             unlist(ls_fty_cid_to_name),
  #             map_chr(names(ls_fty_cid_to_name), \(x) paste0("^", x, "$"))
  #           )
  #         )
  #       )

  #     max_game_count <- max(df_h2h_matchup_game_count_tbl$Total, na.rm = TRUE)
  #     min_next_matchup_game_count <- min(df_h2h_matchup_game_count_tbl$`Next Matchup Period`, na.rm = TRUE)

  #     rowid_to_column(df_h2h_matchup_game_count_tbl) |>
  #       datatable(
  #         rownames = FALSE,
  #         escape = FALSE,
  #         style = "default",
  #         options = lst(
  #           dom = "t",
  #           paging = FALSE,
  #           ordering = FALSE,
  #           columnDefs = list(list(visible = FALSE, targets = "rowid")),
  #           initComplete = JS(
  #             "function(settings, json) {",
  #             "$(this.api().table().header()).css({'background-color': 'blue', 'color': 'white'});",
  #             "}"
  #           )
  #         )
  #       ) |>
  #       formatStyle(colnames(df_h2h_matchup_game_count_tbl), border = "1px solid #000") |>
  #       formatStyle("rowid", target = "row", backgroundColor = styleEqual(3, "grey")) |>
  #       formatStyle("Total", target = "cell", backgroundColor = styleEqual(max_game_count, "lightgreen")) |>
  #       formatStyle(c("Team", "Player"), backgroundColor = "azure") |>
  #       formatStyle("Next Matchup Period", target = "cell", backgroundColor = styleEqual(min_next_matchup_game_count, "#FFBBFF")) |>
  #       (\(dt) {
  #         cols <- str_subset(colnames(df_h2h_matchup_game_count_tbl), "\\(")
  #         for (col in cols) {
  #           dt <- formatStyle(dt, columns = col, target = "cell", backgroundColor = styleInterval(10, c(NA, "pink"))) |>
  #             formatStyle(columns = col, target = "cell", backgroundColor = styleEqual("1*", "pink"))
  #         }

  #         if (format(cur_date, "%a (%d/%m)") %in% cols) {
  #           dt <- formatStyle(
  #             dt,
  #             format(cur_date, "%a (%d/%m)"),
  #             target = "cell",
  #             backgroundColor = styleEqual(
  #               "1*",
  #               "pink",
  #               default = "lightyellow"
  #             )
  #           )
  #         }
  #         if (length(input$h2h_hl_player) > 0) {
  #           dt <- formatStyle(
  #             dt,
  #             "Player",
  #             target = "row",
  #             backgroundColor = styleEqual(input$h2h_hl_player, rep("#54FF9F", length(input$h2h_hl_player)))
  #           )
  #         }

  #         dt
  #       })()
  #   }
  # })

  # # NBA Player Comparison -------------------------------------------------------

  # Count how many events a player excels given the selection
  # THIS SHOULD BE REACTIVE, SO TOO SHOULD BE MAIN DF
  .calc_xl_at_count <- function(df) {
    df$xl_at_count <- 0

    for (category in unlist(cat_specs(h2h = FALSE)[input$comparison_excels_at_filter], use.names = FALSE)) {
      df$xl_at_count <- str_detect(df$`Excels At`, category) + df$xl_at_count
    }
    df
  }

  output$player_comparison_table <- renderReactable({
    req(fty_parameters_met(), exists("df_fty_base"))

    ls_inj <- df_nba_injuries |>
      filter(
        status != "Available",
        game_date <= cur_date,
        # game_date >= cur_date - days(7)
        game_date >= cur_date - days(input$comparison_window)
      ) |>
      left_join(select(df_nba_roster, nba_id = player_id, salary)) |>
      select(team_slug, game_date, matchup, player_name, salary) |>
      arrange(desc(game_date), desc(salary)) |>
      summarise(player_names = paste(player_name, collapse = ", "), .by = c(team_slug, game_date, matchup)) |>
      nest_by(team_slug) |>
      (\(df_t) setNames(df_t$data, df_t$team_slug))()

    # Some players have teams missing | Some players are missing (eg filter to one team)
    df_comparison <<- df_nba_player_box_score |>
      filter(
        game_date <= cur_date,
        game_date >= cur_date - days(input$comparison_window)
      ) |>
      summarise(
        across(any_of(cat_specs(vec = TRUE, incl_nba_cat = c("min", "fgm", "fga", "ftm", "fta"))), \(x) mean(x)),
        .by = c(player_id, player_name)
      ) |>
      mutate(across(where(is.numeric), \(x) replace_na(x, 0L))) |>
      calc_z_pcts() |>
      select(-matches("_pct$|f[g|t][m|a]")) |>
      (\(df_tmp) {
        left_join(
          df_tmp,
          {
            select(df_tmp, player_id, player_name, any_of(cat_specs(vec = TRUE, h2h = FALSE, excl_nba_cat = "min"))) |>
              mutate(across(any_of(cat_specs(vec = TRUE, h2h = FALSE, excl_nba_cat = "tov")), ~ round(scales::rescale(.x), 2))) |>
              mutate(tov = round((((tov * -1) - min(tov)) / (max(tov) - min(tov))) + 1, 2)) |>
              pivot_longer(cols = any_of(cat_specs(vec = TRUE, h2h = FALSE)), names_to = "stat") |>
              (\(df_tmp) {
                bind_rows(
                  slice_max(df_tmp, value, n = 3, by = c(player_id, player_name), with_ties = FALSE) |>
                    mutate(performance = "Excels At") |>
                    filter(value > 0),

                  slice_min(df_tmp, value, n = 3, by = c(player_id, player_name)) |>
                    mutate(performance = "Weak At")
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
      select(player_name, any_of(cat_specs(vec = TRUE, incl_nba_cat = c("fg_z", "ft_z", "min"), excl_nba_cat = c("ft_pct", "fg_pct"))), contains("at")) |>
      rename(any_of(unlist(cat_specs(h2h = FALSE))), Player = player_name) |>
      left_join(
        slice_max(df_nba_player_box_score, game_date, by = player_name) |>
          select(Player = player_name, Team = team_slug, player_availability, player_injury_status),
        by = join_by(Player)
      ) |>
      mutate(inj_status = str_extract(player_injury_status, "[O|INJ|DAY|SUS|NA].*")) |>
      mutate(
        inj_status = case_when(
          str_detect(inj_status, "^O|INJ$|NA$") ~ "(out)",
          inj_status == "SUSPENSION" ~ "(sus)",
          str_detect(inj_status, "DAY_TO_DAY|GTD") ~ "(d2d)",
          .default = ""
        )
      ) |>
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
    if (input$comparison_free_agent_filter) {
      df_comparison_table <- filter(df_comparison_table, player_availability == "free_agent")
    }
    if (!is_null(input$comparison_team_or_player_filter)) {
      df_comparison_table <- {
        if (input$comparison_team_or_player) {
          df_comparison_table |>
            filter(Team %in% input$comparison_team_or_player_filter)
        } else {
          df_comparison_table |>
            filter(str_detect(Player, paste0(input$comparison_team_or_player_filter, collapse = "|")))
        }
      }
    }

    if (!is_null(input$comparison_excels_at_filter)) {
      df_comparison_table <- df_comparison_table |>
        filter(str_detect(`Excels At`, paste0(input$comparison_excels_at_filter, collapse = "|")))
    }
    df_comparison_table <- select(df_comparison_table, -c(player_availability, ends_with("_status"))) |>
      arrange(desc(Minutes))

    reactable(
      df_comparison_table,
      pagination = FALSE,
      bordered = TRUE,
      highlight = TRUE,
      defaultSorted = list(Minutes = "desc", Player = "asc"),
      defaultSortOrder = "desc",
      defaultColDef = colDef(
        align = "left",
        minWidth = 120,
        headerStyle = list(background = "blue", color = "white"),
        format = colFormat(digits = 1)
      ),
      columns = list(
        "Minutes" = colDef(style = \(val) {
          if (val == max(df_comparison_table$Minutes)) {
            list(background = "lightgreen")
          } else {
            NULL
          }
        }),
        "3-pointers" = colDef(style = \(val) {
          if (val == max(df_comparison_table$`3-pointers`)) {
            list(background = "lightgreen")
          } else {
            NULL
          }
        }),
        "Points" = colDef(style = \(val) {
          if (val == max(df_comparison_table$Points)) {
            list(background = "lightgreen")
          } else {
            NULL
          }
        }),
        "Field Goal Z" = colDef(style = \(val) {
          if (val == max(df_comparison_table$`Field Goal Z`)) {
            list(background = "lightgreen")
          } else {
            NULL
          }
        }),
        "Free Throw Z" = colDef(style = \(val) {
          if (val == max(df_comparison_table$`Free Throw Z`)) {
            list(background = "lightgreen")
          } else {
            NULL
          }
        }),
        "Rebounds" = colDef(style = \(val) {
          if (val == max(df_comparison_table$Rebounds)) {
            list(background = "lightgreen")
          } else {
            NULL
          }
        }),
        "Assists" = colDef(style = \(val) {
          if (val == max(df_comparison_table$Assists)) {
            list(background = "lightgreen")
          } else {
            NULL
          }
        }),
        "Steals" = colDef(style = \(val) {
          if (val == max(df_comparison_table$Steals)) {
            list(background = "lightgreen")
          } else {
            NULL
          }
        }),
        "Blocks" = colDef(style = \(val) {
          if (val == max(df_comparison_table$Blocks)) {
            list(background = "lightgreen")
          } else {
            NULL
          }
        }),
        "Turnovers" = colDef(style = \(val) {
          if (val == min(df_comparison_table$Turnovers)) {
            list(background = "lightgreen")
          } else {
            NULL
          }
        }),
        "Player" = colDef(style = \(val) {
          if (str_detect(val, "\\(out\\)")) {
            list(background = "red")
          } else if (str_detect(val, "\\((d2d|sus)\\)")) {
            list(background = "pink")
          } else {
            list(background = "azure")
          }
        }),
        "Excels At" = colDef(style = \(val, ix) {
          xl_at <- df_comparison_table$xl_at_count[ix]
          if (xl_at == 1) {
            list(
              background = "#00CDCD",
              fontSize = "80%",
              whiteSpace = "pre-line"
            )
          } else if (xl_at == 2) {
            list(
              background = "#8DEEEE",
              fontSize = "80%",
              whiteSpace = "pre-line"
            )
          } else if (xl_at == 3) {
            list(
              background = "#00FFFF",
              fontSize = "80%",
              whiteSpace = "pre-line"
            )
          } else {
            list(fontSize = "80%", whiteSpace = "pre-line")
          }
        }),
        "Weak At" = colDef(style = list(fontSize = "80%", whiteSpace = "pre-line")),
        "player_availability" = colDef(show = FALSE),
        "player_injury_status" = colDef(show = FALSE),
        "inj_status" = colDef(show = FALSE),
        "player_colour" = colDef(show = FALSE),
        "xl_at_count" = colDef(show = FALSE)
      ),
      onClick = "expand",
      details = function(ix) {
        # format this table nicely somehow...
        tm <- df_comparison_table$Team[ix]
        if (is.na(tm)) {
          tags$div(tags$h2(class = "title", "Team not found..."))
        } else {
          tags$div(
            style = "margin-left: 10px; margin-top: 10px; margin-bottom: 30px;",
            tags$h2(
              class = "title",
              paste(tm, "Last", input$comparison_window, "Day Injury History: ")
            ),
            reactable(
              ls_inj[[tm]],
              pagination = FALSE,
              fullWidth = FALSE,
              defaultColDef = colDef(
                align = "left",
                minWidth = 120,
                headerStyle = list(background = "lightblue", color = "white")
              ),
              columns = list(player_names = colDef(minWidth = 1000))
            )
          )
        }
      }
    )
  })

  # NBA Schedule Table ------------------------------------------------------

  observe({
    selected_matchup_dates <<- ss_matchup[match(input$matchup_selection, matchup_drop_box_choices), ]
    date_input_value <- cur_date
    if (input$pin_date < selected_matchup_dates$matchup_start) {
      date_input_value <- selected_matchup_dates$matchup_start
    }
    if (input$pin_date > selected_matchup_dates$matchup_end) {
      date_input_value <- selected_matchup_dates$matchup_end
    }
    updateDateInput(session, "pin_date", value = date_input_value, min = selected_matchup_dates$matchup_start, max = selected_matchup_dates$matchup_end)
  }) |>
    bindEvent(input$matchup_selection)

  observe({
    tms <- tbl_schedule_grid[input$schedule_table_rows_current, ]$Team
    updateSwitchInput(session, "comparison_team_or_player", value = TRUE)
    later::later(
      \() updateSelectInput(session, "comparison_team_or_player_filter", choices = teams, selected = tms),
      delay = 0.05
    )
    show_toast(
      title = NULL,
      text = "Teams added to comparison...",
      position = "bottom-start",
      type = "info",
      timer = 2000
    )
  }) |>
    bindEvent(input$copy_teams, ignoreInit = TRUE)

  output$schedule_table <- renderDT({
    req(fty_parameters_met(), exists("df_fty_base"))

    # Prepare tables to be presented
    # tbl_schedule <<- tbl_matchup_games$data[[6]] |>
    tbl_schedule <- tbl_matchup_games$data[[match(input$matchup_selection, matchup_drop_box_choices)]] |>
      mutate(across(ends_with(")"), \(x) if_else(as.character(x) == "NULL", 0, 1))) |>
      mutate(across(c(contains("games"), Team), as.factor))

    ts_names <- tmp_names <- str_subset(colnames(tbl_schedule), "\\(")
    names(ts_names) <- names(tmp_names) <- str_sub(ts_names, end = 3)

    # Add condition for where years aren't equal
    for (ix in 1:length(ts_names)) {
      nm <- table(names(tmp_names)[1:ix])[names(tmp_names)[ix]]
      names(ts_names)[ix] <- paste0(nm[[1]], "_", names(nm))

      if (
        as.Date(paste0(year(selected_matchup_dates$matchup_start), "/", str_extract(tmp_names[ix], "\\d{2}/\\d{2}")), "%Y/%d/%m") ==
          selected_matchup_dates$matchup_end
      ) {
        wk_th <<- ix
      }

      if (
        !exists("wk_th") &&
          as.Date(paste0(year(selected_matchup_dates$matchup_start), "/", str_extract(tmp_names[ix], "\\d{2}/\\d{2}")), "%Y/%d/%m") ==
            selected_matchup_dates$matchup_end + days(1)
      ) {
        wk_th <<- ix - 1
      }
    }
    expected_elements <- c("1_Mon", "1_Tue", "1_Wed", "1_Thu", "1_Fri", "1_Sat", "1_Sun", "2_Mon", "2_Tue")
    ts_names <- discard(ts_names[expected_elements], is.na)

    # If a day is missing from ts_names add it in
    if (length(keep_at(ts_names, \(x) str_detect(x, "1_"))) < 7) {
      ms_dt_nm <- setdiff(
        keep(expected_elements, \(x) str_detect(x, "1_")),
        names(ts_names)
      )
      ms_dt_ix <- which(expected_elements == ms_dt_nm) - 1
      ms_dt <- (selected_matchup_dates$matchup_start + days(ms_dt_ix)) |>
        format("%a (%d/%m)") |>
        setNames(ms_dt_nm)

      ts_names <- append(ts_names, ms_dt, after = ms_dt_ix)
      tbl_schedule <- mutate(tbl_schedule, !!ms_dt := NA_real_, .after = ms_dt_ix + 1)
      wk_th <<- wk_th + 1
    }

    pin_index <- match(
      str_subset(colnames(tbl_schedule), format(input$pin_date, "%d/%m")),
      colnames(tbl_schedule)
    )
    pin_sum_cols <- if (input$pin_dir == "+") {
      pin_index:(wk_th + 1)
    } else {
      2:pin_index
    }

    tbl_schedule_grid <<- tbl_schedule |>
      rowwise() |>
      mutate(
        # `Games From Pin` = factor(sum(c_across(str_subset(ts_names, format(input$pin_date, "%d/%m")):ts_names[wk_th]), na.rm = TRUE)),
        `Games From Pin` = factor(sum(c_across(pin_sum_cols), na.rm = TRUE)),
        .before = "Following Matchup Period Games"
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
      formatStyle(
        columns = str_subset(ts_names, format(input$pin_date, "%d/%m")),
        backgroundColor = "lightyellow"
      ) |>
      (\(tb) {
        lvl <- 0:length(unique(tbl_schedule_grid$`Games From Pin`))
        col <- c("white", rev(RColorBrewer::brewer.pal(5, "Greens")))[lvl + 1]
        tb <- formatStyle(tb, columns = "Games From Pin", backgroundColor = styleEqual(levels = lvl, values = col))

        if (match(input$matchup_selection, matchup_drop_box_choices) < length(matchup_drop_box_choices)) {
          tb <-
            formatStyle(
              tb,
              columns = "Following Matchup Period Games",
              backgroundColor = styleEqual(
                levels = 0:tail(levels(tbl_schedule_grid$`Following Matchup Period Games`), 1),
                values = rev(RColorBrewer::brewer.pal(length(0:tail(levels(tbl_schedule_grid$`Following Matchup Period Games`), 1)), "Greens"))
              )
            ) |>
            formatStyle(
              columns = (ncol(tbl_schedule_grid) - 1):ncol(tbl_schedule_grid),
              backgroundColor = "lightgrey"
            )
        } else {
          tb <- formatStyle(tb, columns = "Following Matchup Period Games", backgroundColor = "lightgrey")
        }

        tb # return
      })()
  })

  # NBA Player Trend --------------------------------------------------------

  output$player_trend_plot <- renderPlotly({
    trend_selected_stat <- sym(input$trend_select_stat)

    if (is.null(input$trend_select_player)) {
      ggplotly(
        (ggplot() +
          theme_void() +
          geom_text(aes(x = 0, y = 0, label = "Select players")))
      )
    } else {
      df_trend <- (if (!input$this_season_trend_switch) {
        df_nba_player_box_score
      } else {
        filter(df_nba_player_box_score, season == cur_season)
      }) |>
        filter(player_name %in% input$trend_select_player) |>
        arrange(game_date) |>
        mutate(
          day_sequence = as.integer((game_date - min(game_date)) + 1),
          smooth = loess(
            replace_na({{ trend_selected_stat }}, 0) ~ day_sequence
          )$fitted,
          .by = c(season, player_name)
        ) |>
        (\(df) {
          bind_rows(
            df,
            summarise(
              df,
              game_date = max(game_date) + 1,
              .by = c(player_name, year_season_type)
            )
          )
        })() |>
        mutate(player_name = factor(player_name, input$trend_select_player))

      ggplotly(
        (ggplot(df_trend, aes(x = game_date, colour = player_name)) +
          geom_point(aes(y = {{ trend_selected_stat }}), alpha = 0.2) +
          geom_line(aes(y = smooth)) +
          scale_x_datetime(
            name = NULL,
            breaks = df_nba_season_segments$mid_date,
            labels = df_nba_season_segments$year_season_type
          ) +
          geom_vline(
            xintercept = as.numeric(df_nba_season_segments$begin_date),
            colour = "grey"
          ) +
          ylim(0, NA) +
          labs(
            title = names(keep(cat_specs(h2h = FALSE), \(x) x == input$draft_stat)),
            x = NULL,
            y = NULL
          ) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)))
      )
    }
  })

  # Draft Assistance --------------------------------------------------------

  # TRIED LIVE STREAMING IT, NO CHANCE. KEEP HERE TO TRY AGIAN IN FUTURE

  # stream_timer <- reactiveTimer(10000)
  # streaming <- reactiveVal(FALSE)
  # observe(streaming(!streaming())) |>
  #   bindEvent(input$draft_live_capture, ignoreInit = TRUE)

  # league_live <- espn_api$League(
  #   league_id = as.integer(95537),
  #   year = as.integer(2026),
  #   espn_s2 = "AEBlel7lXHFxGYh6V9PtPxp%2FGit736EKLhCFm9hV0KCpUzeXeC8sMSlke37b2%2BW2UEf32Gx3dcDd3NSIlaS07GWvDxBuQwitee77r%2FVGmDVjAD%2F065WPb4%2BAW%2FoJTA4G7sZhOkl4gFhfJ9f5i9iPfFiRbHiPS3PPYNl%2Fjhy41EJDgLauGSTOdEAG8tBSWvrsXgI4lPZl4Ds0p71RGK4nMcGwvqzlKggQPs%2Fm9wJsQZIF5jzvBG8WRwL7eq49tC%2BwFg3RwiT5PmVWmIap5d170mMC",
  #   swid = "{65841A9D-0A7D-4E82-A2BD-25B825CBF245}"
  # )

  # existing <- dh_getQuery(db_con, "SELECT * FROM util.draft_player_log")
  # name_match <- dh_getQuery(db_con, "SELECT * FROM util.nba_fty_name_match")

  # observe({
  #   stream_timer()
  #   # skip when input$draft_live_capture FALSE
  #   if (!streaming()) {
  #     return()
  #   }
  #   print("STREAMING!!!")
  #   league_draft_stream <- tibble(espn_name = map_chr(league_live$draft, \(pick) pluck(pick, "playerName"))) |>
  #     left_join(select(name_match, espn_name, player_name = nba_name)) |>
  #     distinct(player_name) |>
  #     na.omit()

  #   ingest <- bind_rows(existing, league_draft_stream) |>
  #     slice_max(base_bust, by = player_name, with_ties = FALSE)

  #   DBI::dbWriteTable(db_con, DBI::Id(schema = "util", table = "draft_player_log"), ingest, overwrite = TRUE)
  #   updateSelectInput(session, "draft_player_log", choices = active_players, selected = ingest$player_name)
  # })

  observe({
    req(exists("vec_player_log_stream"))

    if (length(unique(vec_player_log_stream$player_name)) < length(input$draft_player_log)) {
      nm <- tibble(player_name = setdiff(input$draft_player_log, vec_player_log_stream$player_name))
      dh_ingestData(db_con, nm, "util", "draft_player_log")
    } else if (length(unique(vec_player_log_stream$player_name)) > length(input$draft_player_log)) {
      nm <- str_replace_all(setdiff(vec_player_log_stream$player_name, input$draft_player_log), "'", "''")
      nm <- paste(nm, collapse = "', '")
      DBI::dbBegin(db_con)
      DBI::dbExecute(db_con, glue::glue("DELETE FROM util.draft_player_log WHERE player_name IN ('{nm}')"))
      DBI::dbCommit(db_con)
    }
    vec_player_log_stream <<- dh_getQuery(db_con, "SELECT * FROM util.draft_player_log")
  }) |>
    bindEvent(input$draft_player_log, ignoreNULL = FALSE, ignoreInit = TRUE)

  output$draft_stat_plot <- renderPlotly({
    # USED FOR USAGE RATE, etc... | Need to think about dd and td here...
    # df_team_overview <- filter(df_nba_team_box_score, season == prev_season) |>
    #   summarise(
    #     across(any_of(cat_specs(vec = TRUE, h2h = FALSE)), \(x) sum(x, na.rm = TRUE)),
    #     .by = c(season_type, team_abbreviation)
    #   )

    # Stat calc
    stat_calc <- if (input$draft_tot_avg_toggle) getFunction("sum") else getFunction("mean")

    df_overview <- df_nba_player_box_score |>
      filter(season == prev_season, !(is.na(player_id) | is.na(player_name))) |>
      summarise(
        across(any_of(cat_specs(vec = TRUE, h2h = FALSE)), \(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE), .names = "{.col}_cov"),
        across(any_of(cat_specs(vec = TRUE, h2h = FALSE)), \(x) stat_calc(x, na.rm = TRUE)),
        .by = c(player_id, player_name)
      ) |>
      calc_z_pcts()

    # User defined filters | Scale by minutes (if selected)
    df_overview <- filter(df_overview, min >= input$draft_min_filter)
    df_overview <- filter(df_overview, !player_name %in% input$draft_player_log)

    if (input$draft_scale_minutes) {
      df_overview <- df_overview |>
        mutate(across(any_of(cat_specs(vec = TRUE, h2h = FALSE)), \(x) x / min))
    }

    # Create df for plot
    max_min_players <- slice_max(df_overview, order_by = min, prop = 0.35)$player_id

    df_overview <- left_join(
      select(df_overview, -ends_with("_cov")) |>
        pivot_longer(any_of(cat_specs(vec = TRUE, h2h = FALSE)), names_to = "stat") |>
        filter(!str_detect(stat, "_pct")),

      select(df_overview, player_id, player_name, ends_with("_cov")) |>
        pivot_longer(ends_with("_cov"), names_to = "stat", values_to = "covariance") |>
        mutate(stat = str_remove(stat, "_cov")) |>
        mutate(stat = str_replace(stat, "_pct", "_z"))
    ) |>
      filter(
        stat %in% cat_specs(incl_nba_cat = c("min", "fg_z", "ft_z")),
        !(stat == input$draft_stat & covariance > input$draft_cov_filter)
      ) |>
      mutate(value = if_else(stat == "tov" & !player_id %in% max_min_players, NA, value)) |>
      mutate(
        rank = if_else(
          stat == "tov",
          dense_rank(value),
          dense_rank(desc(value))
        ),
        .by = stat
      ) |>
      mutate(top_cats = if_else(rank <= input$draft_top_n, stat, NA)) |>
      mutate(top_cats = na_if(paste(na.omit(top_cats), collapse = ", "), ""), .by = player_id) |>
      mutate(top_cat_count = str_count(top_cats, ",") + 1)

    # Stat selection and render plot
    plt <- df_overview |>
      filter(stat == input$draft_stat) |>
      slice_min(order_by = rank, n = input$draft_top_n) |>
      ggplot(aes(
        x = value,
        y = if (input$draft_stat == "tov") {
          reorder(player_name, -value)
        } else {
          reorder(player_name, value)
        },
        fill = ordered(top_cat_count),
        text = top_cats
      )) +
      geom_col() +
      guides(fill = guide_legend(title = "Other Category Count", reverse = TRUE)) +
      labs(
        # fmt: skip
        title = str_c(
          "Previous Seasion (", prev_season,"): ",
          ifelse(input$draft_tot_avg_toggle, "Total", "Average"), " ",
          names(keep(cat_specs(h2h=FALSE), \(x) x == input$draft_stat)), ifelse(input$draft_scale_minutes, " Scaled", "")
        ),
        x = NULL,
        y = NULL
      ) +
      theme_bw()

    ggplotly(plt, tooltip = "text") |>
      layout(legend = list(x = 100, y = 0.5)) |>
      reverse_legend_labels() |>
      config(displayModeBar = FALSE)
  })

  # # News Transactions -------------------------------------------------------

  # output$news_transactions <- renderDT({
  #   df_t <- df_nba_news |>
  #     arrange(desc(date)) |>
  #     rename_with(\(x) str_to_title(str_replace(x, "_", " ")))

  #   datatable(
  #     df_t,
  #     rownames = FALSE,
  #     class = "cell-border stripe",
  #     style = "default",
  #     filter = list(position = "top", clear = FALSE),
  #     options = list(
  #       paging = FALSE,
  #       autoWidth = TRUE,
  #       dom = 't',
  #       scrollX = TRUE,
  #       initComplete = JS(
  #         "function(settings, json) {",
  #         "$(this.api().table().header()).css({'background-color': 'blue', 'color': 'white'});",
  #         "}"
  #       )
  #     )
  #   ) |>
  #     formatStyle(
  #       columns = colnames(df_t),
  #       background = "white",
  #       color = "black"
  #     )
  # })
}
