# Initialisation files ----------------------------------------------------

# Helper and Py config files
source(here("R", "useful.R"))
if (Sys.info()["user"] == "shiny") {
  source(here("R", "python_shiny_config.R"))
}

# Modules
# walk(list.files(here("modules"), full.names = TRUE), \(x) {
# cat(paste("Sourcing:", str_extract(x, "modules\\/\\w+.R"), "\n"))
# source(x)
# })
source("modules/serverLoginModal.R")
source("modules/uiLoginModal.R")
source("modules/serverLoadDatasets.R")
# source("modules/serverAdditionalParameters.R")

# Server code -------------------------------------------------------------

server <- function(input, output, session) {
  # Variables ---------------------------------------------------------------

  # DB connection and fantasy base dataframe
  db_con <- if (Sys.info()["user"] == "fred") dh_createCon("postgres") else dh_createCon("cockroach")
  df_fty_base <- readRDS("fty_base.RDS")

  # Convert to reactive val
  # vec_player_log_stream <- dh_getQuery(db_con, "SELECT * FROM util.draft_player_log")    # DELETE

  # Static base parameters referenced throughout
  base_parameters <- lst(
    # cur_date = strptime(Sys.time(), "%Y", tz = "EST"),
    cur_date = as.Date("2025-12-01"), # DELETE
    cur_season = reticulate::import("nba_api")$stats$library$parameters$Season$current_season,
    prev_season = reticulate::import("nba_api")$stats$library$parameters$Season$previous_season,
    ls_fty_base = as.list(deframe(distinct(df_fty_base, league_name, league_handle)))
  )

  # Reactive values referenced throughout
  base_selections <- reactiveValues()
  fty_parameters <- reactiveValues()
  # Create fty counter rv that logs the sequene of things

  # Login -------------------------------------------------------------------

  # Update Dashboard with league title once a league has been selected
  output$navbar_title <- renderUI(span(input$fty_league_select))

  # Login to a league upon app init
  serverLoginModal("kobeee", df_fty_base, base_parameters, base_selections, fty_parameters)
  showModal(uiLoginModal("kobeee", df_fty_base))

  # In the case league is switched mid-use of app
  bindEvent(observe(showModal(uiLoginModal("kobeee", df_fty_base))), input$fty_league_competitor_switch)

  # Internal Data Processing -----------------------------------------------

  # Load datasets
  # FOR SOME REASON base_selections IS NA WITHIN THE SERVER
  serverLoadDatasets("kobeee", db_con, base_parameters, base_selections, fty_parameters)

  # Create additional parameters and reactive values that rely on datasets
  # serverAdditionalParameters("additional_parameters")

  # FTY League Overview ----------------------------------------------------

  # # Update all widgets
  #     # League Overview tab
  #     updateSelectInput(
  #       session,
  #       "fty_lg_ov_cat",
  #       choices = list(
  #         "Overall" = cat_specs(h2h = FALSE)["All Categories"],
  #         "Categories" = cat_specs(),
  #         "Z Scores" = keep(cat_specs(h2h = FALSE), \(x) str_detect(x, "_z"))
  #       )
  #     )

  # # FTY Head to Head -------------------------------------------------------

  # # Update widgets
  #     # H2H tab
  #     updateSelectInput(session, "h2h_competitor", choices = ls_fty_name_to_cid, selected = ls_fty_name_to_cid[input$fty_competitor_select])
  #     updateSelectInput(session, "h2h_log_config", choices = h2h_configurations())
  #     updateSelectInput(
  #       session,
  #       "h2h_matchup",
  #       choices = ss_matchup$matchup_period,
  #       selected = if (
  #         !is.infinite(max(df_fty_schedule$matchup_period)) &&
  #           cur_matchup > max(df_fty_schedule$matchup_period)
  #       ) {
  #         max(df_fty_schedule$matchup_period)
  #       } else {
  #         cur_matchup
  #       }
  #     )

  # # NBA Player Comparison --------------------------------------------------

  #     # Player Comparison tab
  #     updateSelectInput(session, "comparison_team_or_player_filter", choices = teams)
  #     updateSelectizeInput(
  #       session,
  #       "comparison_excels_at_filter",
  #       choices = cat_specs(incl_nba_cat = c("min", "fg_z", "ft_z"), excl_nba_cat = c("fg_pct", "ft_pct"))
  #     )

  # # NBA Schedule -----------------------------------------------------------

  #   # NBA schedule tab
  #     updateSelectInput(session, "matchup_selection", choices = matchup_drop_box_choices, selected = pluck(matchup_drop_box_choices, cur_matchup))

  # # NBA Player Trend -------------------------------------------------------

  #     # Player trend tab
  #     updateSelectInput(session, "trend_select_player", choices = active_players)
  #     updateSelectInput(session, "trend_select_stat", choices = cat_specs(incl_nba_cat = "min"))

  # # FTY Draft Assistance ---------------------------------------------------

  #     # Draft tab
  #     updateSelectInput(session, "draft_player_log", choices = active_players, selected = vec_player_log_stream$player_name)
  #     updateSelectInput(session, "draft_stat", choices = cat_specs(incl_nba_cat = c("min", "fg_z", "ft_z"), excl_nba_cat = c("fg_pct", "ft_pct")))
}
