loadDatasetsServer <- function(id, db_con, base_parameters, base_selections, fty_parameters) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(fty_parameters$met, fty_parameters$seq_cnt == 1)

      # Start loading page
      data_collection_caption <- "Processing data, one minute..."
      showPageSpinner(type = 6, caption = data_collection_caption)

      # Loading of datasets
      if (Sys.info()["user"] == "fred") {
        source(here("data", "base_frames.R"))
        base_frames(db_con, base_parameters)
      }
      load("nba_base.RData")
      load(here(str_c("fty_", base_selections$platform_selected, "_", base_selections$league_id_selected, ".RData")))
      # fn_fty_league_overview <- source(here("data", "internal_data_processing", "fty_league_overview.R"))
      # obj_fty_h2h <- source(here("data", "internal_data_processing", "fty_h2h.R"), local = TRUE)
      # dfs_nba_schedule <- source(here("data", "internal_data_processing", "nba_schedule.R"), local = TRUE)
      # df_nba_player_box_score <- source(here("data", "internal_data_processing", "nba_fty_stitch_up.R"), local = TRUE)

      # If no rosters are full, set fty_parameters$met to false
      if (nrow(df_fty_roster) == 0) {
        fty_parameters$met <- FALSE
      }
      fty_parameters$seq_cnt <- 2

      # Stop loading page
      hidePageSpinner()

      return(
        reactive({
          list(
            "fty" = lst(
              df_fty_box_score,
              df_fty_cats,
              df_fty_competitor,
              df_fty_free_agents,
              df_fty_roster,
              df_fty_schedule
            ),
            "nba" = lst(
              df_nba_injuries,
              df_nba_player_box_score,
              df_nba_roster,
              df_nba_schedule,
              df_nba_season_segments,
              df_nba_team_box_score
            )
          )
        })
      )
    }) |>
      bindEvent(fty_parameters$seq_cnt, ignoreInit = TRUE)
  })
}
