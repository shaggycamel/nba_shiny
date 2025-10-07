serverLoadDatasets <- function(id, db_con, base_parameters, base_selections, fty_parameters) {
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
      fn_fty_league_overview <- source(here("data", "internal_data_processing", "fty_league_overview.R"))
      obj_fty_h2h <- source(here("data", "internal_data_processing", "fty_h2h.R"), local = TRUE)
      dfs_nba_schedule <- source(here("data", "internal_data_processing", "nba_schedule.R"), local = TRUE)
      df_nba_player_box_score <- source(here("data", "internal_data_processing", "nba_fty_stitch_up.R"), local = TRUE)

      # If no rosters are full, set fty_parameters$met to false
      if (nrow(df_fty_roster) == 0) {
        fty_parameters$met <- FALSE
      }

      # HOW TO RETURN THESE?
      print(objects())
      # print(dfs_nba_schedule)
      # print(df_nba_player_box_score)
      # print(fn_fty_league_overview)
      # print(obj_fty_h2h)

      # Stop loading page
      hidePageSpinner()
    }) |>
      bindEvent(fty_parameters$seq_cnt, ignoreInit = TRUE)
  })

  # ADD RETURN FUNCTION
}
