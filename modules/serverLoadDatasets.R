serverLoadDatasets <- function(id, db_con, base_parameters, base_selections, fty_parameters_met) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(fty_parameters_met())

      # Start loading page
      data_collection_caption <- "Processing data, one minute..."
      showPageSpinner(type = 6, caption = data_collection_caption)

      # Loading of datasets
      if (Sys.info()["user"] == "fred") {
        source(here("data", "base_frames.R"))
        base_frames(db_con, base_parameters)
      }
      load("nba_base.RData", envir = globalenv())
      print(str_c("fty_", base_selections$platform_selected, "_", base_selections$league_id_selected, ".RData"))
      load(
        here(str_c("fty_", base_selections$platform_selected, "_", base_selections$league_id_selected, ".RData")),
        envir = globalenv()
      )
      walk(list.files(here("data", "internal_data_processing"), full.names = TRUE), \(x) {
        cat(paste("Sourcing:", str_extract(x, "internal_data_processing\\/\\w+.R"), "\n"))
        source(x, local = TRUE)
      })

      # If no rosters are full, set fty_parameters_met to false
      if (nrow(df_fty_roster) == 0) {
        fty_parameters_met(FALSE)
      }

      # Stop loading page
      hidePageSpinner()
    }) |>
      bindEvent(input$fty_dash_init, fty_parameters_met(), ignoreInit = TRUE)
  })
}
