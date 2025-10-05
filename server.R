# Libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(tibble)
library(plotly)
library(shinycssloaders)
library(jsonlite)


# Initialisation files ----------------------------------------------------

# only init python if running in shiny
# fmt: skip
if(Sys.info()["user"] == "shiny") source(here("R", "python_shiny_config.R"))
source(here("R", "useful.R"))

# modules
walk(list.files(here("modules"), full.names = TRUE), \(x) {
  cat(paste("Sourcing:", str_extract(x, "modules\\/\\w+.R"), "\n"))
  source(x)
})


# Server code -------------------------------------------------------------

server <- function(input, output, session) {
  # Variables ---------------------------------------------------------------

  nba_api <- reticulate::import("nba_api")
  espn_api <- reticulate::import("espn_api.basketball")

  db_con <- if (Sys.info()["user"] == "fred") dh_createCon("postgres") else dh_createCon("cockroach")
  # cur_date <<- strptime(Sys.time(), "%Y", tz = "EST")
  cur_date <- as.Date("2025-12-01") # DELETE
  cur_season <- nba_api$stats$library$parameters$Season$current_season
  prev_season <- nba_api$stats$library$parameters$Season$previous_season
  df_fty_base <- readRDS("fty_base.RDS")
  ls_fty_base <- as.list(deframe(distinct(df_fty_base, league_handle, league_name)))
  vec_player_log_stream <- dh_getQuery(db_con, "SELECT * FROM util.draft_player_log")

  # Reactive values
  base_selections <- reactiveValues()
  fty_parameters_met <- reactiveVal(FALSE)

  # Login -------------------------------------------------------------------

  # Update Dashboard with league title once a league has been selected
  output$navbar_title <- renderUI(span(input$fty_league_select))

  # Login to a league upon app init
  loginModalServer("login_modal", df_fty_base, ls_fty_base, base_selections, fty_parameters_met)
  showModal(loginModalUi("login_modal", df_fty_base))

  # In the case league is switched mid-use of app
  bindEvent(observe(showModal(loginModalUi("login_modal", df_fty_base))), input$fty_league_competitor_switch)

  # Load Datasets ----------------------------------------------------------

  # CREATE MODULE FOR LOADING DATASETS
}
