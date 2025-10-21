# Initialise workspace ----------------------------------------------------

# renv::restore(clean = TRUE, prompt = FALSE)

# Initialise variables ----------------------------------------------------

cat("\nInitialising variables...\n")
prev_season <- reticulate::import("nba_api")$stats$library$parameters$Season$previous_season
cur_season <- reticulate::import("nba_api")$stats$library$parameters$Season$current_season
cur_date <- lubridate::force_tz(as.Date(lubridate::with_tz(Sys.time(), "EST")), tz = "EST")
db_con <- nba.dataRub::dh_createCon("cockroach")
# db_con <- nba.dataRub::dh_createCon("postgres")

# Read datasets -----------------------------------------------------------

cat("Creating datasets...\n")
source(here::here("data", "base_frames.R"))


# Upload to Shiny server --------------------------------------------------

cat("Deploying to Shiny Server...\n\n")
rsconnect::deployApp(
  appDir = here::here(),
  appFiles = c(
    "_proj_python.R",
    "_proj_useful.R",
    "fty_base.RDS",
    unname(fs::dir_ls(glob = "*.RData")),
    ".Rprofile",
    "data",
    "renv.lock",
    "requirements.txt",
    "server.R",
    "ui.R",
    "global.R"
  ),
  appName = "FantasyNBA",
  appTitle = "FantasyNBA",
  appId = 10565372,
  account = "shaggycamel",
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = FALSE,
  logLevel = "normal",
  python = "/usr/bin/python3"
)

cat("Successfully deployed on:", format(as.POSIXct(Sys.time(), tz = "NZ"), usetz = TRUE), "\n\n")

# Delete residual files ---------------------------------------------------
# Not residual any more...
# purrr::walk(c(".RData", "fty_base.RDS"), \(x) file.remove(here::here(x)))
