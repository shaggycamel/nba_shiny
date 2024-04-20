

# Initialise workspace ----------------------------------------------------

# renv::restore(clean = TRUE, prompt = FALSE)

# Initialise variables ----------------------------------------------------

cat("\nInitialising variables...\n")
prev_season <- reticulate::import("nba_api")$stats$library$parameters$Season$previous_season
cur_season <- reticulate::import("nba_api")$stats$library$parameters$Season$current_season
cur_date <- as.Date(stringr::str_extract(as.POSIXct(Sys.time(), tz="NZ"), "\\d{4}-\\d{2}-\\d{2}")) - 1
db_con <- nba.dataRub::dh_createCon("cockroach") 


# Read datasets -----------------------------------------------------------

cat("Creating datasets...\n")
source(here::here("data", "base_frames.R"))


# Save datasets -----------------------------------------------------------

cat("Saving image...\n")

nba.dataRub::dh_getQuery(db_con, "sql/fty_league_info.sql") |> 
  saveRDS(here::here("fty_base.RDS"))

rm(list = c("db_con", "cur_date", "cur_season", "prev_season"))
save.image()


# Upload to Shiny server --------------------------------------------------

cat("Deploying to Shiny Server...\n\n")
rsconnect::deployApp(
  appDir = here::here(),
  appFiles = c(
    "_proj_python.R", 
    "_proj_useful.R",
    "fty_base.RDS",
    ".RData",
    ".Rprofile",
    "data", 
    "renv.lock", 
    "requirements.txt", 
    "server.R", 
    "ui.R"
  ),
  appName = "FantasyNBA",
  appTitle = "FantasyNBA",
  appId = 10565372,
  account = "shaggycamel",
  server = "shinyapps.io",
  launch.browser = FALSE,
  logLevel = "normal",
  forceUpdate = TRUE,
  python = "/usr/bin/python3"
)

cat("Successfully deployed on:", format(as.POSIXct(Sys.time(), tz="NZ"), usetz=TRUE), "\n\n")


# Delete residual files ---------------------------------------------------

walk(c(".RData", "fty_base.RDS"), \(x) file.remove(here::here(x)))
