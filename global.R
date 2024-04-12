

# Common Libraries --------------------------------------------------------

library(nba.dataRub)
library(shiny)
library(DT)
library(plotly)
library(here)
library(purrr)
library(stringr)


# Database connection -----------------------------------------------------

db_con <<- if(Sys.info()["user"] == "fred") dh_createCon("postgres") else dh_createCon("cockroach") 


# Common objects ----------------------------------------------------------
# Variables ---------------------------------------------------------------

# cur_date <<- as.Date(str_extract(as.POSIXct(Sys.time(), tz="NZ"), "\\d{4}-\\d{2}-\\d{2}")) - 1
cur_date <<- as.Date("2024-02-26")
cur_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$current_season
prev_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$previous_season
 
df_fty_base <- dh_getQuery(db_con, "sql/fty_league_info.sql") 

# Project Constants -------------------------------------------------------

# Dataframe used to select NBA stats
stat_selection <- 
  tibble::tribble(
    ~formatted_name, ~database_name,
    "All Nine Categories", "nine_cat",
    "Top Five Categories", "five_cat",
     "Minutes", "min",
     "3-pointers", "fg3_m",
     "Points", "pts",
     "Field Goal %", "fg_pct",
     "Free Throw %", "ft_pct",
     "Field Goal Z", "fg_z",
     "Free Throw Z", "ft_z",
     "Rebounds", "reb",
     "Assists", "ast",
     "Steals", "stl",
     "Blocks", "blk",
     "Turnovers", "tov"
  )

# Useful lists of stat names
fmt_to_db_stat_name <- magrittr::`%$%`(stat_selection, purrr::map(setNames(database_name, formatted_name), \(x) as.vector(x)))
db_to_fmt_stat_name <- magrittr::`%$%`(stat_selection, purrr::map(setNames(formatted_name, database_name), \(x) as.vector(x)))

# Analysis columns
anl_cols <- list(
  stat_cols = c("min", "fgm", "fga", "fg_pct", "fg_z", "fg3_m", "fg3_a", "fg3_pct", "ft_pct", "ft_z", "ftm", "fta", "oreb", "dreb", "reb", "ast", "stl", "blk", "tov", "pf", "pts", "plus_minus"),
  h2h_cols = c("fg_pct", "fg3_m", "ft_pct", "reb", "ast", "stl", "blk", "tov", "pts")
)

