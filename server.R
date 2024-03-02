

# Libraries ---------------------------------------------------------------

library(DT)
library(here)
library(tidyr)
library(purrr)
library(lubridate)


# Init files --------------------------------------------------------------

source(here("_proj_useful.R"))


# Server code -------------------------------------------------------------

server <- function(input, output, session) {
  
  # Variables
  cur_date <<- as.Date(str_extract(as.POSIXct(Sys.time(), tz="NZ"), "\\d{4}-\\d{2}-\\d{2}")) - 1
  cur_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$current_season
  prev_season <<- reticulate::import("nba_api")$stats$library$parameters$Season$previous_season
  db_con <<- dh_createCon("postgres")
  
  # Data Frames
  source(here("data", "base_frames.R"))
  .load_datasets <- function() walk(list.files(here("data", "app_data_prep"), full.names = TRUE), \(x) source(x, local = TRUE))
  .load_datasets()
  
  # Extra variable that relies on datasets
  cur_week <<- df_week_game_count |>
    mutate(week_end = if_else(week_end - week_start < 6, week_start + 6, week_end)) |>
    filter(cur_date >= week_start, cur_date <= week_end) |>
    pull(season_week) |>
    unique()
  
  
  
  # Drop box choices
  week_drop_box_choices <- unique(paste0("Week: ", df_nba_schedule$season_week, " (", df_nba_schedule$week_start, " to ", df_nba_schedule$week_end, ")"))
  
  # Update drop box values
  observe({
    updateSelectInput(
      session, 
      "week_selection", 
      choices = week_drop_box_choices,
      selected = week_drop_box_choices[
        distinct(df_nba_schedule, pick(contains("week"))) |> 
          filter(season_week == cur_week) |>
          pull(season_week) + 1 # plus one because index starts at 1
      ]
    )
  })

  output$schedule_table <- renderDT({

    # Prepare tables to be presented
    tbl_schedule <- tbl_week_games$data[[match(input$week_selection, week_drop_box_choices)]] |>
      mutate(across(ends_with(")"), \(x) as.factor(if_else(as.character(x) == "NULL", ".", "1")))) |>
      mutate(across(c(contains("games"), Team), as.factor))

    ts_names <- str_subset(colnames(tbl_schedule), "\\(")
    names(ts_names) <- str_sub(ts_names, end = 3)
    names(ts_names)[length(ts_names)] <- paste0("2_", names(ts_names)[length(ts_names)])
    names(ts_names)[length(ts_names)-1] <- paste0("2_", names(ts_names)[length(ts_names)-1])
    ts_names <- discard(ts_names[c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "2_Mon", "2_Tue")], is.na)
    tbl_schedule <- select(tbl_schedule, Team, contains("games"), all_of(as.vector(ts_names)))

    # Present table
    DT::datatable(
      tbl_schedule,
      rownames = FALSE,
      class = "cell-border stripe",
      options = list(paging = FALSE, autoWidth = TRUE, dom = 't', scrollX = TRUE),
      filter = list(position = "top", clear = FALSE)
    ) |> 
    formatStyle(columns = "Team", backgroundColor = "lightblue") |>
    (\(tb){
      lvl <- 0:length(unique(tbl_schedule$`Week Games Remaining`))
      col <- c("white", rev(RColorBrewer::brewer.pal(5, "Greens")))[lvl + 1]
      formatStyle(tb, columns = "Week Games Remaining", backgroundColor = styleEqual(levels = lvl, values = col))
    })() |>
    formatStyle(
      columns = "Following Week Games",
      backgroundColor = styleEqual(levels = 0:tail(levels(tbl_schedule$`Following Week Games`), 1), values = rev(RColorBrewer::brewer.pal(length(0:tail(levels(tbl_schedule$`Following Week Games`), 1)), "Greens")))
    ) |> 
    formatStyle(columns = (length(tbl_schedule)-1):length(tbl_schedule), backgroundColor = "lightgrey")
    
  })
  
}
