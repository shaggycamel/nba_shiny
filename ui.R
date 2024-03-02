

# Libraries ---------------------------------------------------------------

library(shiny)
library(bslib)


# Sidebar pages -----------------------------------------------------------

page_league_game_schedule <- layout_sidebar(
  sidebar = selectInput("week_selection", "Week", choices = character(0), selectize = FALSE),
  card(full_screen = TRUE, DT::DTOutput("schedule_table")),
  fillable = TRUE
)


# Main UI -----------------------------------------------------------------

ui <- page_navbar(
  title = "NBA Fantasy",
  nav_spacer(),
  nav_panel("NBA Game Schedule", page_league_game_schedule)
)
