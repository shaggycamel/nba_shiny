

# Libraries ---------------------------------------------------------------

library(shiny)
library(bslib)


# Sidebar pages -----------------------------------------------------------

page_player_trend <- layout_sidebar(
  sidebar = sidebar(
    selectInput("trend_select_stat", "Statistic", choices = dplyr::filter(stat_selection, !stringr::str_detect(formatted_name, "Z"))$formatted_name),
    selectInput("trend_select_player", "Player", multiple = TRUE, choices = character(0)),
    checkboxInput("this_season_trend_switch", "This year only", value = FALSE)
  ),
  card(full_screen = TRUE, plotly::plotlyOutput("player_trend_plot")),
  fillable = TRUE
)

page_league_game_schedule <- layout_sidebar(
  sidebar = sidebar(
    selectInput("week_selection", "Week", choices = character(0), selectize = FALSE)
  ),
  card(full_screen = TRUE, DT::DTOutput("schedule_table")),
  fillable = TRUE
)


# Main UI -----------------------------------------------------------------

ui <- page_navbar(
  title = "NBA Fantasy",
  nav_spacer(),
  nav_panel("NBA Game Schedule", page_league_game_schedule),
  nav_panel("NBA Player Trend", page_player_trend)
)
