

# Libraries ---------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(bslib)

source(here::here("_proj_useful.R"))


# Sidebar pages -----------------------------------------------------------
# Head to Head ------------------------------------------------------------

page_h2h <- layout_sidebar(
  sidebar = sidebar(
    selectInput("h2h_competitor", "Competitor", choices = character(0)),
    selectInput("h2h_week", "Week", choices = 0),
    selectInput("h2h_ex_player", "Exclude Player", choices = character(0), multiple = TRUE),
    selectInput("h2h_add_player", "Add Player", choices = character(0), multiple = TRUE),
    checkboxInput("h2h_future_only", "Future games only"),
    checkboxInput("h2h_future_from_tomorrow", "From tomorrow")
  ),
  card(full_screen = TRUE, DT::DTOutput("h2h_game_table")),
  fillable = TRUE
)

# Player Comparison -------------------------------------------------------

page_player_comparison <- layout_sidebar(
  sidebar = sidebar(
    pickerInput("comparison_team_filter", "Team", choices = character(0), multiple = TRUE),
    pickerInput("comparison_excels_at_filter", "Excels at", choices = dplyr::filter(stat_selection, !stringr::str_detect(formatted_name, "%"))$formatted_name, multiple = TRUE, options =  list("max-options" = 5)),
    sliderInput("comparison_minute_filter", "Minute", min = 0, max = 50, value = 20, round = TRUE),
    radioButtons("date_range_switch", NULL, choices = c("Seven Days", "Two Weeks", "One Month")),
    checkboxInput("comparison_free_agent_filter", "Free Agents only"),
  ),
  card(full_screen = TRUE, DT::DTOutput("player_comparison_table")),
  fillable = TRUE
)


# League Game Schedule ----------------------------------------------------

page_league_game_schedule <- layout_sidebar(
  sidebar = sidebar(
    selectInput("week_selection", "Week", choices = character(0), selectize = FALSE),
    dateInput("pin_date", "Pinned Date")
  ),
  card(full_screen = TRUE, DT::DTOutput("schedule_table")),
  fillable = TRUE
)


# Player Trend ------------------------------------------------------------

page_player_trend <- layout_sidebar(
  sidebar = sidebar(
    selectInput("trend_select_stat", "Statistic", choices = dplyr::filter(stat_selection, !stringr::str_detect(formatted_name, "Z"))$formatted_name),
    selectInput("trend_select_player", "Player", multiple = TRUE, choices = character(0)),
    checkboxInput("this_season_trend_switch", "This year only", value = FALSE)
  ),
  card(full_screen = TRUE, plotly::plotlyOutput("player_trend_plot")),
  fillable = TRUE
)


# Main UI -----------------------------------------------------------------

ui <- page_navbar(
  title = "NBA Fantasy",
  nav_spacer(),
  nav_panel("Head to Head", page_h2h),
  nav_panel("Player Comparison", page_player_comparison),
  nav_panel("Game Schedule", page_league_game_schedule),
  nav_panel("Player Trend", page_player_trend)
)
