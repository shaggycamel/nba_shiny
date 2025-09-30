# Libraries ---------------------------------------------------------------

library(shinyWidgets)
library(bslib)
library(readr)


# Sidebar pages -----------------------------------------------------------
# FTY League Overview -----------------------------------------------------

page_fty_league_overview <- layout_sidebar(
  tags$head(tags$style(HTML(".selectize-dropdown-content{min-width: 100%; box-sizing: border-box;}"))),
  sidebar = sidebar(
    selectInput("fty_lg_ov_cat", "Category", choices = character(0)),
    switchInput("fty_lg_ov_rank_toggle", value = TRUE, onLabel = "Rank", offLabel = "Value", size = "small"),
    switchInput("fty_lg_ov_cum_toggle", value = TRUE, onLabel = "W2W", offLabel = "Cum", size = "small"),
    checkboxInput("fty_lg_ov_just_h2h", "Just H2H")
    # open = "open"
  ),
  card(full_screen = TRUE, plotlyOutput("fty_league_overview_rank_plot")),
  fillable = TRUE
)


# Head to Head ------------------------------------------------------------

page_h2h <- layout_sidebar(
  sidebar = sidebar(
    layout_columns(
      selectInput("h2h_competitor", "Competitor", choices = character(0)),
      selectInput("h2h_matchup", "Matchup", choices = 0)
    ),
    radioButtons("h2h_window", "Rolling days", c(7, 15, 30), inline = TRUE),
    layout_columns(
      selectInput("h2h_ex_player", "Exclude", choices = character(0), multiple = TRUE),
      selectInput("h2h_add_player", "Add", choices = character(0), multiple = TRUE),
    ),
    layout_columns(
      checkboxInput("h2h_future_only", "Future"),
      checkboxInput("h2h_future_from_tomorrow", "Tmrw")
    ),
    selectInput("h2h_hl_player", "Highlight Player", choices = character(0), multiple = TRUE),
    selectInput("h2h_log_config", "Log Filter Config", choices = character(0), size = 4, selectize = FALSE),
    actionButton("h2h_snapshot_config", "Snapshot config"),
    # width = 350,
    # open = "open"
  ),
  card(
    height = 1400,
    fill = FALSE,
    card(full_screen = TRUE, min_height = 500, max_height = 700, plotlyOutput("h2h_stat_plot")),
    card(full_screen = TRUE, min_height = 200, max_height = 650, DTOutput("h2h_game_table"))
  ),
  fillable = TRUE,
  tags$style(
    type = "text/css",
    ".selectize-dropdown-content{width: 200px;background-color: #FFFFFF; align: right;}"
  )
)

# Player Comparison -------------------------------------------------------

page_player_comparison <- layout_sidebar(
  sidebar = sidebar(
    switchInput("comparison_team_or_player", value = TRUE, onLabel = "Team", offLabel = "Player", size = "small"),
    selectInput("comparison_team_or_player_filter", NULL, choices = character(0), multiple = TRUE),
    selectizeInput(
      "comparison_excels_at_filter",
      "Excels at",
      choices = character(0),
      options = list(maxItems = 5, onInitialize = I('function() { this.setValue(""); }'))
    ),
    radioButtons("comparison_window", "Rolling days", c(7, 15, 30), inline = TRUE),
    sliderInput("comparison_minute_filter", "Minute Filter", min = 0, max = 50, value = 20, round = TRUE),
    checkboxInput("comparison_free_agent_filter", "Free Agents only", value = TRUE),
    # open = "open"
  ),
  card(full_screen = TRUE, reactableOutput("player_comparison_table")),
  fillable = TRUE
)


# League Game Schedule ----------------------------------------------------

page_league_game_schedule <- layout_sidebar(
  sidebar = sidebar(
    selectInput("matchup_selection", "Matchup", choices = character(0), selectize = FALSE),
    dateInput("pin_date", "Pinned Date"),
    radioButtons("pin_dir", label = "Pin Direction", choices = c("-", "+"), selected = "+", inline = TRUE),
    actionButton("copy_teams", "Copy teams to Comparison")
    # open = "open"
  ),
  card(full_screen = TRUE, DTOutput("schedule_table")),
  fillable = TRUE
)


# Player Trend ------------------------------------------------------------

page_player_trend <- layout_sidebar(
  sidebar = sidebar(
    selectInput("trend_select_stat", "Statistic", choices = character(0)),
    selectInput("trend_select_player", "Player", multiple = TRUE, choices = character(0)),
    checkboxInput("this_season_trend_switch", "This year only", value = FALSE),
    # open = "open"
  ),
  card(full_screen = TRUE, plotlyOutput("player_trend_plot")),
  fillable = TRUE,
)


# Draft Assistance --------------------------------------------------------

page_draft <- layout_sidebar(
  sidebar = sidebar(
    selectInput("draft_stat", "Statistic", choices = character(0)),
    sliderInput("draft_min_filter", "Limit Minutes", step = 1, min = 0, max = as.numeric(0), value = as.numeric(0), ticks = FALSE),
    sliderInput("draft_top_n", "Top N Players", min = 10, max = 20, value = 15, ticks = FALSE),
    sliderInput("draft_cov_filter", "Variance Coefficient", step = 0.01, min = as.numeric(0), max = as.numeric(0), value = as.numeric(0), ticks = FALSE),
    checkboxInput("draft_scale_minutes", "Scale by Minutes"),
    switchInput("draft_tot_avg_toggle", value = FALSE, onLabel = "Total", offLabel = "Mean", size = "large")
  ),
  card(full_screen = TRUE, plotlyOutput("draft_stat_plot")),
  absolutePanel(
    dropdownButton(
      # switchInput("draft_live_capture", value = FALSE, onLabel = "Stream", offLabel = "Off", size = "normal"),
      selectInput(
        "draft_player_log",
        label = NULL,
        choices = character(0),
        multiple = TRUE,
        width = "100%"
      ),
      width = "700px"
    ),
    right = 10,
    top = 10
  ),
  border_radius = FALSE,
  fillable = TRUE,
  class = "p-0"
)


# News Transactions -------------------------------------------------------

page_news <- card(full_screen = TRUE, DTOutput("news_transactions"))


# Info page ---------------------------------------------------------------

page_info <- card(
  h5("Data Update Frequency"),
  p(read_lines(here("data", "help_descriptions", "data_refresh.txt"))),
  br(),
)


# Main UI -----------------------------------------------------------------

ui <- page_navbar(
  # Move to respective page
  # tags$head(tags$style(HTML(".selectize-dropdown{z-index: 999}"))),
  # tags$head(tags$style(HTML(".selectize-dropdown-content{white-space: nowrap;}"))),
  id = "title_container",
  window_title = "NBA Fantasy",
  title = uiOutput("navbar_title"),
  nav_spacer(),
  nav_panel("Fantasy Overview", page_fty_league_overview),
  nav_panel("Head to Head", page_h2h),
  nav_panel("Player Comparison", page_player_comparison),
  nav_panel("Game Schedule", page_league_game_schedule),
  nav_panel("Player Trend", page_player_trend),
  nav_menu(
    "Misc.",
    nav_panel("Draft", page_draft),
    nav_panel("News", page_news),
    nav_panel("Info", page_info),
    nav_item(actionButton("fty_league_competitor_switch", "League", icon = icon("right-from-bracket"), width = "150px")),
    align = "right"
  ),
  theme = bs_theme(
    version = 5,
    preset = "litera",
    primary = "#133DEF"
  )
)
