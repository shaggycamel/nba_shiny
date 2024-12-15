

# Libraries ---------------------------------------------------------------

library(shinyWidgets)
library(bslib)
library(readr)


# Sidebar pages -----------------------------------------------------------
# FTY League Overview -----------------------------------------------------

page_fty_league_overview <- layout_sidebar(
  sidebar = sidebar(
    selectInput(
      "fty_lg_ov_cat", 
      "Category", 
      choices = list(
        "Overall" = keep(fmt_to_db_stat_name, \(x) str_detect(x, "_cat")),
        "Categories" = keep(fmt_to_db_stat_name, \(x) x %in% anl_cols$h2h_cols),
        "Z Scores" = keep(fmt_to_db_stat_name, \(x) str_detect(x, "_z"))
      )
    ),
    switchInput("fty_lg_ov_rank_toggle", value = FALSE, onLabel = "Value", offLabel = "Rank", size = "small"),
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
      selectInput("h2h_week", "Week", choices = 0) 
    ),
    layout_columns(
      selectInput("h2h_ex_player", "Exclude", choices = character(0), multiple = TRUE),
      selectInput("h2h_add_player", "Add", choices = character(0), multiple = TRUE, width = "400px"),
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
    type="text/css",
    ".selectize-dropdown-content{width: 200px;background-color: #FFFFFF; align: right;}"
  )
)

# Player Comparison -------------------------------------------------------

page_player_comparison <- layout_sidebar(
  sidebar = sidebar(
    selectInput("comparison_team_filter", "Team", choices = character(0), multiple = TRUE),
    selectizeInput("comparison_excels_at_filter", "Excels at", choices = discard(fmt_to_db_stat_name, \(x) str_detect(x, "_pct|_cat")), options = list(maxItems = 5, onInitialize = I('function() { this.setValue(""); }'))),
    sliderInput("comparison_minute_filter", "Minute Filter", min = 0, max = 50, value = 20, round = TRUE),
    radioButtons("date_range_switch", NULL, choices = c("Seven Days", "Two Weeks", "One Month")),
    checkboxInput("comparison_free_agent_filter", "Free Agents only", value = TRUE),
    # open = "open"
  ),
  card(full_screen = TRUE, DTOutput("player_comparison_table")),
  fillable = TRUE
)


# League Game Schedule ----------------------------------------------------

page_league_game_schedule <- layout_sidebar(
  sidebar = sidebar(
    selectInput("week_selection", "Week", choices = character(0), selectize = FALSE),
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
    selectInput("trend_select_stat", "Statistic", choices = discard(fmt_to_db_stat_name, \(x) str_detect(x, "_z|_cat"))),
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
    selectInput(
      "draft_stat", 
      "Statistic", 
      choices = filter(stat_selection, !str_detect(database_name, "_pct|_cat"))$formatted_name
    ),
    sliderTextInput("draft_min_filter", "Limit Minutes", choices = 0), # updated dynamically in server.R
    sliderInput("draft_top_n", "Top N Players", min = 10, max = 20, value = 15, ticks = FALSE),
    checkboxInput("draft_scale_minutes", "Scale by Minutes"),
    switchInput("draft_tot_avg_toggle", value = TRUE, onLabel = "Total", offLabel = "Mean", size = "small"),
    # open = "open"
  ),
  card(full_screen = TRUE, plotlyOutput("draft_stat_plot")),
  fillable = TRUE,
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
  title = "NBA Fantasy",
  id = "title_container",
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

