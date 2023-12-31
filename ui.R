
# Libraries ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)
source(here::here("_proj_useful.R"))

# ui ----------------------------------------------------------------------

# Header
header <- dashboardHeader(title = "Fantasy NBA")
  
# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("News", tabName = "news_transactions", icon = icon("newspaper")),
    menuItem("Head to Head", tabName = "head_to_head", icon = icon("chess")),
    menuItem("Player Overview", tabName = "player_overview", icon = icon("chart-bar")),
    menuItem("Player Comparison", tabName = "player_comparison", icon = icon("basketball")),
    menuItem("League Game Schedule", tabName = "league_game_schedule", icon = icon("calendar-days")),
    menuItem("Player Trend", tabName = "player_trend", icon = icon("chart-line")),
    textAreaInput(inputId = "notepad", label = NULL, value = character(0), height = "100px", resize = "vertical"),
    fixedPanel(
      dropdownButton(
        inputId = "watch_list_button", 
        label = "Watch List",
        icon = icon("plus"),
        tooltip = TRUE,
        up = TRUE,
        selectInput(inputId = "watch_list", label = "Watch List", choices = 0, multiple = TRUE)
      ), 
      left=30, 
      bottom = 180
    )
  )
)
    
# Body
body <- 
  dashboardBody(
    tabItems(

# News (transaction feed) -----------------------------------------------------
      tabItem(tabName = "news_transactions", 
        box(
          title = paste0("News: NBA transactions made within the last two weeks"),
          width = NULL,
          height = 600,
          status = "primary", 
          solidHeader = TRUE,
          style = "overflow-x: scroll",
          DT::DTOutput("news_transactions", height = 530))
      ),


# Head 2 Head -------------------------------------------------------------

      tabItem(tabName = "head_to_head",
        fluidRow(
          column(
            width = 2, 
            selectInput("h2h_competitor", "Competitor", choices = character(0)),
            selectInput("h2h_week", "Week", choices = 0),
            selectInput("ex_player", "Exclude Player", choices = character(0), multiple = TRUE),
            selectInput("add_player", "Add Player", choices = character(0), multiple = TRUE),
            checkboxInput("future_only", "View future games only"),
            checkboxInput("future_from_tomorrow", "From tomorrow")
          ),
          column(
            width = 10, 
            plotly::plotlyOutput("h2h_plot", height = 650),
            gt::gt_output("game_count_table")
          ),
          
        )
      ),

# Player Overview ---------------------------------------------------------

      tabItem(tabName = "player_overview",
        fluidRow(
          column(
            width = 4,
            selectInput("overview_select_stat", "Statistic", choices = dplyr::filter(stat_selection, !stringr::str_detect(formatted_name, "%"))$formatted_name),
            sliderTextInput("overview_minute_filter", "Limit Minutes", choices = 0), # updated dynamically in server.R
            sliderInput("overview_slider_top_n", "Top N Players", min = 10, max = 20, value = 15, ticks = FALSE),
            switchInput("tot_avg_toggle", onLabel = "Total", offLabel = "Average", size = "small"),
            checkboxInput("this_season_overview_switch", "This year only", value = TRUE),
            checkboxInput("overview_scale_by_minutes", "Scale by Minutes"),
            checkboxInput("overview_free_agent_filter", "Only Show Non-Injured Free Agents")
          ),
        
          # Plot
          column(width = 8, plotly::plotlyOutput("player_overview_plot", height = 600)) # unsure how to make height dynamic, as in = "100%"
        )
      ),
     

# Player Performance ------------------------------------------------------

      tabItem(tabName = "player_comparison",
        fluidRow(
          column(
            width = 2, 
            radioButtons("date_range_switch", "Range", choices = c("Two Weeks", "One Month")),
            checkboxInput("performance_free_agent_filter", "Non injured Free Agents"),
            pickerInput("team_filter", "Team filter", choices = character(0), multiple = TRUE),
            pickerInput("excels_at_filter", "Excels At (one or more) filter", choices = dplyr::filter(stat_selection, !stringr::str_detect(formatted_name, "%"))$formatted_name, multiple = TRUE, options =  list("max-options" = 5)),
            selectInput("performance_select_player", "Player", multiple = TRUE, choices = character(0)),
          ),
        
          # Table
          column(width = 10, gt::gt_output("player_comparison_table"))
        )
      ),

# League Game Schedule ----------------------------------------------------

      tabItem(tabName = "league_game_schedule",
        fluidRow(
          column(
            width = 3, 
            selectInput("week_selection", "Week", choices = character(0), selectize = FALSE)
          ),
        
          # Table
          column(width = 9, DT::DTOutput("schedule_table"))
        ) 
      ),
      

# Player Trend ------------------------------------------------------------

      tabItem(tabName = "player_trend",
        fluidRow(
          column(
            width = 4, 
            selectInput("trend_select_stat", "Statistic", choices = dplyr::filter(stat_selection, !stringr::str_detect(formatted_name, "Z"))$formatted_name),
            selectInput("trend_select_player", "Player", multiple = TRUE, choices = character(0)),
            checkboxInput("this_season_trend_switch", "This year only", value = FALSE)
          ),
        
          # Plot
          column(width = 8, plotOutput("player_trend_plot", height = 600)) # unsure how to make height dynamic, as in = "100%"
        )
      )
    ),
  )


# Instantiate page --------------------------------------------------------

ui <- dashboardPage(header, sidebar, body)

