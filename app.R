
library(shiny)
library(bslib)
# library(crosstalk) # not sure how to use
library(plotly)

cars <- tibble::as_tibble(mtcars)
arrests <- tibble::as_tibble(datasets::USArrests)

page_cars <- layout_sidebar(
  sidebar = selectInput("cars_filt", "Cylinder", unique(cars$cyl)),
  card(
    ull_screen = TRUE, 
    card_header("Cars"), 
    plotOutput("cars")
  ),
  fillable = TRUE
)

page_dmds <- layout_sidebar(
  sidebar = sliderInput("arrest_filt", "Pop", min=0, max=100, value=50),
  card(
    full_screen = TRUE, 
    card_header("Diamonds"), 
    plotOutput("dmds")
  ),
  fillable = TRUE
)

ui <- page_navbar(
  title = "NBA Fantasy",
  nav_spacer(),
  nav("Cars", page_cars),
  nav("Diamonds", page_dmds),
)


server <- function(input, output) {
  output$cars <- renderPlot({
    df <- filter(cars, cyl == as.numeric(input$cars))
    ggplot(df, aes(x = disp, y = hp))
      + geom_point()
  })
  
  output$dmds <- renderPlot(ggplot(dmds, aes(x = depth, y = price)) + geom_point())
}

shinyApp(ui, server)