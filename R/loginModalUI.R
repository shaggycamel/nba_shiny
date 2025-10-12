# Login modal object
loginModalUI <- function(id, df_fty_base) {
  ns <- NS(id)

  tagList(
    modalDialog(
      tags$head(tags$style(HTML(".selectize-dropdown-content{min-width: 100%; box-sizing: border-box;}"))),
      selectizeInput(
        ns("fty_league_select"),
        label = NULL,
        choices = unique(df_fty_base$league_name),
        options = list(
          placeholder = "Select Fantasy League",
          onInitialize = I("function(){this.setValue('');}")
        ),
        width = "100%"
      ),
      selectizeInput(
        ns("fty_competitor_select"),
        label = NULL,
        choices = character(0),
        options = list(
          placeholder = "Select Fantasy Competitor",
          onInitialize = I("function(){this.setValue('');}")
        ),
        width = "100%"
      ),
      span(textOutput("login_messages"), style = "color:red"),
      footer = tagList(
        actionButton(
          ns("fty_abort"),
          label = NULL,
          icon = icon("square-xmark"),
          style = "color:#FFF; background-color:#CD3333; border-color:#2E6DA4"
        ),
        actionButton(
          ns("fty_dash_init"),
          "Kobeee!",
          style = "color:#FFF; background-color:#337AB7; border-color:#2E6DA4"
        )
      ),
      size = "m"
    )
  )
}
