loginModalServer <- function(id, df_fty_base, ls_fty_base, base_selections, fty_parameters_met) {
  moduleServer(id, function(input, output, session) {
    # Assign values to platform and league selected
    observe({
      updateSelectInput(inputId = "fty_competitor_select", choices = filter(df_fty_base, league_name == input$fty_league_select)$competitor_name)
      base_selections$platform_selected <- str_split(ls_fty_base[input$fty_league_select], "_")[[1]][1]
      base_selections$league_id_selected <- str_split(ls_fty_base[input$fty_league_select], "_")[[1]][2]
    }) |>
      bindEvent(input$fty_league_select, ignoreNULL = TRUE)

    # Login modal error hadling
    observe({
      if (input$fty_competitor_select == "" && input$fty_league_select == "") {
        output$login_messages <- renderText("Select a league, then select a competitor.")
      } else if (input$fty_competitor_select != "") {
        fty_parameters_met(TRUE)
        base_selections$competitor_name_selected <- input$fty_competitor_select
        removeModal()
        output$login_messages <- NULL
      } else {
        output$login_messages <- renderText("Select a competitor")
      }
    }) |>
      bindEvent(input$fty_dash_init)

    # Login modal progress into app
    observe({
      if (!fty_parameters_met()) {
        output$login_messages <- renderText("You gotta go fill the form at least once!")
      } else {
        removeModal()
        output$login_messages <- NULL
      }
    }) |>
      bindEvent(input$fty_abort)
  })
}
