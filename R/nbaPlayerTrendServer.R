nbaPlayerTrendServer <- function(id, base_parameters, fty_parameters, data_objs) {
  moduleServer(id, function(input, output, session) {
    observe({
      # UNCOMMENT EVENTUALLY
      # req(fty_parameters$met, fty_parameters$seq_cnt == 2)

      # updateSelectInput(session, "trend_select_player", choices = active_players)
      updateSelectInput(session, "trend_select_stat", choices = cat_specs(data_objs$fty$df_fty_cats, incl_nba_cat = "min"))
    }) |>
      bindEvent(fty_parameters$seq_cnt, ignoreInit = TRUE)

    output$player_trend_plot <- renderPlotly({
      trend_selected_stat <- sym(input$trend_select_stat)

      if (is.null(input$trend_select_player)) {
        ggplotly(
          (ggplot() +
            theme_void() +
            geom_text(aes(x = 0, y = 0, label = "Select players")))
        )
      } else {
        df_trend <- (if (!input$this_season_trend_switch) {
          data_objs$nba$df_nba_player_box_score
        } else {
          filter(data_objs$nba$df_nba_player_box_score, season == cur_season) |>
            mutate(year_season_type = fct_drop(year_season_type))
        }) |>
          filter(player_name %in% input$trend_select_player) |>
          arrange(game_date) |>
          mutate(
            day_sequence = as.integer((game_date - min(game_date)) + 1),
            smooth = loess(
              replace_na({{ trend_selected_stat }}, 0) ~ day_sequence
            )$fitted,
            .by = c(season, player_name)
          ) |>
          (\(df) {
            bind_rows(
              df,
              summarise(
                df,
                game_date = max(game_date) + 1,
                .by = c(player_name, year_season_type)
              )
            )
          })() |>
          mutate(player_name = factor(player_name, input$trend_select_player))

        ggplotly(
          (ggplot(df_trend, aes(x = game_date, colour = player_name)) +
            geom_point(aes(y = {{ trend_selected_stat }}), alpha = 0.2) +
            geom_line(aes(y = smooth)) +
            scale_x_datetime(
              name = NULL,
              breaks = df_nba_season_segments$mid_date,
              labels = df_nba_season_segments$year_season_type
            ) +
            geom_vline(
              xintercept = as.numeric(df_nba_season_segments$begin_date),
              colour = "grey"
            ) +
            ylim(0, NA) +
            labs(
              title = names(keep(cat_specs(h2h = FALSE), \(x) x == input$draft_stat)),
              x = NULL,
              y = NULL
            ) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)))
        )
      }
    })
  })
}
