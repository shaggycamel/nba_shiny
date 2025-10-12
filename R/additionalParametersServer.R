additionalParametersServer <- function(id, base_parameters, fty_parameters, data_objs) {
  moduleServer(id, function(input, output, session) {
    observe({
      # UNCOMMENT EVENTUALLY
      # req(fty_parameters$met, fty_parameters$seq_cnt == 2)

      # Alter cur_date if cur_date is greater than season
      base_parameters$cur_date <- if (
        !is.infinite(max(data_objs$fty$df_fty_schedule$matchup_end)) &&
          base_parameters$cur_date > max(data_objs$fty$df_fty_schedule$matchup_end)
      ) {
        max(data_objs$fty$df_fty_schedule$matchup_end)
      } else {
        base_parameters$cur_date
      }
      print("cur_date")

      # Current matchup period
      # NOT SURE WHAT IS GOING ON HERE - FIGURE OUT FIRST
      # base_parameters$cur_matchup <- df_matchup_game_count |>
      #   mutate(matchup_end = if_else(matchup_end - matchup_start < 6, matchup_start + 6, matchup_end)) |>
      #   filter(cur_date >= matchup_start, cur_date <= matchup_end) |>
      #   pull(matchup_period) |>
      #   unique()

      if (length(base_parameters$cur_matchup) == 0) {
        base_parameters$cur_matchup <- 1
      }
      print("cur_matchup")

      # NBA teams
      base_parameters$nba_teams <- sort(unique(data_objs$nba$df_nba_roster$team_slug))
      print("nba_teams")

      # CURRENTLY GIVES ERROR BECAUSE DF IS NULL
      # Active players
      # base_parameters$active_players <- sort_players_by_min_desc(filter(
      #   data_objs$nba$df_nba_player_box_score,
      #   player_injury_status != "NA" | is.na(player_injury_status)
      # ))
      # print("active_players")

      # CURRENTLY GIVES ERROR BECAUSE DF IS NULL
      # Free agents
      # base_parameters$free_agents <- sort_players_by_min_desc(filter(data_objs$nba$df_nba_player_box_score, player_availability == "free_agent"))
      # print("free_agents")

      # CURRENTLY GIVES ERROR BECAUSE DF IS NULL
      # # Fantasy competitor names and ids
      # base_parameters$ls_fty_name_to_cid <- as.list(deframe(distinct(data_objs$fty$df_fty_competitor, competitor_id, competitor_name)))
      # base_parameters$ls_fty_cid_to_name <- as.list(deframe(distinct(data_objs$fty$df_fty_competitor, competitor_name, competitor_id)))
      # base_selections$competitor_id_selected <- base_parameters$ls_fty_name_to_cid[[input$fty_competitor_select]]
      # print("names ids")

      # CURRENTLY GIVES ERROR BECAUSE DF IS NULL
      # ss_matchup <- distinct(data_objs$fty$df_fty_schedule, matchup_period, matchup_start, matchup_end) |> arrange(matchup_period)
      # matchup_drop_box_choices <- str_c("Matchup ", ss_matchup$matchup_period, ": (", ss_matchup$matchup_start, ")")
      # matchup_drop_box_choices <- setNames(str_extract(matchup_drop_box_choices, "\\d{4}.*-\\d{2}"), matchup_drop_box_choices)
      # # SHOULD BE AN UPDATE STATEMENT HERE....
      # print("other")
    }) |>
      bindEvent(fty_parameters$seq_cnt, ignoreInit = TRUE)
  })
}
