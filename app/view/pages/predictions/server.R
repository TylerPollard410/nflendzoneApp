box::use(
  box[export],
  dplyr,
  reactable,
  scales[number, percent],
  shiny,
  shinyWidgets[updateNoUiSliderInput],
  tibble[tibble],
  tidybayes[unnest_rvars],
  utils[modifyList],
)

box::use(
  app /
    logic /
    data /
    startup[
      game_data,
      team_strength_negbinom_summary,
      teams,
      teams_data
    ],
  app /
    logic /
    predictions /
    games /
    core[
      get_prediction_context,
      make_team_palettes,
      prepare_schedule_indices
    ],
  app /
    logic /
    predictions /
    games /
    plot_prep[
      make_joint_plot_prep,
      make_score_plot_prep
    ],
  app /
    logic /
    predictions /
    games /
    plots[
      build_joint_prob_plot,
      build_score_combo_plot,
      build_score_prob_plot
    ],
  app / logic / predictions / games / probabilities[compute_game_probabilities],
  app /
    logic /
    predictions /
    games /
    rvars[
      get_predicted_rvars,
      get_team_strength_rvars
    ],
  app / logic / utils / numbers[same_number],
  app / logic / utils / odds[prob_to_american_odds],
  app / view / shared / reactable_theme[bs_reactable_theme],
)

server <- function(id, dark_mode = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    pred_context <- get_prediction_context(team_strength_negbinom_summary)
    pred_games <- prepare_schedule_indices(game_data, teams) |>
      dplyr$filter(
        season == pred_context$predict_season,
        week == pred_context$predict_week
      )

    palettes <- make_team_palettes(
      teams_data,
      light_amount = 0.1
    )

    shiny$observe({
      shiny$req(nrow(pred_games) > 0)
      shiny$updateSelectInput(
        session = session,
        inputId = "game_select",
        choices = pred_games$game_id,
        selected = pred_games$game_id[[1]]
      )
    }) |>
      shiny$bindEvent(TRUE, once = TRUE)

    display_state <- shiny$reactiveVal(list(
      game_id = NULL,
      spread = 0,
      total = 45
    ))
    plot_flags <- shiny$reactive({
      list(
        show_spread_line = !isFALSE(input$show_spread_line),
        show_total_line = !isFALSE(input$show_total_line),
        show_prob_labels = !isFALSE(input$show_prob_labels)
      )
    })

    shiny$observe({
      state <- display_state()
      shiny$req(!is.null(state$game_id))
      shiny$req(identical(input$game_select, state$game_id))
      shiny$req(!is.null(input$spread_line_slider))

      if (same_number(state$spread, input$spread_line_slider)) {
        return()
      }

      display_state(modifyList(state, list(spread = input$spread_line_slider)))
    }) |>
      shiny$bindEvent(input$spread_line_slider, ignoreInit = TRUE)

    shiny$observe({
      state <- display_state()
      shiny$req(!is.null(state$game_id))
      shiny$req(identical(input$game_select, state$game_id))
      shiny$req(!is.null(input$total_line_slider))

      if (same_number(state$total, input$total_line_slider)) {
        return()
      }

      display_state(modifyList(state, list(total = input$total_line_slider)))
    }) |>
      shiny$bindEvent(input$total_line_slider, ignoreInit = TRUE)

    shiny$observe({
      shiny$req(input$game_select)

      game_row <- pred_games |>
        dplyr$filter(game_id == input$game_select) |>
        dplyr$slice_head(n = 1)
      shiny$req(nrow(game_row) == 1)

      spread_line_adj <- game_row$spread_line[[1]]
      total_line_adj <- game_row$total_line[[1]]

      shiny$freezeReactiveValue(input, "spread_line_slider")
      shiny$freezeReactiveValue(input, "total_line_slider")
      updateNoUiSliderInput(
        session = session,
        inputId = "spread_line_slider",
        range = c(spread_line_adj - 13, spread_line_adj + 13),
        value = spread_line_adj
      )
      updateNoUiSliderInput(
        session = session,
        inputId = "total_line_slider",
        range = c(total_line_adj - 13, total_line_adj + 13),
        value = total_line_adj
      )

      selected_game_pending <- input$game_select
      session$onFlushed(
        function() {
          if (
            !identical(shiny$isolate(input$game_select), selected_game_pending)
          ) {
            return()
          }
          display_state(list(
            game_id = selected_game_pending,
            spread = spread_line_adj,
            total = total_line_adj
          ))
        },
        once = TRUE
      )
    }) |>
      shiny$bindEvent(input$game_select, ignoreInit = FALSE)

    team_strength_rvars <- get_team_strength_rvars(
      team_strength_negbinom_summary,
      teams = teams
    )
    predicted_rvars <- get_predicted_rvars(
      pred_games,
      team_strength_rvars
    )

    game_rvars <- shiny$reactive({
      state <- display_state()
      shiny$req(!is.null(state$game_id))
      predicted_rvars |>
        dplyr$filter(game_id == state$game_id)
    })

    game_draws <- shiny$reactive({
      state <- display_state()
      shiny$req(!is.null(state$game_id))
      game_rvars() |>
        unnest_rvars()
    }) |>
      shiny$bindCache(display_state()$game_id) |>
      shiny$bindEvent(display_state()$game_id, ignoreInit = FALSE)

    game_probs <- shiny$reactive({
      state <- display_state()
      shiny$req(!is.null(state$game_id))
      compute_game_probabilities(
        game_rvars(),
        spread_line = state$spread,
        total_line = state$total
      )
    }) |>
      shiny$bindCache(
        display_state()$game_id,
        display_state()$spread,
        display_state()$total
      ) |>
      shiny$bindEvent(
        display_state()$game_id,
        display_state()$spread,
        display_state()$total,
        ignoreInit = FALSE
      )

    joint_plot_prep <- shiny$reactive({
      state <- display_state()
      shiny$req(!is.null(state$game_id))

      probs <- game_probs()
      shiny$req(!is.null(probs))
      draws <- game_draws()

      list(
        mu = make_joint_plot_prep(
          game_draws = draws,
          probs = probs,
          kind = "mu",
          spread_line_comp = state$spread,
          total_line_comp = state$total
        ),
        y = make_joint_plot_prep(
          game_draws = draws,
          probs = probs,
          kind = "y",
          spread_line_comp = state$spread,
          total_line_comp = state$total
        )
      )
    }) |>
      shiny$bindCache(
        display_state()$game_id,
        display_state()$spread,
        display_state()$total
      ) |>
      shiny$bindEvent(
        display_state()$game_id,
        display_state()$spread,
        display_state()$total,
        ignoreInit = FALSE
      )

    score_plot_prep <- shiny$reactive({
      state <- display_state()
      shiny$req(!is.null(state$game_id))

      probs <- game_probs()
      shiny$req(!is.null(probs))
      draws <- game_draws()

      list(
        mu = make_score_plot_prep(
          game_draws = draws,
          probs = probs,
          kind = "mu",
          spread_line_comp = state$spread,
          total_line_comp = state$total
        ),
        y = make_score_plot_prep(
          game_draws = draws,
          probs = probs,
          kind = "y",
          spread_line_comp = state$spread,
          total_line_comp = state$total
        )
      )
    }) |>
      shiny$bindCache(
        display_state()$game_id,
        display_state()$spread,
        display_state()$total
      ) |>
      shiny$bindEvent(
        display_state()$game_id,
        display_state()$spread,
        display_state()$total,
        ignoreInit = FALSE
      )

    prob_summary <- shiny$reactive({
      state <- display_state()
      shiny$req(!is.null(state$game_id))
      probs <- game_probs()
      shiny$req(!is.null(probs))
      home_team <- probs$home_team
      away_team <- probs$away_team
      probs_y <- probs$y

      out <- tibble(
        metric = c(
          paste0(home_team, " cover"),
          paste0(away_team, " cover"),
          "Over",
          "Under",
          paste0(home_team, " cover & Over"),
          paste0(home_team, " cover & Under"),
          paste0(away_team, " cover & Over"),
          paste0(away_team, " cover & Under")
        ),
        probability = c(
          probs_y$p_home_cover,
          probs_y$p_away_cover,
          probs_y$p_over,
          probs_y$p_under,
          probs_y$p_home_over,
          probs_y$p_home_under,
          probs_y$p_away_over,
          probs_y$p_away_under
        ),
        odds = prob_to_american_odds(probability, digits = 0L)
      )

      out |>
        dplyr$mutate(
          probability = percent(probability, accuracy = 0.1),
          odds = number(odds, style_positive = "plus")
        )
    })

    output$prob_summary <- reactable$renderReactable({
      reactable$reactable(
        prob_summary(),
        sortable = FALSE,
        pagination = FALSE,
        compact = TRUE,
        bordered = FALSE,
        highlight = TRUE,
        theme = bs_reactable_theme(),
        columns = list(
          metric = reactable$colDef(name = "Metric"),
          probability = reactable$colDef(name = "Probability"),
          odds = reactable$colDef(name = "Odds")
        )
      )
    })

    output$joint_mu_prob_plot <- shiny$renderPlot(
      {
        state <- display_state()
        flags <- plot_flags()
        plot_prep <- joint_plot_prep()$mu

        build_joint_prob_plot(
          plot_prep = plot_prep,
          kind = "mu",
          palettes = palettes,
          spread_line = state$spread,
          total_line = state$total,
          heatmap_shape = input$predictions_plot_shape,
          show_spread_line = flags$show_spread_line,
          show_total_line = flags$show_total_line,
          show_prob_labels = flags$show_prob_labels
        )
      },
      bg = "transparent"
    )

    output$joint_y_prob_plot <- shiny$renderPlot(
      {
        state <- display_state()
        flags <- plot_flags()
        shiny$req(!is.null(state$game_id))
        plot_prep <- joint_plot_prep()$y

        build_joint_prob_plot(
          plot_prep = plot_prep,
          kind = "y",
          palettes = palettes,
          spread_line = state$spread,
          total_line = state$total,
          heatmap_shape = input$predictions_plot_shape,
          show_spread_line = flags$show_spread_line,
          show_total_line = flags$show_total_line,
          show_prob_labels = flags$show_prob_labels
        )
      },
      bg = "transparent"
    )

    output$joint_score_mu_prob_plot <- shiny$renderPlot(
      {
        state <- display_state()
        flags <- plot_flags()
        plot_prep <- score_plot_prep()$mu

        if (identical(input$score_plot_mode, "combo")) {
          build_score_combo_plot(
            plot_prep = plot_prep,
            kind = "mu",
            palettes = palettes,
            spread_line = state$spread,
            total_line = state$total,
            heatmap_shape = input$predictions_plot_shape,
            show_spread_line = flags$show_spread_line,
            show_total_line = flags$show_total_line,
            show_prob_labels = flags$show_prob_labels
          )
        } else {
          build_score_prob_plot(
            plot_prep = plot_prep,
            kind = "mu",
            palettes = palettes,
            spread_line = state$spread,
            total_line = state$total,
            heatmap_shape = input$predictions_plot_shape,
            show_spread_line = flags$show_spread_line,
            show_total_line = flags$show_total_line,
            show_prob_labels = flags$show_prob_labels
          )
        }
      },
      bg = "transparent"
    )

    output$joint_score_y_prob_plot <- shiny$renderPlot(
      {
        state <- display_state()
        flags <- plot_flags()
        shiny$req(!is.null(state$game_id))
        plot_prep <- score_plot_prep()$y
        if (identical(input$score_plot_mode, "combo")) {
          build_score_combo_plot(
            plot_prep = plot_prep,
            kind = "y",
            palettes = palettes,
            spread_line = state$spread,
            total_line = state$total,
            heatmap_shape = input$predictions_plot_shape,
            show_spread_line = flags$show_spread_line,
            show_total_line = flags$show_total_line,
            show_prob_labels = flags$show_prob_labels
          )
        } else {
          build_score_prob_plot(
            plot_prep = plot_prep,
            kind = "y",
            palettes = palettes,
            spread_line = state$spread,
            total_line = state$total,
            heatmap_shape = input$predictions_plot_shape,
            show_spread_line = flags$show_spread_line,
            show_total_line = flags$show_total_line,
            show_prob_labels = flags$show_prob_labels
          )
        }
      },
      bg = "transparent"
    )

    session$onFlushed(
      function() {
        shiny$outputOptions(
          output,
          "joint_y_prob_plot",
          suspendWhenHidden = TRUE
        )
        shiny$outputOptions(
          output,
          "joint_mu_prob_plot",
          suspendWhenHidden = TRUE
        )
        shiny$outputOptions(
          output,
          "joint_score_y_prob_plot",
          suspendWhenHidden = TRUE
        )
        shiny$outputOptions(
          output,
          "joint_score_mu_prob_plot",
          suspendWhenHidden = TRUE
        )
      },
      once = TRUE
    )
  })
}

export(server)
