box::use(
  bslib,
  dplyr,
  reactable,
  scales[percent, number],
  shiny,
  shinyWidgets[noUiSliderInput, updateNoUiSliderInput, wNumbFormat],
  tibble[tibble],
  tidybayes[unnest_rvars],
  utils[modifyList],
)

box::use(
  app /
    logic /
    data_startup[teams_data, teams, game_data, team_strength_negbinom_summary],
  app / logic / predictions_games,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  bslib$card(
    bslib$card_header(
      shiny$div(
        style = "display:flex; align-items:center; justify-content:space-between; gap: 1rem;",
        shiny$h3("Game Predictions", style = "margin:0;"),
        shiny$selectInput(
          ns("game_select"),
          NULL,
          choices = character(0),
          selected = NULL,
          width = "200px", # Specified width helps alignment
          selectize = FALSE
        )
      )
    ),
    bslib$card_body(
      padding = "0.5rem",
      gap = "0.5rem",
      bslib$layout_sidebar(
        # 3. Assign an ID to the layout itself so JS can find it
        id = ns("lines_sidebar_layout"),
        padding = 0,
        sidebar = bslib$sidebar(
          title = "Lines",
          id = ns("lines_sidebar"),
          width = 400,
          noUiSliderInput(
            inputId = ns("spread_line_slider"),
            label = "Spread line",
            min = -21,
            max = 21,
            value = 0,
            step = 0.5,
            pips = list(
              mode = "count",
              values = 9,
              density = 3,
              format = wNumbFormat(decimals = 1),
              stepped = TRUE
            ),
            update_on = "end",
            color = "purple",
            format = wNumbFormat(decimals = 1),
            height = "10px"
          ),
          noUiSliderInput(
            inputId = ns("total_line_slider"),
            label = "Total line",
            min = 24,
            max = 66,
            value = 45,
            step = 0.5,
            pips = list(
              mode = "count",
              values = 9,
              density = 3,
              format = wNumbFormat(decimals = 1),
              stepped = TRUE
            ),
            update_on = "end",
            color = "purple",
            format = wNumbFormat(decimals = 1),
            height = "10px"
          )
        ),
        bslib$card(
          class = "predictions-summary",
          bslib$card_header("Cover Probability Summary"),
          reactable$reactableOutput(ns("prob_summary"), height = "100%")
        ),
        bslib$layout_column_wrap(
          width = 1 / 2,
          class = "predictions-plots",
          bslib$card(
            full_screen = TRUE,
            bslib$card_header("Simulated (y): Result/Total"),
            bslib$card_body(
              class = "predictions-plot",
              shiny$plotOutput(
                outputId = ns("joint_y_prob_plot"),
                height = "520px"
              )
            )
          ),
          bslib$card(
            full_screen = TRUE,
            bslib$card_header("Expected (μ): Result/Total"),
            bslib$card_body(
              class = "predictions-plot",
              shiny$plotOutput(
                outputId = ns("joint_mu_prob_plot"),
                height = "520px"
              )
            )
          ),
          bslib$card(
            full_screen = TRUE,
            bslib$card_header("Simulated (y): Home/Away"),
            bslib$card_body(
              class = "predictions-plot",
              shiny$plotOutput(
                outputId = ns("joint_score_y_prob_plot"),
                height = "520px"
              )
            )
          ),
          bslib$card(
            full_screen = TRUE,
            bslib$card_header("Expected (μ): Home/Away"),
            bslib$card_body(
              class = "predictions-plot",
              shiny$plotOutput(
                outputId = ns("joint_score_mu_prob_plot"),
                height = "520px"
              )
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, dark_mode = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    # plot_base_size <- function(width_px) {
    #   if (is.null(width_px) || is.na(width_px) || width_px <= 0) {
    #     width_px <- 900
    #   }
    #   base <- 11 + (width_px - 375) / (1200 - 375) * (18 - 11)
    #   max(11, min(18, base))
    # }

    # pt_to_mm <- function(pt) pt / 2.845276

    # is_dark_mode <- shiny$reactive({
    #   if (is.null(dark_mode)) {
    #     return(FALSE)
    #   }
    #   value <- dark_mode()
    #   if (is.null(value) || length(value) == 0L) {
    #     return(FALSE)
    #   }
    #   if (is.logical(value)) {
    #     return(isTRUE(value[[1]]))
    #   }
    #   if (is.character(value)) {
    #     return(tolower(value[[1]]) %in% c("dark", "1", "true", "yes", "on"))
    #   }
    #   FALSE
    # })

    # plots_style <- shiny$reactive({
    #   list(
    #     dark = is_dark_mode()
    #     # width_mu = session$clientData[[paste0(
    #     #   "output_",
    #     #   session$ns("joint_mu_prob_plot"),
    #     #   "_width"
    #     # )]],
    #     # width_y = session$clientData[[paste0(
    #     #   "output_",
    #     #   session$ns("joint_y_prob_plot"),
    #     #   "_width"
    #     # )]]
    #   )
    # }) |>
    #   # Coalesce dark-mode toggle + post-toggle width reflow to avoid
    #   # double-invalidating the plots.
    #   shiny$debounce(600)
    # filter_season <- shiny$reactiveVal(unique(
    #   team_strength_negbinom_summary$filtered_season
    # ))
    # filter_week <- shiny$reactiveVal(unique(
    #   team_strength_negbinom_summary$filtered_week
    # ))
    # predict_season <- shiny$reactiveVal(unique(
    #   team_strength_negbinom_summary$predicted_season
    # ))
    # predict_week <- shiny$reactiveVal(unique(
    #   team_strength_negbinom_summary$predicted_week
    # ))
    pred_context <- predictions_games$get_prediction_context(
      team_strength_negbinom_summary
    )
    pred_games <- predictions_games$prepare_schedule_indices(
      game_data,
      teams
    ) |>
      dplyr$filter(
        season == pred_context$predict_season,
        week == pred_context$predict_week
      )

    palettes <- predictions_games$make_team_palettes(
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

    same_number <- function(x, y) {
      if (is.null(x) || is.null(y)) {
        return(FALSE)
      }
      if (is.na(x) || is.na(y)) {
        return(is.na(x) && is.na(y))
      }
      isTRUE(all.equal(as.numeric(x), as.numeric(y)))
    }

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
      # shiny$updateSliderInput(
      #   session = session,
      #   inputId = "total_line_slider",
      #   min = max(0, total_line_adj - 14),
      #   max = total_line_adj + 14,
      #   value = total_line_adj,
      #   step = 0.5
      # )

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

    team_strength_rvars <- predictions_games$get_team_strength_rvars(
      team_strength_negbinom_summary,
      teams = teams
    )
    predicted_rvars <- predictions_games$get_predicted_rvars(
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
      predictions_games$compute_game_probabilities(
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
        mu = predictions_games$make_joint_plot_prep(
          game_draws = draws,
          probs = probs,
          kind = "mu",
          spread_line_comp = state$spread,
          total_line_comp = state$total
        ),
        y = predictions_games$make_joint_plot_prep(
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
        mu = predictions_games$make_score_plot_prep(
          game_draws = draws,
          probs = probs,
          kind = "mu",
          spread_line_comp = state$spread,
          total_line_comp = state$total
        ),
        y = predictions_games$make_score_plot_prep(
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
      probs_mu <- probs$mu
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
        odds = predictions_games$prob_to_american_odds(probability, digits = 0L)
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
        theme = reactable$reactableTheme(
          color = "var(--bs-emphasis-color, var(--bs-body-color))",
          backgroundColor = "var(--bs-body-bg)",
          borderColor = "var(--bs-border-color)",
          stripedColor = "var(--bs-tertiary-bg)",
          highlightColor = "var(--bs-secondary-bg)",
          headerStyle = list(
            fontWeight = 600,
            color = "var(--bs-emphasis-color, var(--bs-body-color))",
            backgroundColor = "var(--bs-tertiary-bg)"
          ),
          cellStyle = list(
            color = "var(--bs-emphasis-color, var(--bs-body-color))"
          )
        ),
        columns = list(
          metric = reactable$colDef(name = "Metric"),
          probability = reactable$colDef(name = "Probability"),
          odds = reactable$colDef(name = "Odds")
        )
      )
    })

    output$joint_mu_prob_plot <- shiny$renderPlot(
      {
        # shiny$req(input$team_game)
        # shiny$req(spread_line_rv(), total_line_rv())

        # game_id_sel <- input$team_game

        # game_rvars <- pred_rvars |>
        #   dplyr$filter(game_id == game_id_sel)

        # game_draws <- game_rvars |>
        #   tidybayes::unnest_rvars()

        state <- display_state()
        #shiny$req(!is.null(state$game_id))

        spread_line_use <- state$spread
        total_line_use <- state$total

        plot_prep <- joint_plot_prep()$mu
        #style <- plots_style()
        #shiny$req(isTRUE(style$width_mu > 0), isTRUE(style$width_y > 0))
        #base_size <- plot_base_size(style$width_mu)
        #label_text_size <- pt_to_mm(base_size) * as.numeric(ggplot2$rel(0.95))

        predictions_games$build_joint_prob_plot(
          plot_prep = plot_prep,
          kind = "mu",
          palettes = palettes,
          spread_line = spread_line_use,
          total_line = total_line_use
          #base_size = base_size,
          #label_text_size = label_text_size
        )
      },
      bg = "transparent"
    )

    # output$joint_y_prob_plot <- renderPlotly({
    #   plot_ly(
    #     data = joint_plot_prep()$y$plot_draws,
    #     type = "scatter",
    #     mode = "markers",
    #     x = ~pred_result,
    #     y = ~pred_total
    #   )
    # })
    output$joint_y_prob_plot <- shiny$renderPlot(
      {
        # shiny$req(input$team_game)
        # shiny$req(spread_line_rv(), total_line_rv())

        # game_id_sel <- input$team_game

        # game_rvars <- pred_rvars |>
        #   dplyr$filter(game_id == game_id_sel)

        # game_draws <- game_rvars |>
        #   tidybayes::unnest_rvars()
        state <- display_state()
        shiny$req(!is.null(state$game_id))

        spread_line_use <- state$spread
        total_line_use <- state$total

        plot_prep <- joint_plot_prep()$y
        #style <- plots_style()
        #shiny$req(isTRUE(style$width_mu > 0), isTRUE(style$width_y > 0))
        #base_size <- plot_base_size(style$width_y)
        #label_text_size <- pt_to_mm(base_size) * as.numeric(ggplot2$rel(0.95))

        predictions_games$build_joint_prob_plot(
          plot_prep = plot_prep,
          kind = "y",
          palettes = palettes,
          spread_line = spread_line_use,
          total_line = total_line_use
          #base_size = base_size,
          #label_text_size = label_text_size
        )
      },
      bg = "transparent"
    )

    output$joint_score_mu_prob_plot <- shiny$renderPlot(
      {
        state <- display_state()

        spread_line_use <- state$spread
        total_line_use <- state$total

        plot_prep <- score_plot_prep()$mu

        predictions_games$build_score_prob_plot(
          plot_prep = plot_prep,
          kind = "mu",
          palettes = palettes,
          spread_line = spread_line_use,
          total_line = total_line_use
        )
      },
      bg = "transparent"
    )

    output$joint_score_y_prob_plot <- shiny$renderPlot(
      {
        state <- display_state()
        shiny$req(!is.null(state$game_id))

        spread_line_use <- state$spread
        total_line_use <- state$total

        plot_prep <- score_plot_prep()$y

        predictions_games$build_score_prob_plot(
          plot_prep = plot_prep,
          kind = "y",
          palettes = palettes,
          spread_line = spread_line_use,
          total_line = total_line_use
        )
      },
      bg = "transparent"
    )
  })
}
