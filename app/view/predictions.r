box::use(
  shiny[
    NS,
    tagList,
    moduleServer,
    div,
    tags,
    h3,
    p,
    selectInput,
    sliderInput,
    plotOutput,
    renderPlot,
    useBusyIndicators,
    observe,
    bindEvent,
    bindCache,
    freezeReactiveValue,
    updateSelectInput,
    updateSliderInput,
    reactive,
    reactiveVal,
    debounce,
    req,
    br,
    isolate
  ],
  bslib[
    card,
    card_header,
    card_body,
    layout_column_wrap
  ],
  reactable[reactable, renderReactable, reactableOutput],
  ggplot2[
    ggplot,
    aes,
    geom_vline,
    geom_hline,
    geom_label,
    labs,
    rel,
    theme,
    element_rect,
    element_blank,
    element_line,
    element_text,
    scale_fill_manual,
    scale_fill_viridis_c,
    stat_bin_hex,
    facet_wrap,
    theme_minimal,
    after_stat
  ],
  grid[unit],
  ggside[
    geom_xsidehistogram,
    geom_ysidehistogram,
    geom_xsidetext,
    geom_ysidetext,
    theme_ggside_void
  ],
  ggnewscale[new_scale_fill],
  plotly[plot_ly, plotlyOutput, renderPlotly, layout],
  dplyr[filter, mutate, case_when, pull, distinct, slice_head],
  tibble[tibble],
  tidybayes[unnest_rvars],
  scales[percent, label_number, breaks_pretty],
  utils[modifyList],
  thematic[thematic_get_option],
)

box::use(
  app /
    logic /
    data_startup[teams_data, teams, game_data, team_strength_negbinom_summary],
  #app / logic / data_import_functions[],
  app /
    logic /
    predictions_games[
      make_team_palettes,
      get_prediction_context,
      prepare_schedule_indices,
      get_team_strength_rvars,
      get_predicted_rvars,
      compute_game_probabilities,
      make_joint_plot_prep,
      build_joint_prob_plot,
      build_joint_prob_plot_int
    ],
)

ui <- function(id) {
  ns <- NS(id)
  card(
    useBusyIndicators(),
    card_header(
      div(
        style = "display:flex; align-items:center; justify-content:space-between; gap: 1rem;",
        h3("Game Predictiions", style = "margin:0;"),
        selectInput(
          ns("game_select"),
          NULL,
          choices = character(0),
          selected = NULL,
          width = "auto",
          selectize = TRUE
        )
      )
    ),
    card_body(
      div(
        class = "predictions-layout",
        layout_column_wrap(
          width = 1 / 2,
          sliderInput(
            inputId = ns("spread_line_slider"),
            label = "Spread line",
            min = -21,
            max = 21,
            value = 0,
            step = 0.5
          ),
          sliderInput(
            inputId = ns("total_line_slider"),
            label = "Total line",
            min = 0,
            max = 100,
            value = 45,
            step = 0.5
          )
        ),
        br(),
        div(
          class = "predictions-summary",
          reactableOutput(ns("prob_summary"))
        ),
        br(),
        layout_column_wrap(
          width = 1 / 2,
          class = "predictions-plots",
          card(
            full_screen = TRUE,
            card_header("Expected (μ)"),
            card_body(
              class = "predictions-plot",
              plotOutput(
                outputId = ns("joint_mu_prob_plot"),
                height = "520px"
              )
            )
          ),
          card(
            full_screen = TRUE,
            card_header("Simulated (y)"),
            card_body(
              class = "predictions-plot",
              plotOutput(
                outputId = ns("joint_y_prob_plot"),
                height = "520px"
              )
            )
          )
        )
      )
    )
  )
}

server <- function(id, dark_mode = NULL) {
  moduleServer(id, function(input, output, session) {
    # plot_base_size <- function(width_px) {
    #   if (is.null(width_px) || is.na(width_px) || width_px <= 0) {
    #     width_px <- 900
    #   }
    #   base <- 11 + (width_px - 375) / (1200 - 375) * (18 - 11)
    #   max(11, min(18, base))
    # }

    # pt_to_mm <- function(pt) pt / 2.845276

    # is_dark_mode <- reactive({
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

    # plots_style <- reactive({
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
    #   debounce(600)
    # filter_season <- reactiveVal(unique(
    #   team_strength_negbinom_summary$filtered_season
    # ))
    # filter_week <- reactiveVal(unique(
    #   team_strength_negbinom_summary$filtered_week
    # ))
    # predict_season <- ureactiveVal(nique(
    #   team_strength_negbinom_summary$predicted_season
    # ))
    # predict_week <- reactiveVal(unique(
    #   team_strength_negbinom_summary$predicted_week
    # ))
    pred_context <- get_prediction_context(team_strength_negbinom_summary)
    pred_games <- prepare_schedule_indices(game_data, teams) |>
      filter(
        season == pred_context$predict_season,
        week == pred_context$predict_week
      )

    palettes <- make_team_palettes(teams_data)

    observe({
      req(nrow(pred_games) > 0)
      updateSelectInput(
        session = session,
        inputId = "game_select",
        choices = pred_games$game_id,
        selected = pred_games$game_id[[1]]
      )
    }) |>
      bindEvent(TRUE, once = TRUE)

    display_state <- reactiveVal(list(
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

    observe({
      state <- display_state()
      req(!is.null(state$game_id))
      req(identical(input$game_select, state$game_id))
      req(!is.null(input$spread_line_slider))

      if (same_number(state$spread, input$spread_line_slider)) {
        return()
      }

      display_state(modifyList(state, list(spread = input$spread_line_slider)))
    }) |>
      bindEvent(input$spread_line_slider, ignoreInit = TRUE)

    observe({
      state <- display_state()
      req(!is.null(state$game_id))
      req(identical(input$game_select, state$game_id))
      req(!is.null(input$total_line_slider))

      if (same_number(state$total, input$total_line_slider)) {
        return()
      }

      display_state(modifyList(state, list(total = input$total_line_slider)))
    }) |>
      bindEvent(input$total_line_slider, ignoreInit = TRUE)

    # --- Reset sliders + derived state when the game changes ----
    observe({
      req(input$game_select)

      game_row <- pred_games |>
        filter(game_id == input$game_select) |>
        slice_head(n = 1)
      req(nrow(game_row) == 1)

      spread_line_adj <- game_row$spread_line[[1]]
      total_line_adj <- game_row$total_line[[1]]

      freezeReactiveValue(input, "spread_line_slider")
      freezeReactiveValue(input, "total_line_slider")
      updateSliderInput(
        session = session,
        inputId = "spread_line_slider",
        min = spread_line_adj - 21,
        max = spread_line_adj + 21,
        value = spread_line_adj,
        step = 0.5
      )
      updateSliderInput(
        session = session,
        inputId = "total_line_slider",
        min = max(0, total_line_adj - 21),
        max = total_line_adj + 21,
        value = total_line_adj,
        step = 0.5
      )

      selected_game_pending <- input$game_select
      session$onFlushed(
        function() {
          if (!identical(isolate(input$game_select), selected_game_pending)) {
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
      bindEvent(input$game_select, ignoreInit = FALSE)

    team_strength_rvars <- get_team_strength_rvars(
      team_strength_negbinom_summary,
      teams = teams
    )
    predicted_rvars <- get_predicted_rvars(pred_games, team_strength_rvars)

    game_rvars <- reactive({
      state <- display_state()
      req(!is.null(state$game_id))
      predicted_rvars |>
        filter(game_id == state$game_id)
    })

    game_draws <- reactive({
      state <- display_state()
      req(!is.null(state$game_id))
      game_rvars() |>
        unnest_rvars()
    }) |>
      bindCache(display_state()$game_id) |>
      bindEvent(display_state()$game_id, ignoreInit = FALSE)

    game_probs <- reactive({
      state <- display_state()
      req(!is.null(state$game_id))
      compute_game_probabilities(
        game_rvars(),
        spread_line = state$spread,
        total_line = state$total
      )
    }) |>
      bindCache(
        display_state()$game_id,
        display_state()$spread,
        display_state()$total
      ) |>
      bindEvent(
        display_state()$game_id,
        display_state()$spread,
        display_state()$total,
        ignoreInit = FALSE
      )

    joint_plot_prep <- reactive({
      state <- display_state()
      req(!is.null(state$game_id))

      probs <- game_probs()
      req(!is.null(probs))
      draws <- game_draws()

      list(
        mu = make_joint_plot_prep(
          game_draws = draws,
          probs = probs,
          kind = "mu",
          spread_line = state$spread,
          total_line = state$total
        ),
        y = make_joint_plot_prep(
          game_draws = draws,
          probs = probs,
          kind = "y",
          spread_line = state$spread,
          total_line = state$total
        )
      )
    }) |>
      bindCache(
        display_state()$game_id,
        display_state()$spread,
        display_state()$total
      ) |>
      bindEvent(
        display_state()$game_id,
        display_state()$spread,
        display_state()$total,
        ignoreInit = FALSE
      )

    prob_summary <- reactive({
      state <- display_state()
      req(!is.null(state$game_id))
      probs <- game_probs()
      req(!is.null(probs))
      home_team <- probs$home_team
      away_team <- probs$away_team
      probs_mu <- probs$mu
      probs_y <- probs$y

      tibble(
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
        probability = percent(
          c(
            probs_y$p_home_cover,
            probs_y$p_away_cover,
            probs_y$p_over,
            probs_y$p_under,
            probs_y$p_home_over,
            probs_y$p_home_under,
            probs_y$p_away_over,
            probs_y$p_away_under
          ),
          accuracy = 0.1
        )
      )
    })

    output$prob_summary <- renderReactable({
      reactable(
        prob_summary(),
        sortable = FALSE,
        pagination = FALSE,
        compact = TRUE,
        bordered = TRUE,
        highlight = TRUE,
        theme = reactable::reactableTheme(
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
        )
      )
    })

    output$joint_mu_prob_plot <- renderPlot(
      {
        # shiny::req(input$team_game)
        # shiny::req(spread_line_rv(), total_line_rv())

        # game_id_sel <- input$team_game

        # game_rvars <- pred_rvars |>
        #   filter(game_id == game_id_sel)

        # game_draws <- game_rvars |>
        #   tidybayes::unnest_rvars()

        state <- display_state()
        #req(!is.null(state$game_id))

        spread_line_use <- state$spread
        total_line_use <- state$total

        plot_prep <- joint_plot_prep()$mu
        #style <- plots_style()
        #req(isTRUE(style$width_mu > 0), isTRUE(style$width_y > 0))
        #base_size <- plot_base_size(style$width_mu)
        #label_text_size <- pt_to_mm(base_size) * as.numeric(rel(0.95))

        build_joint_prob_plot(
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
    output$joint_y_prob_plot <- renderPlot(
      {
        # shiny::req(input$team_game)
        # shiny::req(spread_line_rv(), total_line_rv())

        # game_id_sel <- input$team_game

        # game_rvars <- pred_rvars |>
        #   filter(game_id == game_id_sel)

        # game_draws <- game_rvars |>
        #   tidybayes::unnest_rvars()
        state <- display_state()
        req(!is.null(state$game_id))

        spread_line_use <- state$spread
        total_line_use <- state$total

        plot_prep <- joint_plot_prep()$y
        #style <- plots_style()
        #req(isTRUE(style$width_mu > 0), isTRUE(style$width_y > 0))
        #base_size <- plot_base_size(style$width_y)
        #label_text_size <- pt_to_mm(base_size) * as.numeric(rel(0.95))

        build_joint_prob_plot(
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
  })
}
