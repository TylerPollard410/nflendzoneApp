box::use(
  shiny[
    NS,
    tagList,
    moduleServer,
    div,
    h3,
    p,
    selectInput,
    sliderInput,
    plotOutput,
    renderPlot,
    observe,
    bindEvent,
    updateSliderInput,
    reactive,
    req,
    br
  ],
  bslib[card, card_header, card_body, layout_sidebar, sidebar],
  ggplot2[
    ggplot,
    aes,
    geom_vline,
    geom_hline,
    geom_label,
    labs,
    theme,
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
  dplyr[filter, mutate, case_when, pull, distinct],
  tibble[tibble],
  tidybayes[unnest_rvars],
  posterior[Pr],
  scales[percent, label_number, breaks_pretty],
  stats[quantile],
)

box::use(
  app / logic / data_startup[teams, game_data, team_strength_negbinom_summary],
  #app / logic / data_import_functions[],
  app /
    logic /
    predictions_games[
      game_select_choices,
      spread_line_slider_init,
      total_line_slider_init,
      result_fill_values,
      total_fill_values,
      prepare_schedule_indices,
      get_team_strength_rvars,
      get_filtered_rvars,
      get_predicted_rvars
    ],
)

ui <- function(id, all_seasons, current_season, current_week) {
  ns <- NS(id)
  card(
    card_header(
      #div(
      #style = "display:flex; align-items:center; justify-content:space-between; gap: 1rem;",
      h3("Game Predictiions", style = "margin:0;"),
      selectInput(
        ns("game_select"),
        NULL,
        choices = game_select_choices,
        selected = game_select_choices[1],
        width = "auto"
      )
      #)
    ),
    card_body(
      layout_sidebar(
        sidebar = sidebar(
          sliderInput(
            inputId = ns("spread_line_slider"),
            label = "Spread line",
            min = spread_line_slider_init - 21,
            max = spread_line_slider_init + 21,
            value = spread_line_slider_init,
            step = 0.5
          ),
          sliderInput(
            inputId = ns("total_line_slider"),
            label = "Total line",
            min = total_line_slider_init - 21,
            max = total_line_slider_init + 21,
            value = total_line_slider_init,
            step = 0.5
          )
        ),
        plotOutput(outputId = ns("joint_mu_prob_plot")),
        br(),
        plotOutput(outputId = ns("joint_y_prob_plot"))
      )
    )
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
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
    pred_games <- prepare_schedule_indices(game_data, teams) |>
      filter(
        season == unique(team_strength_negbinom_summary$predicted_season),
        week == unique(team_strength_negbinom_summary$predicted_week)
      )

    # --- Reset sliders + reactive values when the game changes ----
    observe({
      req(input$game_select)

      spread_line_adj <- pred_games |>
        filter(game_id == input$game_select) |>
        pull(spread_line)

      updateSliderInput(
        inputId = "spread_line_slider",
        min = spread_line_adj - 21,
        max = spread_line_adj + 21,
        value = spread_line_adj,
        step = 0.5
      )
    }) |>
      bindEvent(input$game_select, ignoreInit = FALSE)

    observe({
      req(input$game_select)

      total_line_adj <- pred_games |>
        filter(game_id == input$game_select) |>
        pull(total_line)

      updateSliderInput(
        inputId = "total_line_slider",
        min = total_line_adj - 21,
        max = total_line_adj + 21,
        value = total_line_adj,
        step = 0.5
      )
    }) |>
      bindEvent(input$game_select, ignoreInit = FALSE)

    team_strength_rvars <- get_team_strength_rvars(
      team_strength_negbinom_summary
    )
    filtered_rvars <- get_filtered_rvars(team_strength_rvars)
    predicted_rvars <- get_predicted_rvars(pred_games, team_strength_rvars)

    one_game_rvars <- reactive({
      req(input$game_select)
      predicted_rvars |>
        filter(game_id == input$game_select)
    })

    one_game_draws <- reactive({
      one_game_rvars() |>
        unnest_rvars()
    })

    output$joint_mu_prob_plot <- renderPlot({
      # shiny::req(input$team_game)
      # shiny::req(spread_line_rv(), total_line_rv())

      # game_id_sel <- input$team_game

      # one_game_rvars <- pred_rvars |>
      #   filter(game_id == game_id_sel)

      # one_game_draws <- one_game_rvars |>
      #   tidybayes::unnest_rvars()

      one_game_rvars <- one_game_rvars()
      one_game_draws <- one_game_draws()

      spread_line_use <- input$spread_line_slider
      total_line_use <- input$total_line_slider

      one_game_rvars <- one_game_rvars |>
        mutate(
          p_home_over = Pr(
            mu_result > spread_line_use & mu_total > total_line_use
          ),
          p_home_under = Pr(
            mu_result > spread_line_use & mu_total < total_line_use
          ),
          p_away_over = Pr(
            mu_result < spread_line_use & mu_total > total_line_use
          ),
          p_away_under = Pr(
            mu_result < spread_line_use & mu_total < total_line_use
          ),
          p_home_cover = Pr(mu_result > spread_line_use),
          p_away_cover = Pr(mu_result < spread_line_use),
          p_over = Pr(mu_total > total_line_use),
          p_under = Pr(mu_total < total_line_use)
        )

      x_off_min <- quantile(
        one_game_rvars$mu_result,
        probs = 0.005,
        na.rm = TRUE
      )
      x_off_max <- quantile(
        one_game_rvars$mu_result,
        probs = 0.995,
        na.rm = TRUE
      )
      y_off_min <- quantile(
        one_game_rvars$mu_total,
        probs = 0.005,
        na.rm = TRUE
      )
      y_off_max <- quantile(
        one_game_rvars$mu_total,
        probs = 0.995,
        na.rm = TRUE
      )

      quad_labels <- tibble(
        x = c(x_off_max, x_off_max, x_off_min, x_off_min),
        y = c(y_off_max, y_off_min, y_off_max, y_off_min),
        label = c(
          paste0(
            one_game_rvars$home_team,
            " cover & Over:  ",
            percent(one_game_rvars$p_home_over, 0.1)
          ),
          paste0(
            one_game_rvars$home_team,
            " cover & Under: ",
            percent(one_game_rvars$p_home_under, 0.1)
          ),
          paste0(
            one_game_rvars$away_team,
            " cover & Over:  ",
            percent(one_game_rvars$p_away_over, 0.1)
          ),
          paste0(
            one_game_rvars$away_team,
            " cover & Under: ",
            percent(one_game_rvars$p_away_under, 0.1)
          )
        )
      )

      xside_labels <- tibble(
        x = c(x_off_min, x_off_max),
        y = Inf,
        label = c(
          paste0(
            one_game_rvars$away_team,
            " cover: ",
            percent(one_game_rvars$p_away_cover, 0.1)
          ),
          paste0(
            one_game_rvars$home_team,
            " cover: ",
            percent(one_game_rvars$p_home_cover, 0.1)
          )
        )
      )

      yside_labels <- tibble(
        y = c(y_off_min, y_off_max),
        x = Inf,
        label = c(
          paste0("Under: ", percent(one_game_rvars$p_under, 0.1)),
          paste0("Over:  ", percent(one_game_rvars$p_over, 0.1))
        )
      )

      one_game_draws |>
        mutate(
          result_bin = case_when(
            mu_result > spread_line_use ~ home_team,
            mu_result < spread_line_use ~ away_team,
            TRUE ~ "Push"
          ),
          total_bin = case_when(
            mu_total > total_line_use ~ "Over",
            mu_total < total_line_use ~ "Under",
            TRUE ~ "Push"
          )
        ) |>
        ggplot(aes(mu_result, mu_total)) +
        stat_bin_hex(binwidth = c(1, 1)) +
        scale_fill_viridis_c(
          breaks = breaks_pretty(6),
          labels = label_number(),
          name = "Count"
        ) +
        geom_vline(
          xintercept = spread_line_use,
          linetype = 2,
          color = "red"
        ) +
        geom_hline(
          yintercept = total_line_use,
          linetype = 2,
          color = "red"
        ) +
        geom_label(
          data = quad_labels,
          aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          size = 3
        ) +
        new_scale_fill() +
        geom_xsidehistogram(
          aes(y = after_stat(count), fill = result_bin),
          binwidth = 1,
          boundary = spread_line_use,
          alpha = 0.7
        ) +
        scale_fill_manual(values = result_fill_values) +
        new_scale_fill() +
        geom_ysidehistogram(
          aes(x = after_stat(count), fill = total_bin),
          binwidth = 1,
          boundary = total_line_use,
          alpha = 0.7
        ) +
        scale_fill_manual(values = total_fill_values) +
        geom_xsidetext(
          data = xside_labels,
          aes(x = x, y = Inf, label = label),
          inherit.aes = FALSE,
          vjust = 1.1,
          size = 3
        ) +
        geom_ysidetext(
          data = yside_labels,
          aes(x = Inf, y = y, label = label),
          inherit.aes = FALSE,
          hjust = 1.1,
          size = 3
        ) +
        facet_wrap(~game_id) +
        labs(x = "Result", y = "Total") +
        theme_minimal() +
        theme_ggside_void() +
        theme(
          ggside.axis.minor.ticks.length = unit(0, "pt"),
          ggside.panel.scale = 0.25,
          ggside.axis.line = element_blank(),
          ggside.axis.ticks = element_blank(),
          ggside.axis.text = element_blank(),
          legend.position = "none"
        )
    })

    output$joint_y_prob_plot <- renderPlot({
      # shiny::req(input$team_game)
      # shiny::req(spread_line_rv(), total_line_rv())

      # game_id_sel <- input$team_game

      # one_game_rvars <- pred_rvars |>
      #   filter(game_id == game_id_sel)

      # one_game_draws <- one_game_rvars |>
      #   tidybayes::unnest_rvars()
      one_game_rvars <- one_game_rvars()
      one_game_draws <- one_game_draws()

      spread_line_use <- input$spread_line_slider
      total_line_use <- input$total_line_slider

      one_game_rvars <- one_game_rvars |>
        mutate(
          p_home_over = Pr(
            y_result > spread_line_use & y_total > total_line_use
          ),
          p_home_under = Pr(
            y_result > spread_line_use & y_total < total_line_use
          ),
          p_away_over = Pr(
            y_result < spread_line_use & y_total > total_line_use
          ),
          p_away_under = Pr(
            y_result < spread_line_use & y_total < total_line_use
          ),
          p_home_cover = Pr(y_result > spread_line_use),
          p_away_cover = Pr(y_result < spread_line_use),
          p_over = Pr(y_total > total_line_use),
          p_under = Pr(y_total < total_line_use)
        )

      x_off_min <- quantile(
        one_game_rvars$y_result,
        probs = 0.005,
        na.rm = TRUE
      )
      x_off_max <- quantile(
        one_game_rvars$y_result,
        probs = 0.995,
        na.rm = TRUE
      )
      y_off_min <- quantile(
        one_game_rvars$y_total,
        probs = 0.005,
        na.rm = TRUE
      )
      y_off_max <- quantile(
        one_game_rvars$y_total,
        probs = 0.995,
        na.rm = TRUE
      )

      quad_labels <- tibble(
        x = c(x_off_max, x_off_max, x_off_min, x_off_min),
        y = c(y_off_max, y_off_min, y_off_max, y_off_min),
        label = c(
          paste0(
            one_game_rvars$home_team,
            " cover & Over:  ",
            percent(one_game_rvars$p_home_over, 0.1)
          ),
          paste0(
            one_game_rvars$home_team,
            " cover & Under: ",
            percent(one_game_rvars$p_home_under, 0.1)
          ),
          paste0(
            one_game_rvars$away_team,
            " cover & Over:  ",
            percent(one_game_rvars$p_away_over, 0.1)
          ),
          paste0(
            one_game_rvars$away_team,
            " cover & Under: ",
            percent(one_game_rvars$p_away_under, 0.1)
          )
        )
      )

      xside_labels <- tibble(
        x = c(x_off_min, x_off_max),
        y = Inf,
        label = c(
          paste0(
            one_game_rvars$away_team,
            " cover: ",
            percent(one_game_rvars$p_away_cover, 0.1)
          ),
          paste0(
            one_game_rvars$home_team,
            " cover: ",
            percent(one_game_rvars$p_home_cover, 0.1)
          )
        )
      )

      yside_labels <- tibble(
        y = c(y_off_min, y_off_max),
        x = Inf,
        label = c(
          paste0("Under: ", percent(one_game_rvars$p_under, 0.1)),
          paste0("Over:  ", percent(one_game_rvars$p_over, 0.1))
        )
      )

      one_game_draws |>
        mutate(
          result_bin = case_when(
            y_result > spread_line_use ~ home_team,
            y_result < spread_line_use ~ away_team,
            TRUE ~ "Push"
          ),
          total_bin = case_when(
            y_total > total_line_use ~ "Over",
            y_total < total_line_use ~ "Under",
            TRUE ~ "Push"
          )
        ) |>
        ggplot(aes(y_result, y_total)) +
        stat_bin_hex(binwidth = c(2, 2)) +
        scale_fill_viridis_c(
          breaks = breaks_pretty(6),
          labels = label_number(),
          name = "Count"
        ) +
        geom_vline(
          xintercept = spread_line_use,
          linetype = 2,
          color = "red"
        ) +
        geom_hline(
          yintercept = total_line_use,
          linetype = 2,
          color = "red"
        ) +
        geom_label(
          data = quad_labels,
          aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          size = 3
        ) +
        new_scale_fill() +
        geom_xsidehistogram(
          aes(y = after_stat(count), fill = result_bin),
          binwidth = 1,
          boundary = spread_line_use,
          alpha = 0.7
        ) +
        scale_fill_manual(values = result_fill_values) +
        new_scale_fill() +
        geom_ysidehistogram(
          aes(x = after_stat(count), fill = total_bin),
          binwidth = 1,
          boundary = total_line_use,
          alpha = 0.7
        ) +
        scale_fill_manual(values = total_fill_values) +
        geom_xsidetext(
          data = xside_labels,
          aes(x = x, y = Inf, label = label),
          inherit.aes = FALSE,
          vjust = 1.1,
          size = 3
        ) +
        geom_ysidetext(
          data = yside_labels,
          aes(x = Inf, y = y, label = label),
          inherit.aes = FALSE,
          hjust = 1.1,
          size = 3
        ) +
        # coord_cartesian(
        #   xlim = c(-21, 21),
        #   ylim = c(20, 70)
        # ) +
        facet_wrap(~game_id) +
        labs(x = "Result", y = "Total") +
        theme_minimal() +
        theme_ggside_void() +
        theme(
          ggside.axis.minor.ticks.length = unit(0, "pt"),
          ggside.panel.scale = 0.25,
          ggside.axis.line = element_blank(),
          ggside.axis.ticks = element_blank(),
          ggside.axis.text = element_blank(),
          legend.position = "none"
        )
    })
  })
}
