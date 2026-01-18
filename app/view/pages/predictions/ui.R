box::use(
  box[export],
  bslib,
  reactable,
  shiny,
  shinyWidgets[noUiSliderInput, wNumbFormat],
)

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
          width = "200px",
          selectize = FALSE
        )
      )
    ),
    bslib$card_body(
      padding = "0.5rem",
      gap = "0.5rem",
      bslib$layout_sidebar(
        id = ns("lines_sidebar_layout"),
        padding = 0,
        sidebar = bslib$sidebar(
          title = "Lines",
          id = ns("lines_sidebar"),
          width = 400,
          shiny$radioButtons(
            inputId = ns("score_plot_mode"),
            label = "Home/Away plot",
            choices = c(
              "Classic density" = "classic",
              "Cover/Total combos" = "combo"
            ),
            selected = "classic",
            width = "100%"
          ),
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
          ),
          shiny$checkboxInput(
            inputId = ns("show_spread_line"),
            label = "Show spread line",
            value = TRUE
          ),
          shiny$checkboxInput(
            inputId = ns("show_total_line"),
            label = "Show total line",
            value = TRUE
          ),
          shiny$checkboxInput(
            inputId = ns("show_prob_labels"),
            label = "Show probability labels",
            value = TRUE
          )
        ),
        bslib$layout_columns(
          col_widths = bslib$breakpoints(xs = c(12, 12), lg = c(4, 8)),
          gap = "0.5rem",
          bslib$card(
            class = "predictions-summary",
            bslib$card_header("Cover Probability Summary"),
            reactable$reactableOutput(ns("prob_summary"), height = "100%")
          ),
          bslib$navset_card_tab(
            id = ns("plots_tabset"),
            full_screen = TRUE,
            bslib$nav_panel(
              title = "Simulated (y): Result/Total",
              bslib$card_body(
                class = "predictions-plot",
                shiny$plotOutput(
                  outputId = ns("joint_y_prob_plot"),
                  height = "520px"
                )
              )
            ),
            bslib$nav_panel(
              title = "Expected (μ): Result/Total",
              bslib$card_body(
                class = "predictions-plot",
                shiny$plotOutput(
                  outputId = ns("joint_mu_prob_plot"),
                  height = "520px"
                )
              )
            ),
            bslib$nav_panel(
              title = "Simulated (y): Home/Away",
              bslib$card_body(
                class = "predictions-plot",
                shiny$plotOutput(
                  outputId = ns("joint_score_y_prob_plot"),
                  height = "520px"
                )
              )
            ),
            bslib$nav_panel(
              title = "Expected (μ): Home/Away",
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
  )
}

export(ui)
