box::use(
  bslib[
    navset_card_pill,
    navset_pill,
    navset_underline,
    nav_panel,
    card,
    card_header,
    card_body
  ],
  dplyr[filter],
  gt[render_gt, gt_output, opt_interactive],
  nflseedR[nfl_standings, nfl_standings_prettify],
  shiny[
    p,
    NS,
    moduleServer,
    h3,
    tagList,
    selectInput,
    div,
    br,
    radioButtons,
    reactive,
    req
  ],
)

box::use(
  app / logic / data_startup[all_seasons, game_data],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      div(
        style = "display:flex; align-items:center; justify-content:space-between; gap: 1rem;",
        h3("NFL Standings", style = "margin:0;"),
        selectInput(
          ns("season_select"),
          NULL,
          choices = rev(all_seasons),
          selected = max(all_seasons),
          width = "auto"
        )
      )
    ),
    card_body(
      padding = "0.5rem",
      navset_underline(
        id = ns("navset"),
        nav_panel(
          title = "Standings",
          value = "standings",
          br(),
          radioButtons(
            inputId = ns("standings_group_by"),
            label = "Group By:",
            choices = c(
              "Division" = "div",
              "Conference" = "conf",
              "League" = "nfl"
            ),
            selected = "div",
            inline = TRUE,
            width = "auto"
          ),
          radioButtons(
            inputId = ns("standings_order_by"),
            label = "Order By:",
            choices = c(
              "Division Rank" = "div_rank",
              "Conference Rank" = "conf_rank",
              "Draft Rank" = "draft_rank"
            ),
            selected = "div_rank",
            inline = TRUE,
            width = "auto"
          ),
          card(
            card_body(
              padding = "0.5rem",
              gt_output(ns("standings_table"))
            )
          )
        ),
        nav_panel(
          title = "Playoffs",
          value = "playoffs",
          p("More standings views coming soon!")
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Filter games down to selected season
    standings_data <- reactive({
      req(input$season_select)
      game_data |>
        filter(season == input$season_select, !is.na(result)) |>
        nfl_standings(
          ranks = "CONF",
          tiebreaker_depth = "SOS",
          playoff_seeds = NULL,
          verbosity = "NONE"
        )
    })

    # Render standings table based on selected grouping and ordering
    output$standings_table <- render_gt({
      req(standings_data())
      standings_data() |>
        nfl_standings_prettify(
          grp_by = input$standings_group_by,
          order_by = input$standings_order_by,
          reverse = FALSE
        ) |>
        opt_interactive(
          active = FALSE,
          use_pagination = FALSE,
          use_pagination_info = TRUE,
          use_sorting = TRUE,
          use_search = FALSE,
          use_filters = FALSE,
          use_resizers = FALSE,
          use_highlight = TRUE,
          use_compact_mode = TRUE,
          use_text_wrapping = FALSE,
          use_page_size_select = FALSE,
          page_size_default = 10,
          page_size_values = c(10, 25, 50, 100),
          pagination_type = "numbers",
          height = "auto",
          selection_mode = NULL
        )
    })
  })
}
