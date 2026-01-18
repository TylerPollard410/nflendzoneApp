box::use(
  bsicons[bs_icon],
  bslib[
    page_navbar,
    navbar_options,
    bs_theme,
    bs_add_rules,
    input_dark_mode,
    nav_item,
    nav_menu,
    nav_panel,
    nav_spacer,
    navset_bar,
    navset_card_pill,
    navset_pill
  ],
  sass[sass_file],
  reactable[reactable, renderReactable, reactableOutput],
  shiny[
    NS,
    bootstrapPage,
    div,
    moduleServer,
    p,
    br,
    renderUI,
    tags,
    uiOutput,
    reactive,
    useBusyIndicators
  ],
  thematic[thematic_shiny],
)

box::use(
  app /
    logic /
    data_startup[
      teams_data,
      teams,
      game_data,
      game_data_long,
      season_weeks_df,
      base_repo_url,
      season_standings_data,
      team_features_data,
      team_strength_negbinom_summary
    ],
  app / view / standings,
  app / view / predictions,
)

# Keep app theming centralized here for now.
app_theme <- bs_theme(version = 5) |>
  bs_add_rules(sass_file("app/styles/main.scss"))

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_navbar(
    title = "NFL EndZone Anaytics",
    id = ns("navbar"),
    fillable = TRUE,
    theme = app_theme,
    navbar_options = navbar_options(
      position = "static-top"
    ),
    fillable_mobile = FALSE,
    padding = 0,
    selected = "standings",
    header = list(
      useBusyIndicators()
    ),
    nav_panel(
      title = "Home",
      value = "home",
      reactableOutput(ns("test_df")),
      uiOutput(ns("message"))
    ),
    nav_panel(
      title = "Standings",
      value = "standings",
      icon = bs_icon("table"),
      standings$ui(ns("standings"))
    ),
    nav_menu(
      title = "Predictions",
      value = "predictions",
      icon = bs_icon("three-dots"),
      nav_panel(
        "Games",
        value = "games",
        icon = bs_icon("info-circle"),
        predictions$ui(ns("predictions_games"))
      )
    ),
    nav_spacer(),
    nav_item(
      input_dark_mode(
        id = ns("dark_mode"),
        mode = "dark"
      )
    )
  )
}

#thematic_shiny()

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI({
      div(
        style = "display: flex; justify-content: center; align-items: center; height: 100vh;",
        tags$h1(
          tags$a(
            "Check out Rhino docs!",
            href = "https://appsilon.github.io/rhino/"
          )
        )
      )
    })
    standings$server(
      "standings",
      dark_mode = reactive(input$dark_mode)
    )

    predictions$server(
      "predictions_games",
      dark_mode = reactive(input$dark_mode)
    )
  })
}
