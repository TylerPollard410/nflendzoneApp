box::use(
  bsicons[bs_icon],
  bslib[
    bs_theme,
    input_dark_mode,
    nav_item,
    nav_menu,
    nav_panel,
    nav_spacer,
    navbar_options,
    navset_pill,
    page_navbar,
    navset_card_pill
  ],
  reactable[reactable, renderReactable, reactableOutput],
  shiny[NS, bootstrapPage, div, moduleServer, p, br, renderUI, tags, uiOutput],
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
  app / view / standings2,
  app / view / predictions,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title = "NFL EndZone Anaytics",
    id = ns("navbar"),
    theme = bs_theme(
      version = 5,
      primary = "purple",
      info = "#eec900"
    ),
    fillable_mobile = FALSE,
    selected = "standings",
    navbar_options = navbar_options(
      bg = "purple",
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
      #standings$mod_standings_ui(ns("standings"))
      standings2$ui(ns("standings2"))
    ),
    nav_menu(
      title = "Predictions",
      icon = bs_icon("three-dots"),
      nav_panel(
        "Games",
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

thematic_shiny()

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
    # standings$mod_standings_server(
    #   "standings",
    #   teams_data = teams_data,
    #   season_standings_data = season_standings_data
    # )

    standings2$server("standings2")

    predictions$server("predictions_games")
  })
}
