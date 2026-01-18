box::use(
  bsicons[bs_icon],
  bslib,
  sass[sass_file],
  reactable[reactable, renderReactable, reactableOutput],
  shiny,
)

box::use(
  app / logic / data_startup,
  app / view / standings,
  app / view / predictions,
)

# Keep app theming centralized here for now.
app_theme <- bslib$bs_theme(version = 5) |>
  bslib$bs_add_rules(sass_file("app/styles/main.scss"))

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$page_navbar(
    title = "NFL EndZone Anaytics",
    id = ns("navbar"),
    fillable = TRUE,
    theme = app_theme,
    navbar_options = bslib$navbar_options(
      position = "static-top"
    ),
    fillable_mobile = FALSE,
    padding = 0,
    selected = "standings",
    header = list(
      shiny$useBusyIndicators()
    ),
    bslib$nav_panel(
      title = "Home",
      value = "home",
      shiny$h3("Welcome to NFL EndZone Analytics")
    ),
    bslib$nav_panel(
      title = "Standings",
      value = "standings",
      icon = bs_icon("table"),
      standings$ui(ns("standings"))
    ),
    bslib$nav_menu(
      title = "Predictions",
      value = "predictions",
      icon = bs_icon("three-dots"),
      bslib$nav_panel(
        "Games",
        value = "games",
        icon = bs_icon("info-circle"),
        predictions$ui(ns("predictions_games"))
      )
    ),
    bslib$nav_spacer(),
    bslib$nav_item(
      bslib$input_dark_mode(
        id = ns("dark_mode"),
        mode = "dark"
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    standings$server(
      "standings",
      dark_mode = shiny$reactive(input$dark_mode)
    )

    predictions$server(
      "predictions_games",
      dark_mode = shiny$reactive(input$dark_mode)
    )
  })
}
