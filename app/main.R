box::use(
  box[export],
  bsicons[bs_icon],
  bslib,
  sass[sass_file],
  shiny,
)

box::use(
  app / view / pages / predictions[predictions_server, predictions_ui],
)

box::use(
  app / view / pages / standings[standings_server, standings_ui],
)

# Keep app theming centralized here for now.
app_theme <- bslib$bs_theme(version = 5) |>
  bslib$bs_add_rules(sass_file("app/styles/main.scss"))

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
      standings_ui(ns("standings"))
    ),
    bslib$nav_menu(
      title = "Predictions",
      value = "predictions",
      icon = bs_icon("three-dots"),
      bslib$nav_panel(
        "Games",
        value = "games",
        icon = bs_icon("info-circle"),
        predictions_ui(ns("predictions_games"))
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

server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    standings_server(
      "standings",
      dark_mode = shiny$reactive(input$dark_mode)
    )

    predictions_server(
      "predictions_games",
      dark_mode = shiny$reactive(input$dark_mode)
    )
  })
}

export(ui, server)
