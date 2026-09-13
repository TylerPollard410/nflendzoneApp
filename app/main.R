box::use(
  box[export],
  brand.yml[brand_use_logo],
  bsicons[bs_icon],
  bslib,
  shiny,
)

box::use(
  app / view / themes / themes,
  #app / view / themes / light[theme_light, brand_light_yml],
)

box::use(
  app / view / pages / standings[standings_server, standings_ui],
  app / view / pages / predictions[predictions_server, predictions_ui],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$page_navbar(
    title = shiny$a(
      class = "text-decoration-none",
      style = "color: inherit;",
      href = "#",
      # class = "navbar-logo",
      brand_use_logo(
        class = "d-inline-block align-text-bottom",
        themes$brand_light_yml,
        "small",
        height = 48
      ),
      #shiny$span(
      "NFL EndZone Analytics"
      #)
    ),
    id = ns("navbar"),
    fillable = TRUE,
    theme = themes$theme_brand_light,
    navbar_options = bslib$navbar_options(
      class = "bg-primary",
      position = "static-top",
      theme = "dark"
    ),
    fillable_mobile = TRUE,
    padding = 0,
    selected = "standings",
    header = shiny$tagList(
      shiny$tags$head(
        shiny$tags$link(
          rel = "icon",
          type = "image/png",
          href = "static/favicon-96x96.png",
          sizes = "96x96"
        ),
        shiny$tags$link(
          rel = "icon",
          type = "image/svg+xml",
          href = "static/favicon.svg"
        ),
        shiny$tags$link(rel = "shortcut icon", href = "static/favicon.ico"),
        shiny$tags$link(
          rel = "apple-touch-icon",
          sizes = "180x180",
          href = "static/apple-touch-icon.png"
        ),
        shiny$tags$meta(
          name = "apple-mobile-web-app-title",
          content = "NFLEndZoneAnalytics"
        ),
        shiny$tags$link(rel = "manifest", href = "static/site.webmanifest")
      ),
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
      icon = bs_icon("list-ol"),
      standings_ui(ns("standings"))
    ),
    bslib$nav_menu(
      title = "Predictions",
      value = "predictions",
      icon = shiny$icon("wand-magic-sparkles"),
      bslib$nav_panel(
        "Games",
        value = "games",
        icon = shiny$icon("football"),
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

#' @export
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
