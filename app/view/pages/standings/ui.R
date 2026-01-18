box::use(
  box[export],
  bslib,
  shiny,
)

box::use(
  app / logic / data / startup[all_seasons],
)

ui <- function(id) {
  ns <- shiny$NS(id)
  bslib$card(
    bslib$card_header(
      shiny$div(
        style = "display:flex; align-items:center; justify-content:space-between; gap: 0rem;",
        shiny$h3("NFL Standings", style = "margin:0;"),
        shiny$selectInput(
          ns("season_select"),
          NULL,
          choices = rev(all_seasons),
          selected = max(all_seasons),
          width = "auto",
          selectize = FALSE
        )
      )
    ),
    bslib$card_body(
      padding = "0.5rem",
      bslib$navset_underline(
        id = ns("navset"),
        bslib$nav_panel(
          title = "Standings",
          value = "standings",
          shiny$br(),
          shiny$radioButtons(
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
          shiny$radioButtons(
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
          shiny$uiOutput(ns("standings_tables_ui"))
        ),
        bslib$nav_panel(
          title = "Playoffs",
          value = "playoffs",
          shiny$br(),
          shiny$uiOutput(ns("playoffs_tables_ui"))
        )
      )
    )
  )
}

export(ui)
