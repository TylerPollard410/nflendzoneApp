box::use(
  bslib,
  dplyr,
  nflseedR[nfl_standings, compute_division_ranks, compute_conference_seeds],
  reactable,
  reactablefmtr,
  shiny,
)

box::use(
  app / logic / data_startup[all_seasons, game_data, teams_data],
)

# Standings module.

#' @export
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

#' @export
server <- function(id, dark_mode = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    is_dark_mode <- shiny$reactive({
      if (is.null(dark_mode)) {
        return(FALSE)
      }
      value <- dark_mode()
      if (is.null(value) || length(value) == 0L) {
        return(FALSE)
      }
      if (is.logical(value)) {
        return(isTRUE(value[[1]]))
      }
      if (is.character(value)) {
        return(tolower(value[[1]]) %in% c("dark", "1", "true", "yes", "on"))
      }
      FALSE
    })

    bs_reactable_theme <- function() {
      reactable$reactableTheme(
        color = "var(--bs-emphasis-color, var(--bs-body-color))",
        backgroundColor = "var(--bs-body-bg, #ffffff)",
        borderColor = "var(--bs-border-color, #dee2e6)",
        stripedColor = "var(--bs-tertiary-bg, rgba(0, 0, 0, 0.03))",
        highlightColor = "var(--bs-secondary-bg, rgba(0, 0, 0, 0.05))",
        headerStyle = list(
          fontWeight = 600,
          color = "var(--bs-emphasis-color, var(--bs-body-color))",
          backgroundColor = "var(--bs-tertiary-bg, rgba(0, 0, 0, 0.03))"
        ),
        cellStyle = list(
          color = "var(--bs-emphasis-color, var(--bs-body-color))"
        )
      )
    }

    standings_games <- shiny$reactive({
      shiny$req(input$season_select)
      game_data |>
        dplyr$filter(
          season == input$season_select,
          game_type == "REG",
          !is.na(result)
        )
    })

    standings_data <- shiny$reactive({
      shiny$req(standings_games())
      ranks <- if (identical(input$standings_order_by, "draft_rank")) {
        "DRAFT"
      } else {
        "CONF"
      }

      standings_games() |>
        nfl_standings(
          ranks = ranks,
          tiebreaker_depth = "SOS",
          playoff_seeds = NULL,
          verbosity = "NONE"
        )
    })

    standings_display <- shiny$reactive({
      shiny$req(standings_data())
      standings_data() |>
        dplyr$left_join(
          teams_data |> dplyr$select(team_abbr, team_name, team_logo_espn),
          by = c("team" = "team_abbr")
        ) |>
        dplyr$mutate(
          GP = games,
          W = wins,
          L = losses,
          T = ties,
          `W-L%` = win_pct,
          PF = pf,
          PA = pa,
          PD = pd,
          Division = division,
          Conf = conf,
          Team = team_name
        )
    })

    standings_ranked <- shiny$reactive({
      shiny$req(standings_display())
      data <- standings_display()

      rank_col <- switch(
        input$standings_order_by,
        "div_rank" = "div_rank",
        "conf_rank" = "conf_rank",
        "draft_rank" = "draft_rank",
        "div_rank"
      )

      data |>
        dplyr$mutate(Rank = data[[rank_col]]) |>
        dplyr$select(
          Conf,
          Division,
          team_logo_espn,
          Team,
          Rank,
          GP,
          W,
          L,
          T,
          `W-L%`,
          PF,
          PA,
          PD
        )
    })

    rank_label <- shiny$reactive({
      switch(
        input$standings_order_by,
        "div_rank" = "Div",
        "conf_rank" = "Conf",
        "draft_rank" = "Draft",
        "Rank"
      )
    })

    make_standings_table <- function(
      data,
      show_conf = FALSE,
      show_division = FALSE,
      group_by_division = FALSE,
      rank_label = "Rank"
    ) {
      select_cols <- c(
        if (show_conf) "Conf",
        if (show_division) "Division",
        "team_logo_espn",
        "Team",
        "Rank",
        "GP",
        "W",
        "L",
        "T",
        "W-L%",
        "PF",
        "PA",
        "PD"
      )

      data <- data |> dplyr$select(dplyr$any_of(select_cols))
      logo_cell <- reactablefmtr$embed_img(
        data = data,
        height = 24,
        width = 24,
        horizontal_align = "left",
        label = "Team",
        label_position = "right"
      )

      cols <- list(
        Conf = reactable$colDef(name = "Conf", minWidth = 60, sortable = FALSE),
        Division = reactable$colDef(
          name = "",
          minWidth = 95,
          sortable = FALSE,
          style = if (group_by_division) reactablefmtr$group_merge_sort("Division") else NULL
        ),
        team_logo_espn = reactable$colDef(
          name = "Team",
          sticky = "left",
          minWidth = 160,
          sortable = FALSE,
          cell = logo_cell,
          style = list(borderRight = "1px solid var(--bs-border-color)")
        ),
        Team = reactable$colDef(show = FALSE),
        Rank = reactable$colDef(name = rank_label, align = "center", minWidth = 55, sortable = FALSE),
        GP = reactable$colDef(
          name = "GP",
          align = "center",
          minWidth = 45,
          sortable = FALSE,
          style = list(borderRight = "1px dashed var(--bs-border-color)")
        ),
        W = reactable$colDef(name = "W", align = "center", minWidth = 35, sortable = FALSE),
        L = reactable$colDef(name = "L", align = "center", minWidth = 35, sortable = FALSE),
        T = reactable$colDef(
          name = "T",
          align = "center",
          minWidth = 35,
          sortable = FALSE,
          style = list(borderRight = "1px solid var(--bs-border-color)")
        ),
        `W-L%` = reactable$colDef(
          name = "W-L%",
          align = "center",
          minWidth = 60,
          sortable = FALSE,
          format = reactable$colFormat(percent = TRUE, digits = 1),
          style = list(borderRight = "1px solid var(--bs-border-color)")
        ),
        PF = reactable$colDef(name = "PF", align = "center", minWidth = 55, sortable = FALSE),
        PA = reactable$colDef(name = "PA", align = "center", minWidth = 55, sortable = FALSE),
        PD = reactable$colDef(
          name = "PD",
          align = "center",
          minWidth = 55,
          sortable = FALSE,
          style = reactablefmtr$color_scales(
            data = data,
            colors = c(
              "#dc3545",
              if (isTRUE(is_dark_mode())) "#343a40" else "whitesmoke",
              "#198754"
            ),
            bias = 1,
            brighten_text = FALSE,
            show_text = TRUE
          )
        )
      )

      reactable$reactable(
        data,
        theme = bs_reactable_theme(),
        compact = TRUE,
        highlight = TRUE,
        pagination = FALSE,
        sortable = FALSE,
        showSortable = FALSE,
        fullWidth = TRUE,
        wrap = FALSE,
        outlined = FALSE,
        bordered = FALSE,
        rowStyle = if (group_by_division && "Division" %in% names(data)) {
          reactablefmtr$group_border_sort(
            columns = "Division",
            border_color = "var(--bs-border-color)",
            border_width = "1.5px",
            border_style = "solid"
          )
        } else {
          NULL
        },
        defaultColGroup = reactable$colGroup(headerStyle = list(border = "none")),
        columnGroups = list(
          reactable$colGroup(name = "Record", columns = c("GP", "W", "L", "T", "W-L%")),
          reactable$colGroup(name = "Points", columns = c("PF", "PA", "PD"))
        ),
        defaultColDef = reactable$colDef(vAlign = "center", headerStyle = list(borderTop = "none")),
        columns = cols[names(cols) %in% names(data)]
      )
    }

    output$standings_tables_ui <- shiny$renderUI({
      ns <- session$ns
      switch(
        input$standings_group_by,
        "nfl" = bslib$card(bslib$card_header("NFL"), bslib$card_body(reactable$reactableOutput(ns("nfl_table")))),
        bslib$layout_columns(
          col_widths = c(6, 6),
          gap = "0.75rem",
          bslib$card(
            bslib$card_header(if (identical(input$standings_group_by, "div")) "AFC (Divisions)" else "AFC"),
            bslib$card_body(reactable$reactableOutput(ns("afc_table")))
          ),
          bslib$card(
            bslib$card_header(if (identical(input$standings_group_by, "div")) "NFC (Divisions)" else "NFC"),
            bslib$card_body(reactable$reactableOutput(ns("nfc_table")))
          )
        )
      )
    })

    output$nfl_table <- reactable$renderReactable({
      shiny$req(standings_ranked())
      make_standings_table(
        standings_ranked() |> dplyr$arrange(Rank),
        show_conf = TRUE,
        show_division = TRUE,
        group_by_division = FALSE,
        rank_label = rank_label()
      )
    })

    output$afc_table <- reactable$renderReactable({
      shiny$req(standings_ranked())
      by_div <- identical(input$standings_group_by, "div")
      data <- standings_ranked() |> dplyr$filter(Conf == "AFC")
      data <- if (by_div) data |> dplyr$arrange(Division, Rank) else data |> dplyr$arrange(Rank)
      make_standings_table(
        data,
        show_conf = FALSE,
        show_division = by_div,
        group_by_division = by_div,
        rank_label = rank_label()
      )
    })

    output$nfc_table <- reactable$renderReactable({
      shiny$req(standings_ranked())
      by_div <- identical(input$standings_group_by, "div")
      data <- standings_ranked() |> dplyr$filter(Conf == "NFC")
      data <- if (by_div) data |> dplyr$arrange(Division, Rank) else data |> dplyr$arrange(Rank)
      make_standings_table(
        data,
        show_conf = FALSE,
        show_division = by_div,
        group_by_division = by_div,
        rank_label = rank_label()
      )
    })

    playoffs_data <- shiny$reactive({
      shiny$req(standings_games())
      games <- standings_games() |>
        dplyr$transmute(
          sim = season,
          game_type,
          week,
          away_team,
          home_team,
          result
        )

      suppressMessages({
        div_ranks <- compute_division_ranks(games, tiebreaker_depth = 3, .debug = FALSE)
        compute_conference_seeds(
          teams = div_ranks$standings,
          h2h = div_ranks$h2h,
          tiebreaker_depth = 3,
          .debug = FALSE,
          playoff_seeds = 7
        )$standings
      })
    })

    playoffs_display <- shiny$reactive({
      shiny$req(playoffs_data())
      playoffs_data() |>
        dplyr$left_join(
          teams_data |> dplyr$select(team_abbr, team_name, team_logo_espn),
          by = c("team" = "team_abbr")
        ) |>
        dplyr$mutate(
          GP = games,
          W = wins,
          L = losses,
          T = ties,
          `W-L%` = win_pct,
          `DIV%` = div_pct,
          `CON%` = conf_pct,
          SOV = sov,
          SOS = sos,
          Conf = conf,
          Team = team_name
        ) |>
        dplyr$arrange(Conf, seed) |>
        dplyr$select(
          Conf,
          seed,
          team_logo_espn,
          Team,
          GP,
          W,
          L,
          T,
          `W-L%`,
          `DIV%`,
          `CON%`,
          SOV,
          SOS
        )
    })

    make_playoffs_table <- function(data) {
      logo_cell <- reactablefmtr$embed_img(
        data = data,
        height = 24,
        width = 24,
        horizontal_align = "left",
        label = "Team",
        label_position = "right"
      )

      reactable$reactable(
        data,
        theme = bs_reactable_theme(),
        compact = TRUE,
        highlight = TRUE,
        pagination = FALSE,
        sortable = FALSE,
        showSortable = FALSE,
        fullWidth = TRUE,
        wrap = FALSE,
        outlined = FALSE,
        bordered = FALSE,
        defaultColDef = reactable$colDef(vAlign = "center"),
        defaultColGroup = reactable$colGroup(headerStyle = list(border = "none")),
        columnGroups = list(
          reactable$colGroup(name = "Record", columns = c("GP", "W", "L", "T", "W-L%")),
          reactable$colGroup(name = "Tiebreak", columns = c("DIV%", "CON%", "SOV", "SOS"))
        ),
        columns = list(
          Conf = reactable$colDef(show = FALSE),
          seed = reactable$colDef(name = "Seed", align = "center", minWidth = 55, sticky = "left"),
          team_logo_espn = reactable$colDef(
            name = "Team",
            sticky = "left",
            minWidth = 180,
            cell = logo_cell,
            style = list(borderRight = "1px solid var(--bs-border-color)")
          ),
          Team = reactable$colDef(show = FALSE),
          GP = reactable$colDef(name = "GP", align = "center", minWidth = 45),
          W = reactable$colDef(name = "W", align = "center", minWidth = 35),
          L = reactable$colDef(name = "L", align = "center", minWidth = 35),
          T = reactable$colDef(name = "T", align = "center", minWidth = 35),
          `W-L%` = reactable$colDef(format = reactable$colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 60),
          `DIV%` = reactable$colDef(format = reactable$colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 70),
          `CON%` = reactable$colDef(format = reactable$colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 70),
          SOV = reactable$colDef(format = reactable$colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 70),
          SOS = reactable$colDef(format = reactable$colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 70)
        )
      )
    }

    output$playoffs_tables_ui <- shiny$renderUI({
      ns <- session$ns
      shiny$tagList(
        bslib$layout_columns(
          col_widths = c(6, 6),
          gap = "0.75rem",
          bslib$card(bslib$card_header("AFC Seeds"), bslib$card_body(reactable$reactableOutput(ns("playoffs_afc_table")))),
          bslib$card(bslib$card_header("NFC Seeds"), bslib$card_body(reactable$reactableOutput(ns("playoffs_nfc_table"))))
        )
      )
    })

    output$playoffs_afc_table <- reactable$renderReactable({
      shiny$req(playoffs_display())
      make_playoffs_table(playoffs_display() |> dplyr$filter(Conf == "AFC"))
    })

    output$playoffs_nfc_table <- reactable$renderReactable({
      shiny$req(playoffs_display())
      make_playoffs_table(playoffs_display() |> dplyr$filter(Conf == "NFC"))
    })
  })
}
