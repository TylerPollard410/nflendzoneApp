box::use(
  box[export],
  bslib,
  dplyr,
  nflseedR[compute_conference_seeds, compute_division_ranks, nfl_standings],
  reactable,
  shiny,
)

box::use(
  app / logic / data / startup[game_data, teams_data],
  app / view / pages / standings / tables[make_playoffs_table, make_standings_table],
  app / view / shared / dark_mode[is_dark_mode_reactive],
)

server <- function(id, dark_mode = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    is_dark_mode <- is_dark_mode_reactive(dark_mode)

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
          `T`,
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

    output$standings_tables_ui <- shiny$renderUI({
      ns <- session$ns
      switch(
        input$standings_group_by,
        "nfl" = bslib$card(
          bslib$card_header("NFL"),
          bslib$card_body(reactable$reactableOutput(ns("nfl_table")))
        ),
        bslib$layout_columns(
          col_widths = c(6, 6),
          gap = "0.75rem",
          bslib$card(
            bslib$card_header(
              if (identical(input$standings_group_by, "div")) {
                "AFC (Divisions)"
              } else {
                "AFC"
              }
            ),
            bslib$card_body(reactable$reactableOutput(ns("afc_table")))
          ),
          bslib$card(
            bslib$card_header(
              if (identical(input$standings_group_by, "div")) {
                "NFC (Divisions)"
              } else {
                "NFC"
              }
            ),
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
        rank_label = rank_label(),
        is_dark_mode = is_dark_mode()
      )
    })

    output$afc_table <- reactable$renderReactable({
      shiny$req(standings_ranked())
      by_div <- identical(input$standings_group_by, "div")
      data <- standings_ranked() |> dplyr$filter(Conf == "AFC")
      data <- if (by_div) {
        data |> dplyr$arrange(Division, Rank)
      } else {
        data |> dplyr$arrange(Rank)
      }
      make_standings_table(
        data,
        show_conf = FALSE,
        show_division = by_div,
        group_by_division = by_div,
        rank_label = rank_label(),
        is_dark_mode = is_dark_mode()
      )
    })

    output$nfc_table <- reactable$renderReactable({
      shiny$req(standings_ranked())
      by_div <- identical(input$standings_group_by, "div")
      data <- standings_ranked() |> dplyr$filter(Conf == "NFC")
      data <- if (by_div) {
        data |> dplyr$arrange(Division, Rank)
      } else {
        data |> dplyr$arrange(Rank)
      }
      make_standings_table(
        data,
        show_conf = FALSE,
        show_division = by_div,
        group_by_division = by_div,
        rank_label = rank_label(),
        is_dark_mode = is_dark_mode()
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
        div_ranks <- compute_division_ranks(
          games,
          tiebreaker_depth = 3,
          .debug = FALSE
        )
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
          `T`,
          `W-L%`,
          `DIV%`,
          `CON%`,
          SOV,
          SOS
        )
    })

    output$playoffs_tables_ui <- shiny$renderUI({
      ns <- session$ns
      shiny$tagList(
        bslib$layout_columns(
          col_widths = c(6, 6),
          gap = "0.75rem",
          bslib$card(
            bslib$card_header("AFC Seeds"),
            bslib$card_body(reactable$reactableOutput(ns("playoffs_afc_table")))
          ),
          bslib$card(
            bslib$card_header("NFC Seeds"),
            bslib$card_body(reactable$reactableOutput(ns("playoffs_nfc_table")))
          )
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

export(server)
