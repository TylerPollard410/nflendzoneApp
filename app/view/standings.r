box::use(
  bslib[
    navset_card_pill,
    navset_pill,
    navset_underline,
    nav_panel,
    card,
    card_header,
    card_body,
    layout_columns
  ],
  dplyr[filter, mutate, select, arrange, left_join, transmute, any_of],
  nflseedR[
    nfl_standings,
    compute_division_ranks,
    compute_conference_seeds
  ],
  reactable[
    reactable,
    reactableTheme,
    renderReactable,
    reactableOutput,
    colGroup,
    colDef,
    colFormat
  ],
  reactablefmtr[
    group_border_sort,
    group_merge_sort,
    color_scales,
    embed_img
  ],
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
    req,
    uiOutput,
    renderUI
  ],
)

box::use(
  app / logic / data_startup[all_seasons, game_data, teams_data],
)

# Standings module.

#' @export
ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      div(
        style = "display:flex; align-items:center; justify-content:space-between; gap: 0rem;",
        h3("NFL Standings", style = "margin:0;"),
        selectInput(
          ns("season_select"),
          NULL,
          choices = rev(all_seasons),
          selected = max(all_seasons),
          width = "auto",
          selectize = FALSE
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
          uiOutput(ns("standings_tables_ui"))
        ),
        nav_panel(
          title = "Playoffs",
          value = "playoffs",
          br(),
          uiOutput(ns("playoffs_tables_ui"))
        )
      )
    )
  )
}

#' @export
server <- function(id, dark_mode = NULL) {
  moduleServer(id, function(input, output, session) {
    is_dark_mode <- reactive({
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
      reactableTheme(
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

    standings_games <- reactive({
      req(input$season_select)
      game_data |>
        filter(
          season == input$season_select,
          game_type == "REG",
          !is.na(result)
        )
    })

    standings_data <- reactive({
      req(standings_games())
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

    standings_display <- reactive({
      req(standings_data())
      standings_data() |>
        left_join(
          teams_data |> select(team_abbr, team_name, team_logo_espn),
          by = c("team" = "team_abbr")
        ) |>
        mutate(
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

    standings_ranked <- reactive({
      req(standings_display())
      data <- standings_display()

      rank_col <- switch(
        input$standings_order_by,
        "div_rank" = "div_rank",
        "conf_rank" = "conf_rank",
        "draft_rank" = "draft_rank",
        "div_rank"
      )

      data |>
        mutate(Rank = data[[rank_col]]) |>
        select(
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

    rank_label <- reactive({
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

      data <- data |> select(any_of(select_cols))
      logo_cell <- embed_img(
        data = data,
        height = 24,
        width = 24,
        horizontal_align = "left",
        label = "Team",
        label_position = "right"
      )

      cols <- list(
        Conf = colDef(name = "Conf", minWidth = 60, sortable = FALSE),
        Division = colDef(
          name = "",
          minWidth = 95,
          sortable = FALSE,
          style = if (group_by_division) group_merge_sort("Division") else NULL
        ),
        team_logo_espn = colDef(
          name = "Team",
          sticky = "left",
          minWidth = 160,
          sortable = FALSE,
          cell = logo_cell,
          style = list(borderRight = "1px solid var(--bs-border-color)")
        ),
        Team = colDef(show = FALSE),
        Rank = colDef(name = rank_label, align = "center", minWidth = 55, sortable = FALSE),
        GP = colDef(
          name = "GP",
          align = "center",
          minWidth = 45,
          sortable = FALSE,
          style = list(borderRight = "1px dashed var(--bs-border-color)")
        ),
        W = colDef(name = "W", align = "center", minWidth = 35, sortable = FALSE),
        L = colDef(name = "L", align = "center", minWidth = 35, sortable = FALSE),
        T = colDef(
          name = "T",
          align = "center",
          minWidth = 35,
          sortable = FALSE,
          style = list(borderRight = "1px solid var(--bs-border-color)")
        ),
        `W-L%` = colDef(
          name = "W-L%",
          align = "center",
          minWidth = 60,
          sortable = FALSE,
          format = colFormat(percent = TRUE, digits = 1),
          style = list(borderRight = "1px solid var(--bs-border-color)")
        ),
        PF = colDef(name = "PF", align = "center", minWidth = 55, sortable = FALSE),
        PA = colDef(name = "PA", align = "center", minWidth = 55, sortable = FALSE),
        PD = colDef(
          name = "PD",
          align = "center",
          minWidth = 55,
          sortable = FALSE,
          style = color_scales(
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

      reactable(
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
          group_border_sort(
            columns = "Division",
            border_color = "var(--bs-border-color)",
            border_width = "1.5px",
            border_style = "solid"
          )
        } else {
          NULL
        },
        defaultColGroup = colGroup(headerStyle = list(border = "none")),
        columnGroups = list(
          colGroup(name = "Record", columns = c("GP", "W", "L", "T", "W-L%")),
          colGroup(name = "Points", columns = c("PF", "PA", "PD"))
        ),
        defaultColDef = colDef(vAlign = "center", headerStyle = list(borderTop = "none")),
        columns = cols[names(cols) %in% names(data)]
      )
    }

    output$standings_tables_ui <- renderUI({
      ns <- session$ns
      switch(
        input$standings_group_by,
        "nfl" = card(card_header("NFL"), card_body(reactableOutput(ns("nfl_table")))),
        layout_columns(
          col_widths = c(6, 6),
          gap = "0.75rem",
          card(
            card_header(if (identical(input$standings_group_by, "div")) "AFC (Divisions)" else "AFC"),
            card_body(reactableOutput(ns("afc_table")))
          ),
          card(
            card_header(if (identical(input$standings_group_by, "div")) "NFC (Divisions)" else "NFC"),
            card_body(reactableOutput(ns("nfc_table")))
          )
        )
      )
    })

    output$nfl_table <- renderReactable({
      req(standings_ranked())
      make_standings_table(
        standings_ranked() |> arrange(Rank),
        show_conf = TRUE,
        show_division = TRUE,
        group_by_division = FALSE,
        rank_label = rank_label()
      )
    })

    output$afc_table <- renderReactable({
      req(standings_ranked())
      by_div <- identical(input$standings_group_by, "div")
      data <- standings_ranked() |> filter(Conf == "AFC")
      data <- if (by_div) data |> arrange(Division, Rank) else data |> arrange(Rank)
      make_standings_table(
        data,
        show_conf = FALSE,
        show_division = by_div,
        group_by_division = by_div,
        rank_label = rank_label()
      )
    })

    output$nfc_table <- renderReactable({
      req(standings_ranked())
      by_div <- identical(input$standings_group_by, "div")
      data <- standings_ranked() |> filter(Conf == "NFC")
      data <- if (by_div) data |> arrange(Division, Rank) else data |> arrange(Rank)
      make_standings_table(
        data,
        show_conf = FALSE,
        show_division = by_div,
        group_by_division = by_div,
        rank_label = rank_label()
      )
    })

    playoffs_data <- reactive({
      req(standings_games())
      games <- standings_games() |>
        transmute(
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

    playoffs_display <- reactive({
      req(playoffs_data())
      playoffs_data() |>
        left_join(
          teams_data |> select(team_abbr, team_name, team_logo_espn),
          by = c("team" = "team_abbr")
        ) |>
        mutate(
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
        arrange(Conf, seed) |>
        select(
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
      logo_cell <- embed_img(
        data = data,
        height = 24,
        width = 24,
        horizontal_align = "left",
        label = "Team",
        label_position = "right"
      )

      reactable(
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
        defaultColDef = colDef(vAlign = "center"),
        defaultColGroup = colGroup(headerStyle = list(border = "none")),
        columnGroups = list(
          colGroup(name = "Record", columns = c("GP", "W", "L", "T", "W-L%")),
          colGroup(name = "Tiebreak", columns = c("DIV%", "CON%", "SOV", "SOS"))
        ),
        columns = list(
          Conf = colDef(show = FALSE),
          seed = colDef(name = "Seed", align = "center", minWidth = 55, sticky = "left"),
          team_logo_espn = colDef(
            name = "Team",
            sticky = "left",
            minWidth = 180,
            cell = logo_cell,
            style = list(borderRight = "1px solid var(--bs-border-color)")
          ),
          Team = colDef(show = FALSE),
          GP = colDef(name = "GP", align = "center", minWidth = 45),
          W = colDef(name = "W", align = "center", minWidth = 35),
          L = colDef(name = "L", align = "center", minWidth = 35),
          T = colDef(name = "T", align = "center", minWidth = 35),
          `W-L%` = colDef(format = colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 60),
          `DIV%` = colDef(format = colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 70),
          `CON%` = colDef(format = colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 70),
          SOV = colDef(format = colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 70),
          SOS = colDef(format = colFormat(percent = TRUE, digits = 1), align = "center", minWidth = 70)
        )
      )
    }

    output$playoffs_tables_ui <- renderUI({
      ns <- session$ns
      tagList(
        layout_columns(
          col_widths = c(6, 6),
          gap = "0.75rem",
          card(card_header("AFC Seeds"), card_body(reactableOutput(ns("playoffs_afc_table")))),
          card(card_header("NFC Seeds"), card_body(reactableOutput(ns("playoffs_nfc_table"))))
        )
      )
    })

    output$playoffs_afc_table <- renderReactable({
      req(playoffs_display())
      make_playoffs_table(playoffs_display() |> filter(Conf == "AFC"))
    })

    output$playoffs_nfc_table <- renderReactable({
      req(playoffs_display())
      make_playoffs_table(playoffs_display() |> filter(Conf == "NFC"))
    })
  })
}
