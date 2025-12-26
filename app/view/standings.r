box::use(
  bslib[card, card_title, card_body, card_header],
  shiny[
    uiOutput,
    NS,
    moduleServer,
    reactive,
    renderUI,
    tagList,
    div,
    img,
    strong,
    fluidRow,
    column,
    conditionalPanel,
    selectInput,
    radioButtons,
    tags,
    br,
    HTML,
    req
  ],
  dplyr[
    filter,
    select,
    any_of,
    arrange,
    desc,
    pull,
    left_join,
    join_by,
    mutate,
    relocate,
    row_number
  ],
  nflreadr[get_current_season],
  reactable[
    reactable,
    renderReactable,
    reactableOutput,
    colGroup,
    colDef,
    colFormat
  ],
  reactablefmtr[
    fivethirtyeight,
    group_border_sort,
    group_merge_sort,
    color_scales,
    embed_img
  ],
)


#' @title Standings Table Output (UI)
#' @description Output UI for a standings table module.
#' @param id Shiny module id.
#' @return A UI output for reactable standings table.
#' @export
standingsTableOutput <- function(id) {
  # withSpinner(
  uiOutput(NS(id, "standingsTableUI"))
  #   type = 8
  # )
}

#' @description Server logic for an individual standings table.
#' @param id Shiny module id.
#' @param standingsSeason Reactive, selected season.
#' @param standingsStat Reactive, selected statistic ("Total" or "Game") (may be NULL for playoffs).
#' @param teams_data Team reference data (for logo/conf logos).
#' @param standingsTableData Reactive data frame for this table.
#' @param conference "AFC", "NFC", or NULL (for NFL-wide table).
#' @param type One of "regular", "playoff", "nfl".
#' @export
standingsTableServer <- function(
  id,
  standingsSeason,
  standingsStat,
  teams_data,
  standingsTableData,
  conference = NULL,
  type = c("regular", "playoff", "nfl"),
  rankType
) {
  type <- match.arg(type)
  moduleServer(id, function(input, output, session) {
    # Reactive table data for this table
    tableDataReact <- reactive({
      data <- standingsTableData()
      rankReg <- rankType()
      if (!is.null(conference)) {
        data <- data |> filter(team_conf == conference)
      }
      if (type == "regular" || type == "nfl") {
        cols <- if (type == "regular") {
          c(
            "team_division",
            "team_logo_espn",
            "team",
            "div_rank",
            "conf_rank",
            "GP",
            "W",
            "L",
            "T",
            "W-L%",
            "PF",
            "PFG",
            "PA",
            "PAG",
            "PD",
            "MOV",
            "SOS",
            "SRS",
            "OSRS",
            "DSRS"
          )
          # if (rankReg == "div_rank") {
          #   stringr::str_subset(reg_cols, pattern = "conf_rank",  negate = TRUE)
          # } else stringr::str_subset(reg_cols, pattern = "conf_rank",  negate = TRUE)
        } else {
          c(
            "team_logo_espn",
            "team",
            "nfl_rank",
            "conf_rank",
            "GP",
            "W",
            "L",
            "T",
            "W-L%",
            "PF",
            "PFG",
            "PA",
            "PAG",
            "PD",
            "MOV",
            "SOS",
            "SRS",
            "OSRS",
            "DSRS"
          )
        }
        data <- data |> select(any_of(cols))
        if (type != "nfl" && rankReg == "div_rank") {
          data <- data |>
            arrange(team_division, div_rank) |>
            select(-div_rank, -conf_rank)
        } else if (type != "nfl" && rankReg == "conf_rank") {
          data <- data |>
            arrange(conf_rank) |>
            select(-team_division, -div_rank, -conf_rank)
        } else {
          if ("nfl_rank" %in% names(data)) {
            data <- data |> arrange(nfl_rank)
          }
        }

        # if ("div_rank" %in% names(data) && is.null(conference)) {
        #   data <- data |> arrange(team_division, div_rank)
        # }
        # if ("conf_rank" %in% names(data) && !is.null(conference)) {
        #   data <- data |> arrange(conf_rank)
        # }
        # if ("nfl_rank" %in% names(data)) data <- data |> arrange(nfl_rank)
        data
      } else {
        data |>
          select(
            seed,
            team_logo_espn,
            team_name,
            GP,
            W,
            L,
            T,
            `W-L%`,
            `CON%`,
            `DIV%`
          ) |>
          arrange(
            seed,
            desc(`W-L%`),
            desc(`CON%`),
            desc(`DIV%`)
          )
      }
    })

    output$standingsTable <- renderReactable({
      data <- tableDataReact()
      # REGULAR
      if (type == "regular") {
        stat <- if (!is.null(standingsStat())) standingsStat() else "Total"
        show_data <- if (stat == "Total") {
          data |> select(-c(PFG, PAG))
        } else {
          data |> select(-c(PF, PA))
        }
        reactable(
          show_data,
          theme = fivethirtyeight(
            centered = TRUE,
            header_font_size = "0.9em",
            font_size = "1.0em"
          ),
          highlight = TRUE,
          compact = TRUE,
          pagination = FALSE,
          wrap = FALSE,
          outlined = FALSE,
          bordered = FALSE,
          sortable = FALSE,
          showSortable = FALSE,
          fullWidth = TRUE,
          rowStyle = if ("team_division" %in% names(data)) {
            group_border_sort(
              columns = "team_division",
              border_color = "black",
              border_width = "1.5px",
              border_style = "solid"
            )
          } else {
            NULL
          },
          defaultColGroup = colGroup(headerStyle = list(border = "none")),
          columnGroups = if ("team_division" %in% names(data)) {
            list(
              colGroup(
                name = "",
                columns = c("team_division"),
                headerClass = "no-division-underline"
              ),
              colGroup(
                name = "Record",
                columns = c("GP", "W", "L", "T", "W-L%")
              ),
              colGroup(
                name = "Points",
                columns = if (stat == "Total") {
                  c("PF", "PA", "PD")
                } else {
                  c("PFG", "PAG", "PD")
                }
              ),
              colGroup(
                name = "Performance",
                columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS")
              )
            )
          } else {
            list(
              colGroup(
                name = "Record",
                columns = c("GP", "W", "L", "T", "W-L%")
              ),
              colGroup(
                name = "Points",
                columns = if (stat == "Total") {
                  c("PF", "PA", "PD")
                } else {
                  c("PFG", "PAG", "PD")
                }
              ),
              colGroup(
                name = "Performance",
                columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS")
              )
            )
          },
          defaultColDef = colDef(
            vAlign = "center",
            minWidth = 50,
            headerStyle = list(borderTop = "none", paddingTop = "3px")
          ),
          columns = list(
            team_division = colDef(
              name = "",
              minWidth = 90,
              style = group_merge_sort("team_division")
            ),
            team_logo_espn = colDef(
              name = "",
              maxWidth = 35,
              sticky = "left",
              cell = embed_img(width = "30px", height = "30px")
            ),
            team = colDef(
              name = "Team",
              maxWidth = 60,
              style = list(borderRight = "1px solid black")
            ),
            GP = colDef(
              name = "GP",
              minWidth = 40,
              align = "center",
              style = list(borderRight = "1px dashed #d3d3d3")
            ),
            W = colDef(name = "W", align = "center", minWidth = 30),
            L = colDef(name = "L", align = "center", minWidth = 30),
            T = colDef(name = "T", align = "center", minWidth = 30),
            `W-L%` = colDef(
              name = "W-L%",
              format = colFormat(percent = TRUE, digits = 1),
              align = "center",
              minWidth = 60,
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            PFG = colDef(format = colFormat(digits = 2)),
            PAG = colDef(format = colFormat(digits = 2)),
            PD = colDef(
              align = "center",
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            MOV = colDef(format = colFormat(digits = 2)),
            SOS = colDef(format = colFormat(digits = 2)),
            SRS = colDef(
              format = colFormat(digits = 2),
              style = color_scales(
                data = data,
                colors = c("red", "pink", "whitesmoke", "palegreen", "green"),
                bias = 1,
                brighten_text = FALSE
              )
            ),
            OSRS = colDef(format = colFormat(digits = 2)),
            DSRS = colDef(format = colFormat(digits = 2)),
            conf_rank = colDef(name = "Conf", minWidth = 40, align = "center"),
            nfl_rank = colDef(name = "NFL", minWidth = 40, align = "center")
          )
        )
        # NFL RANK TABLE
      } else if (type == "nfl") {
        reactable(
          data,
          theme = fivethirtyeight(
            centered = TRUE,
            header_font_size = "0.9em",
            font_size = "1.0em"
          ),
          highlight = TRUE,
          compact = TRUE,
          pagination = FALSE,
          wrap = FALSE,
          outlined = FALSE,
          bordered = FALSE,
          sortable = FALSE,
          showSortable = FALSE,
          fullWidth = TRUE,
          defaultColGroup = colGroup(headerStyle = list(border = "none")),
          columnGroups = list(
            colGroup(name = "Rank", columns = c("nfl_rank", "conf_rank")),
            colGroup(name = "Record", columns = c("GP", "W", "L", "T", "W-L%")),
            colGroup(
              name = "Points",
              columns = c("PF", "PFG", "PA", "PAG", "PD")
            ),
            colGroup(
              name = "Performance",
              columns = c("MOV", "SOS", "SRS", "OSRS", "DSRS")
            )
          ),
          defaultColDef = colDef(
            vAlign = "center",
            minWidth = 50,
            headerStyle = list(borderTop = "none", paddingTop = "3px")
          ),
          columns = list(
            team_logo_espn = colDef(
              name = "",
              maxWidth = 35,
              sticky = "left",
              cell = embed_img(width = "30px", height = "30px")
            ),
            team = colDef(
              name = "Team",
              maxWidth = 60,
              style = list(borderRight = "1px solid black")
            ),
            nfl_rank = colDef(name = "NFL", minWidth = 40, align = "center"),
            conf_rank = colDef(
              name = "Conf",
              minWidth = 40,
              align = "center",
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            GP = colDef(
              name = "GP",
              minWidth = 40,
              align = "center",
              style = list(borderRight = "1px dashed #d3d3d3")
            ),
            W = colDef(name = "W", align = "center", minWidth = 30),
            L = colDef(name = "L", align = "center", minWidth = 30),
            T = colDef(name = "T", align = "center", minWidth = 30),
            `W-L%` = colDef(
              name = "W-L%",
              format = colFormat(percent = TRUE, digits = 1),
              align = "center",
              minWidth = 60,
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            PFG = colDef(format = colFormat(digits = 2)),
            PAG = colDef(format = colFormat(digits = 2)),
            PD = colDef(
              align = "center",
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            MOV = colDef(format = colFormat(digits = 2)),
            SOS = colDef(format = colFormat(digits = 2)),
            SRS = colDef(
              format = colFormat(digits = 2),
              style = color_scales(
                data = data,
                colors = c("red", "pink", "whitesmoke", "palegreen", "green"),
                bias = 1,
                brighten_text = FALSE
              )
            ),
            OSRS = colDef(format = colFormat(digits = 2)),
            DSRS = colDef(format = colFormat(digits = 2))
          )
        )
        # PLAYOFFS
      } else {
        reactable(
          data,
          theme = fivethirtyeight(
            centered = TRUE,
            header_font_size = "0.9em",
            font_size = "1.0em"
          ),
          highlight = TRUE,
          compact = TRUE,
          pagination = FALSE,
          wrap = FALSE,
          outlined = FALSE,
          sortable = FALSE,
          showSortable = FALSE,
          fullWidth = TRUE,
          columns = list(
            seed = colDef(
              name = "Seed",
              align = "center",
              minWidth = 50,
              sticky = "left"
            ),
            team_logo_espn = colDef(
              name = "",
              minWidth = 30,
              sticky = "left",
              cell = embed_img(height = "25px")
            ),
            team_name = colDef(
              name = "Team",
              minWidth = 150,
              style = list(borderRight = "1px solid black")
            ),
            GP = colDef(
              name = "GP",
              align = "center",
              minWidth = 30,
              style = list(borderRight = "1px dashed #d3d3d3")
            ),
            W = colDef(name = "W", align = "center", minWidth = 30),
            L = colDef(name = "L", align = "center", minWidth = 30),
            T = colDef(
              name = "T",
              align = "center",
              minWidth = 30,
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            `W-L%` = colDef(
              name = "W-L%",
              format = colFormat(percent = TRUE, digits = 1),
              align = "center",
              minWidth = 50,
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            `CON%` = colDef(
              name = "CON%",
              format = colFormat(percent = TRUE, digits = 1),
              align = "center",
              minWidth = 50,
              style = list(borderRight = "1px solid #d3d3d3")
            ),
            `DIV%` = colDef(
              name = "DIV%",
              format = colFormat(percent = TRUE, digits = 1),
              align = "center",
              minWidth = 50
            )
          )
        )
      }
    })

    output$standingsTableUI <- renderUI({
      ns <- session$ns
      title_logo <- if (!is.null(conference)) {
        teams_data |>
          filter(team_conf == conference) |>
          pull(team_conference_logo) |>
          unique()
      } else {
        teams_data |> pull(team_league_logo) |> unique()
      }

      table_title <- switch(
        type,
        "regular" = if (!is.null(conference)) {
          paste(conference, "Standings")
        } else {
          "NFL Standings"
        },
        "playoff" = if (!is.null(conference)) {
          paste(conference, "Playoff Standings")
        } else {
          "NFL Playoffs"
        },
        "nfl" = "NFL Standings"
      )
      tagList(
        tags$style(HTML(
          ".no-division-underline.rt-th-group:after {display: none !important;} .card-body { padding: 0px }"
        )),
        card(
          card_header = div(
            style = "display: flex; align-items: center;",
            if (!is.null(title_logo)) {
              img(src = title_logo, style = "height: 25px;")
            },
            strong(table_title, style = "margin-left: 6px; font-size: 25px;"),
            strong(
              standingsSeason(),
              style = "margin-left: 6px; font-size: 20px;"
            )
          ),
          #width = 12,
          #status = "primary",
          # withSpinner(
          card_body(
            reactableOutput(ns("standingsTable"))
          )
          #   type = 8
          # )
        )
      )
    })
  })
}

#' @title Standings Tab UI
#' @description Full tab for interactive NFL standings, with input widgets and all standings tables.
#' @param id Shiny module id.
#' @return Shiny UI element for the standings tab.
#' @export
mod_standings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      div(
        style = "margin-right: 1rem",
        selectInput(
          inputId = ns("season"),
          label = "Select season",
          choices = seq(2006, get_current_season()),
          selected = get_current_season()
        )
      ),
      div(
        style = "margin-right: 1rem",
        radioButtons(
          inputId = ns("stat"),
          label = "Table Statistic",
          choices = c("Total", "Game"),
          inline = TRUE
          #status = "info"
        )
      ),
      div(
        style = "margin-right: 1rem",
        radioButtons(
          inputId = ns("rank_type"),
          label = "Rank Type",
          choices = c(
            "Division" = "div_rank",
            "Conference" = "conf_rank",
            "NFL" = "nfl_rank"
          ),
          selected = "div_rank",
          inline = TRUE
          #status = "info"
        )
      )
      # div(style = "margin-right: 1rem",
      #     shinyWidgets::virtualSelectInput(
      #       inputId = ns("season"),
      #       label = "Select season",
      #       choices = as.character(seq(2007, get_current_season())),
      #       selected = as.character(get_current_season()),
      #       search = FALSE
      #     )
      # ),
      # div(style = "margin-right: 1rem",
      #     shinyWidgets::radioGroupButtons(
      #       inputId = ns("stat"),
      #       label = "Table Statistic",
      #       choices = c("Total", "Game"),
      #       status = "info",
      #       checkIcon = list(
      #         yes = icon("check"),
      #         no = icon("")
      #       )
      #     )
      # ),
      # div(style = "margin-right: 1rem",
      #     shinyWidgets::radioGroupButtons(
      #       inputId = ns("rank_type"),
      #       label = "Rank Type",
      #       choices = c("Division" = "div_rank", "Conference" = "conf_rank", "NFL" = "nfl_rank"),
      #       selected = "div_rank",
      #       status = "info",
      #       checkIcon = list(
      #         yes = icon("check"),
      #         no = icon("")
      #       )
      #     )
      # )
    ),
    br(),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'nfl_rank'", ns("rank_type")),
      standingsTableOutput(ns("nflTable"))
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] != 'nfl_rank'", ns("rank_type")),
      fluidRow(
        style = "margin-left: -7.5px; margin-right: -7.5px",
        column(6, standingsTableOutput(ns("afcReg")), style = "padding: 0px"),
        column(6, standingsTableOutput(ns("nfcReg")), style = "padding: 0px")
      )
    ),
    fluidRow(
      column(6, standingsTableOutput(ns("afcPlayoff"))),
      column(6, standingsTableOutput(ns("nfcPlayoff")))
    )
  )
}

#' @description Server logic for the NFL standings tab, coordinating all tables and inputs.
#' @param id Shiny module id.
#' @param teams_data Data frame with team information (logos, names, etc).
#' @export
mod_standings_server <- function(id, teams_data, season_standings_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    standingsSeason <- reactive(as.numeric(input$season))
    standingsStat <- reactive(input$stat)
    rankType <- reactive(input$rank_type)

    selected_season_data <- reactive({
      req(standingsSeason())
      # standings_df <- load_season_standings_data(seasons = standingsSeason())
      # standings_df |>
      season_standings_data |>
        filter(season == standingsSeason()) |>
        # Join for logo and team name (assumes 'team' is the key in both)
        left_join(
          teams_data |> select(team_abbr, team_logo_espn, team_name),
          by = join_by(team == team_abbr)
        ) |>
        mutate(
          team_conf = conf,
          team_division = division,
          GP = games,
          W = wins,
          L = losses,
          T = ties,
          `W-L%` = win_pct,
          `CON%` = conf_pct,
          `DIV%` = div_pct,
          PF = pf,
          PA = pa,
          PD = pd,
          MOV = MOV,
          SOS = SOS,
          SRS = SRS,
          OSRS = OSRS,
          DSRS = DSRS,
          PFG = round(pf / games, 2),
          PAG = round(pa / games, 2),
          seed = conf_rank # For playoff tables
        ) |>
        relocate(team_logo_espn, .after = team_division)
    })

    nfl_data <- reactive({
      data <- selected_season_data()
      data <- data |>
        arrange(desc(win_pct), desc(SRS)) |>
        mutate(nfl_rank = row_number())
      data |> arrange(team)
    })

    # Register all child tables
    standingsTableServer(
      id = "afcReg",
      standingsSeason = standingsSeason,
      standingsStat = standingsStat,
      teams_data = teams_data,
      standingsTableData = selected_season_data,
      conference = "AFC",
      type = "regular",
      rankType = rankType
    )
    standingsTableServer(
      id = "nfcReg",
      standingsSeason = standingsSeason,
      standingsStat = standingsStat,
      teams_data = teams_data,
      standingsTableData = selected_season_data,
      conference = "NFC",
      type = "regular",
      rankType = rankType
    )
    standingsTableServer(
      id = "afcPlayoff",
      standingsSeason = standingsSeason,
      standingsStat = NULL,
      teams_data = teams_data,
      standingsTableData = selected_season_data,
      conference = "AFC",
      type = "playoff",
      rankType = rankType
    )
    standingsTableServer(
      id = "nfcPlayoff",
      standingsSeason = standingsSeason,
      standingsStat = NULL,
      teams_data = teams_data,
      standingsTableData = selected_season_data,
      conference = "NFC",
      type = "playoff",
      rankType = rankType
    )
    standingsTableServer(
      id = "nflTable",
      standingsSeason = standingsSeason,
      standingsStat = standingsStat,
      teams_data = teams_data,
      standingsTableData = nfl_data,
      conference = NULL,
      type = "nfl",
      rankType = rankType
    )
  })
}
