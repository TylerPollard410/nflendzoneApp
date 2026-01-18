box::use(
  box[export],
  dplyr,
  reactable,
  reactablefmtr,
)

box::use(
  app / view / shared / reactable_theme[bs_reactable_theme],
)

make_standings_table <- function(
  data,
  show_conf = FALSE,
  show_division = FALSE,
  group_by_division = FALSE,
  rank_label = "Rank",
  is_dark_mode = FALSE
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
      style = if (group_by_division) {
        reactablefmtr$group_merge_sort("Division")
      } else {
        NULL
      }
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
    Rank = reactable$colDef(
      name = rank_label,
      align = "center",
      minWidth = 55,
      sortable = FALSE
    ),
    GP = reactable$colDef(
      name = "GP",
      align = "center",
      minWidth = 45,
      sortable = FALSE,
      style = list(borderRight = "1px dashed var(--bs-border-color)")
    ),
    W = reactable$colDef(
      name = "W",
      align = "center",
      minWidth = 35,
      sortable = FALSE
    ),
    L = reactable$colDef(
      name = "L",
      align = "center",
      minWidth = 35,
      sortable = FALSE
    ),
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
    PF = reactable$colDef(
      name = "PF",
      align = "center",
      minWidth = 55,
      sortable = FALSE
    ),
    PA = reactable$colDef(
      name = "PA",
      align = "center",
      minWidth = 55,
      sortable = FALSE
    ),
    PD = reactable$colDef(
      name = "PD",
      align = "center",
      minWidth = 55,
      sortable = FALSE,
      style = reactablefmtr$color_scales(
        data = data,
        colors = c(
          "#dc3545",
          if (isTRUE(is_dark_mode)) "#343a40" else "whitesmoke",
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
    defaultColGroup = reactable$colGroup(
      headerStyle = list(border = "none")
    ),
    columnGroups = list(
      reactable$colGroup(
        name = "Record",
        columns = c("GP", "W", "L", "T", "W-L%")
      ),
      reactable$colGroup(name = "Points", columns = c("PF", "PA", "PD"))
    ),
    defaultColDef = reactable$colDef(
      vAlign = "center",
      headerStyle = list(borderTop = "none")
    ),
    columns = cols[names(cols) %in% names(data)]
  )
}

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
    defaultColGroup = reactable$colGroup(
      headerStyle = list(border = "none")
    ),
    columnGroups = list(
      reactable$colGroup(
        name = "Record",
        columns = c("GP", "W", "L", "T", "W-L%")
      ),
      reactable$colGroup(
        name = "Tiebreak",
        columns = c("DIV%", "CON%", "SOV", "SOS")
      )
    ),
    columns = list(
      Conf = reactable$colDef(show = FALSE),
      seed = reactable$colDef(
        name = "Seed",
        align = "center",
        minWidth = 55,
        sticky = "left"
      ),
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
      `W-L%` = reactable$colDef(
        format = reactable$colFormat(percent = TRUE, digits = 1),
        align = "center",
        minWidth = 60
      ),
      `DIV%` = reactable$colDef(
        format = reactable$colFormat(percent = TRUE, digits = 1),
        align = "center",
        minWidth = 70
      ),
      `CON%` = reactable$colDef(
        format = reactable$colFormat(percent = TRUE, digits = 1),
        align = "center",
        minWidth = 70
      ),
      SOV = reactable$colDef(
        format = reactable$colFormat(percent = TRUE, digits = 1),
        align = "center",
        minWidth = 70
      ),
      SOS = reactable$colDef(
        format = reactable$colFormat(percent = TRUE, digits = 1),
        align = "center",
        minWidth = 70
      )
    )
  )
}

export(make_standings_table, make_playoffs_table)
