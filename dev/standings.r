source("dependencies.R")
source("app/logic/data_import_functions.R")
source("app/logic/data_startup.R")

standings_season <- 2006
games <- game_data |>
  filter(season == standings_season, !is.na(result))
ranks = c("CONF", "DIV", "DRAFT", "NONE")
tiebreaker_depth = c("SOS", "PRE-SOV", "POINTS", "RANDOM")
playoff_seeds = NULL
verbosity = c("MIN", "MAX", "NONE")

standings1 <- nfl_standings(
  games,
  ranks = ranks,
  tiebreaker_depth = tiebreaker_depth,
  playoff_seeds = playoff_seeds,
  verbosity = verbosity[3]
)

standings_grp_by = c("div", "conf", "nfl")
standings_order_by = c("div_rank", "conf_rank", "draft_rank")
standings_reverse = FALSE

table1 <- standings1 |>
  filter(conf == "AFC") |>
  nfl_standings_prettify(
    grp_by = standings_grp_by[1],
    order_by = standings_order_by[1],
    reverse = standings_reverse
  )
table1
table1 |>
  opt_interactive(
    active = TRUE,
    use_pagination = TRUE,
    use_pagination_info = TRUE,
    use_sorting = TRUE,
    use_search = FALSE,
    use_filters = FALSE,
    use_resizers = FALSE,
    use_highlight = TRUE,
    use_compact_mode = TRUE,
    use_text_wrapping = FALSE,
    use_page_size_select = FALSE,
    page_size_default = 10,
    page_size_values = c(10, 25, 50, 100),
    pagination_type = c("numbers", "jump", "simple"),
    height = "auto",
    selection_mode = NULL
  )
