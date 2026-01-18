box::use(
  box[export],
  cli[cli_warn],
  colorspace[lighten],
  dplyr,
  stats[setNames],
)

make_team_palettes <- function(teams_data, light_amount = 0.25) {
  team_colors <- setNames(teams_data$team_color, teams_data$team_abbr)
  team_colors_light <- lighten(team_colors, amount = light_amount)
  list(
    team_colors = team_colors,
    team_colors_light = team_colors_light,
    result_fill_values = c(team_colors_light, Push = "grey70"),
    total_fill_values = c(
      Under = "#C85B5B",
      Over = "#3B6FB6",
      Push = "grey70"
    ),
    home_away_colors = list(
      home = "#35B779",
      away = "#482777"
    )
  )
}

get_prediction_context <- function(team_strength_negbinom_summary) {
  pick_one <- function(x, name) {
    x <- unique(x)
    x <- x[!is.na(x)]
    if (length(x) == 0L) {
      cli_warn("No value found for {.val {name}}; predictions may be empty.")
      return(NA_integer_)
    }
    if (length(x) > 1L) {
      cli_warn(
        "Multiple values found for {.val {name}}; using max(): {.val {x}}."
      )
      return(max(x))
    }
    x[[1]]
  }

  list(
    filter_season = pick_one(
      team_strength_negbinom_summary$filtered_season,
      "filtered_season"
    ),
    filter_week = pick_one(
      team_strength_negbinom_summary$filtered_week,
      "filtered_week"
    ),
    predict_season = pick_one(
      team_strength_negbinom_summary$predicted_season,
      "predicted_season"
    ),
    predict_week = pick_one(
      team_strength_negbinom_summary$predicted_week,
      "predicted_week"
    )
  )
}

prepare_schedule_indices <- function(game_data, teams) {
  game_data |>
    dplyr$arrange(season, week) |>
    dplyr$mutate(
      game_idx = dplyr$row_number(),
      season_idx = as.integer(as.factor(season)),
      week_idx = if ("week_seq" %in% names(game_data)) {
        as.integer(week_seq)
      } else {
        as.integer(dplyr$dense_rank(paste(season, week, sep = "_")))
      },
      fw_season_idx = as.integer(ifelse(week == 1, 1, 0)),
      lw_season_idx = as.integer(ifelse(game_type == "SB", 1, 0)),
      home_idx = match(home_team, teams),
      away_idx = match(away_team, teams),
      hfa = as.integer(ifelse(location == "Home", 1, 0))
    )
}

export(make_team_palettes, get_prediction_context, prepare_schedule_indices)
