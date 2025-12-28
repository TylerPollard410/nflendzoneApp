box::use(
  colorspace[lighten],
  dplyr[
    filter,
    mutate,
    select,
    ungroup,
    arrange,
    left_join,
    relocate,
    row_number,
    pull,
    n,
    distinct
  ],
  tidyr[pivot_wider],
  posterior[
    rvar_rng,
    ndraws
  ],
  tidybayes[
    unnest_rvars,
    spread_rvars
  ],
  stats[rnorm, rnbinom, setNames, quantile]
)

box::use(
  app /
    logic /
    data_startup[
      teams_data,
      teams,
      game_data,
      current_season,
      current_week,
      team_strength_negbinom_summary
    ],
  app / logic / data_import_functions,
)

filter_season <- unique(team_strength_negbinom_summary$filtered_season)
filter_week <- unique(team_strength_negbinom_summary$filtered_week)
predict_season <- unique(team_strength_negbinom_summary$predicted_season)
predict_week <- unique(team_strength_negbinom_summary$predicted_week)

game_data_predict <- game_data |>
  filter(
    season == predict_season,
    week == predict_week
  )

#' @export
game_select_choices <- game_data_predict$game_id

#' @export
spread_line_slider_init <- game_data_predict |>
  filter(game_id == game_select_choices[1]) |>
  pull(spread_line)

#' @export
total_line_slider_init <- game_data_predict |>
  filter(game_id == game_select_choices[1]) |>
  pull(total_line)

#' @export
team_colors <- setNames(teams_data$team_color, teams_data$team_abbr)

#' @export
team_colors_light <- lighten(team_colors, amount = 0.25)

#' @export
result_fill_values <- c(team_colors_light, Push = "grey70")

#' @export
total_fill_values <- c(Under = "steelblue3", Over = "orange2", Push = "grey70")

#' @export
prepare_schedule_indices <- function(game_data, teams) {
  game_data |>
    arrange(season, week) |>
    mutate(
      game_idx = row_number(),
      season_idx = as.integer(as.factor(season)),
      # Use existing week_seq if available, otherwise create it
      week_idx = if ("week_seq" %in% names(game_data)) {
        as.integer(week_seq)
      } else {
        as.integer(dplyr::dense_rank(paste(season, week, sep = "_")))
      },
      # Season boundary indicators
      fw_season_idx = as.integer(ifelse(week == 1, 1, 0)),
      lw_season_idx = as.integer(ifelse(game_type == "SB", 1, 0)),
      # Team indices
      home_idx = match(home_team, teams),
      away_idx = match(away_team, teams),
      # Home field advantage indicator
      hfa = as.integer(ifelse(location == "Home", 1, 0))
    )
}

#' @export
get_team_strength_rvars <- function(nb_sum_data) {
  filter_season <- unique(nb_sum_data$filtered_season)
  filter_week <- unique(nb_sum_data$filtered_week)
  predict_season <- unique(nb_sum_data$predicted_season)
  predict_week <- unique(nb_sum_data$predicted_week)

  nb_sum_data |>
    ungroup() |>
    mutate(
      rvariable = rvar_rng(
        rnorm,
        n = n(),
        mean = mean,
        sd = sd,
        ndraws = 4000
      ),
      .after = variable
    ) |>
    # mutate(team = as.numeric(factor(team))) |>
    # mutate(
    #   variable = ifelse(!is.na(team), paste0(variable, "[", team, "]"), variable)
    # ) |>
    select(variable, rvariable) |>
    pivot_wider(
      names_from = variable,
      values_from = rvariable
      # id_cols = c(
      #   "filtered_season",
      #   "filtered_week",
      #   "predicted_season",
      #   "predicted_week"
      # )
    ) |>
    unnest_rvars() |>
    #spread_rvars(!!!rlang::parse_exprs(week_vars)) |>
    spread_rvars(
      phi_home,
      phi_away,
      filtered_alpha_log,
      filtered_team_off_strength[team],
      filtered_team_def_strength[team],
      filtered_team_hfa[team],
      filtered_league_hfa,
      predicted_alpha_log,
      predicted_team_off_strength[team],
      predicted_team_def_strength[team],
      predicted_team_hfa[team],
      predicted_league_hfa
    ) |>
    relocate(team, .before = 1) |>
    mutate(
      team = teams[team],
      filtered_season = filter_season,
      filtered_week = filter_week,
      predicted_season = predict_season,
      predicted_week = predict_week,
      .before = 1
    )
}

#' @export
get_filtered_rvars <- function(strength_rvars) {
  strength_rvars |>
    select(
      season = filtered_season,
      week = filtered_week,
      team,
      filtered_alpha_log,
      phi_home,
      phi_away,
      filtered_team_off_strength,
      filtered_team_def_strength,
      filtered_team_hfa,
      filtered_league_hfa
    ) |>
    mutate(
      team_off_strength = exp(filtered_alpha_log + filtered_team_off_strength) -
        exp(filtered_alpha_log),
      team_def_strength = exp(filtered_alpha_log + filtered_team_def_strength) -
        exp(filtered_alpha_log),
      team_strength = team_off_strength + team_def_strength,
      team_hfa = exp(filtered_alpha_log + filtered_team_hfa) -
        exp(filtered_alpha_log),
      league_hfa = exp(filtered_alpha_log + filtered_league_hfa) -
        exp(filtered_alpha_log)
    ) |>
    mutate(
      n = ndraws(filtered_team_off_strength)
    )
}

#' @export
get_predicted_rvars <- function(pred_games, strength_rvars) {
  pred_games |>
    left_join(
      strength_rvars |>
        # filter(season == max(season)) |>
        select(
          season = predicted_season,
          week = predicted_week,
          team,
          predicted_alpha_log,
          phi_home,
          phi_away,
          home_predicted_team_off_strength = predicted_team_off_strength,
          home_predicted_team_def_strength = predicted_team_def_strength,
          home_predicted_team_hfa = predicted_team_hfa
        ),
      by = c("season", "week", "home_team" = "team")
    ) |>
    left_join(
      strength_rvars |>
        # filter(season == max(season)) |>
        select(
          season = predicted_season,
          week = predicted_week,
          team,
          away_predicted_team_off_strength = predicted_team_off_strength,
          away_predicted_team_def_strength = predicted_team_def_strength
        ),
      by = c("season", "week", "away_team" = "team")
    ) |>
    mutate(
      eta_home = predicted_alpha_log +
        home_predicted_team_off_strength -
        away_predicted_team_def_strength +
        (hfa * (home_predicted_team_hfa / 2)),
      eta_away = predicted_alpha_log +
        away_predicted_team_off_strength -
        home_predicted_team_def_strength -
        (hfa * (home_predicted_team_hfa / 2)),
      mu_home = exp(eta_home),
      mu_away = exp(eta_away),
      mu_result = mu_home - mu_away,
      mu_total = mu_home + mu_away
    ) |>
    mutate(
      y_home = rvar_rng(
        rnbinom,
        n = n(),
        mu = mu_home,
        size = phi_home
      ),
      #y_home2 = exp(y_home2),
      y_away = rvar_rng(
        rnbinom,
        n = n(),
        mu = mu_away,
        size = phi_away
      ),
      y_result = y_home - y_away,
      y_total = y_home + y_away
    )
}
