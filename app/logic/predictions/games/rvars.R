box::use(
  box[export],
  dplyr,
  posterior[ndraws, rvar_rng],
  stats[rnbinom, rnorm],
  tidybayes[spread_rvars, unnest_rvars],
  tidyr[pivot_wider],
)

box::use(
  app / logic / predictions / games / core[get_prediction_context],
)

get_team_strength_rvars <- function(nb_sum_data, teams, ndraws = 4000) {
  pred_context <- get_prediction_context(nb_sum_data)
  nb_sum_data |>
    dplyr$ungroup() |>
    dplyr$mutate(
      rvariable = rvar_rng(
        rnorm,
        n = dplyr$n(),
        mean = mean,
        sd = sd,
        ndraws = ndraws
      ),
      .after = variable
    ) |>
    dplyr$select(variable, rvariable) |>
    pivot_wider(
      names_from = variable,
      values_from = rvariable
    ) |>
    unnest_rvars() |>
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
    dplyr$relocate(team, .before = 1) |>
    dplyr$mutate(
      team = teams[team],
      filtered_season = pred_context$filter_season,
      filtered_week = pred_context$filter_week,
      predicted_season = pred_context$predict_season,
      predicted_week = pred_context$predict_week,
      .before = 1
    )
}

get_filtered_rvars <- function(strength_rvars) {
  strength_rvars |>
    dplyr$select(
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
    dplyr$mutate(
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
    dplyr$mutate(
      n = ndraws(filtered_team_off_strength)
    )
}

get_predicted_rvars <- function(pred_games, strength_rvars) {
  pred_games |>
    dplyr$left_join(
      strength_rvars |>
        dplyr$select(
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
    dplyr$left_join(
      strength_rvars |>
        dplyr$select(
          season = predicted_season,
          week = predicted_week,
          team,
          away_predicted_team_off_strength = predicted_team_off_strength,
          away_predicted_team_def_strength = predicted_team_def_strength
        ),
      by = c("season", "week", "away_team" = "team")
    ) |>
    dplyr$mutate(
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
    dplyr$mutate(
      y_home = rvar_rng(
        rnbinom,
        n = dplyr$n(),
        mu = mu_home,
        size = phi_home
      ),
      y_away = rvar_rng(
        rnbinom,
        n = dplyr$n(),
        mu = mu_away,
        size = phi_away
      ),
      y_result = y_home - y_away,
      y_total = y_home + y_away
    )
}

export(get_team_strength_rvars, get_filtered_rvars, get_predicted_rvars)
