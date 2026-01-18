box::use(
  box[export],
  dplyr,
  posterior[Pr],
  scales[percent],
  tibble[tibble],
)

compute_game_probabilities <- function(game_rvars, spread_line, total_line) {
  game_row <- game_rvars |>
    dplyr$distinct(home_team, away_team, .keep_all = TRUE) |>
    dplyr$slice_head(n = 1)

  if (nrow(game_row) == 0L) {
    return(NULL)
  }

  home_team <- game_row$home_team[[1]]
  away_team <- game_row$away_team[[1]]

  mu_result <- game_row$mu_result[[1]]
  mu_total <- game_row$mu_total[[1]]
  y_result <- game_row$y_result[[1]]
  y_total <- game_row$y_total[[1]]

  list(
    home_team = home_team,
    away_team = away_team,
    mu = list(
      p_home_over = Pr(mu_result > spread_line & mu_total > total_line),
      p_home_under = Pr(mu_result > spread_line & mu_total < total_line),
      p_away_over = Pr(mu_result < spread_line & mu_total > total_line),
      p_away_under = Pr(mu_result < spread_line & mu_total < total_line),
      p_home_cover = Pr(mu_result > spread_line),
      p_away_cover = Pr(mu_result < spread_line),
      p_over = Pr(mu_total > total_line),
      p_under = Pr(mu_total < total_line)
    ),
    y = list(
      p_home_over = Pr(y_result > spread_line & y_total > total_line),
      p_home_under = Pr(y_result > spread_line & y_total < total_line),
      p_away_over = Pr(y_result < spread_line & y_total > total_line),
      p_away_under = Pr(y_result < spread_line & y_total < total_line),
      p_home_cover = Pr(y_result > spread_line),
      p_away_cover = Pr(y_result < spread_line),
      p_over = Pr(y_total > total_line),
      p_under = Pr(y_total < total_line)
    )
  )
}

make_game_labels <- function(home_team, away_team, probs_kind, wrap = TRUE) {
  spacer <- ifelse(wrap, "\n", " ")
  quad_labels <- tibble(
    x = c(Inf, Inf, -Inf, -Inf),
    y = c(Inf, -Inf, Inf, -Inf),
    hjust = c(1.1, 1.1, -0.1, -0.1),
    vjust = c(1.1, -0.1, 1.1, -0.1),
    label = c(
      paste0(
        home_team,
        " cover",
        spacer,
        "Over",
        spacer,
        percent(probs_kind$p_home_over, 0.1)
      ),
      paste0(
        home_team,
        " cover",
        spacer,
        "Under",
        spacer,
        percent(probs_kind$p_home_under, 0.1)
      ),
      paste0(
        away_team,
        " cover",
        spacer,
        "Over",
        spacer,
        percent(probs_kind$p_away_over, 0.1)
      ),
      paste0(
        away_team,
        " cover",
        spacer,
        "Under",
        spacer,
        percent(probs_kind$p_away_under, 0.1)
      )
    )
  )

  xside_labels <- tibble(
    x = c(-Inf, Inf),
    y = Inf,
    hjust = c(-0.1, 1.1),
    label = c(
      paste0(
        away_team,
        " cover\n",
        percent(probs_kind$p_away_cover, 0.1)
      ),
      paste0(
        home_team,
        " cover\n",
        percent(probs_kind$p_home_cover, 0.1)
      )
    )
  )

  yside_labels <- tibble(
    y = c(-Inf, Inf),
    x = Inf,
    vjust = c(-0.1, 1.1),
    label = c(
      paste0("Under\n", percent(probs_kind$p_under, 0.1)),
      paste0("Over\n", percent(probs_kind$p_over, 0.1))
    )
  )

  list(
    quad_labels = quad_labels,
    xside_labels = xside_labels,
    yside_labels = yside_labels
  )
}

export(compute_game_probabilities, make_game_labels)
