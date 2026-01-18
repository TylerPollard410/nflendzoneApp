box::use(
  box[export],
  dplyr,
  rlang[.data],
  tibble[tibble],
)

box::use(
  app / logic / predictions / games / probabilities[make_game_labels],
)

make_joint_plot_prep <- function(
  game_draws,
  probs,
  kind,
  spread_line_comp,
  total_line_comp
) {
  kind <- match.arg(kind, c("mu", "y"))

  home_team <- probs$home_team
  away_team <- probs$away_team
  probs_kind <- probs[[kind]]

  if (kind == "mu") {
    plot_draws <- game_draws |>
      dplyr$mutate(
        pred_result = .data$mu_result,
        pred_total = .data$mu_total
      )
  } else {
    plot_draws <- game_draws |>
      dplyr$mutate(
        pred_result = .data$y_result,
        pred_total = .data$y_total
      )
  }

  plot_draws <- plot_draws |>
    dplyr$mutate(
      result_bin = dplyr$case_when(
        pred_result > spread_line_comp ~ home_team,
        pred_result < spread_line_comp ~ away_team,
        TRUE ~ "Push"
      ),
      total_bin = dplyr$case_when(
        pred_total > total_line_comp ~ "Over",
        pred_total < total_line_comp ~ "Under",
        TRUE ~ "Push"
      )
    )

  labels <- make_game_labels(home_team, away_team, probs_kind)

  c(
    list(
      plot_draws = plot_draws,
      home_team = home_team,
      away_team = away_team
    ),
    labels
  )
}

make_score_plot_prep <- function(
  game_draws,
  probs,
  kind,
  use_clamp = FALSE,
  spread_line_comp,
  total_line_comp
) {
  kind <- match.arg(kind, c("mu", "y"))

  home_team <- probs$home_team
  away_team <- probs$away_team
  probs_kind <- probs[[kind]]

  if (kind == "mu") {
    plot_draws <- game_draws |>
      dplyr$mutate(
        pred_home = .data$mu_home,
        pred_away = .data$mu_away,
        pred_result = .data$mu_result,
        pred_total = .data$mu_total
      )
  } else {
    plot_draws <- game_draws |>
      dplyr$mutate(
        pred_home = .data$y_home,
        pred_away = .data$y_away,
        pred_result = .data$y_result,
        pred_total = .data$y_total
      )
  }

  plot_draws <- plot_draws |>
    dplyr$mutate(
      result_bin = dplyr$case_when(
        pred_result > spread_line_comp ~ home_team,
        pred_result < spread_line_comp ~ away_team,
        TRUE ~ "Push"
      ),
      total_bin = dplyr$case_when(
        pred_total > total_line_comp ~ "Over",
        pred_total < total_line_comp ~ "Under",
        TRUE ~ "Push"
      )
    )

  base_labels <- make_game_labels(
    home_team,
    away_team,
    probs_kind,
    wrap = FALSE
  )$quad_labels

  clamp <- function(x, lim, margin = 0) {
    if (any(!is.finite(lim)) || length(lim) != 2L) {
      return(x)
    }
    lo <- lim[[1]] + margin
    hi <- lim[[2]] - margin
    pmin(pmax(x, lo), hi)
  }

  x_rng <- range(plot_draws$pred_home, na.rm = TRUE)
  y_rng <- range(plot_draws$pred_away, na.rm = TRUE)
  x_margin <- if (is.finite(diff(x_rng))) 0.06 * diff(x_rng) else 0
  y_margin <- if (is.finite(diff(y_rng))) 0.06 * diff(y_rng) else 0
  score_rng <- range(c(x_rng, y_rng), na.rm = TRUE)
  score_pad <- if (is.finite(diff(score_rng))) 0.06 * diff(score_rng) else 1
  score_limits <- c(score_rng[[1]] - score_pad, score_rng[[2]] + score_pad)

  x0 <- (total_line_comp + spread_line_comp) / 2
  y0 <- (total_line_comp - spread_line_comp) / 2
  delta <- 0.15 * max(diff(x_rng), diff(y_rng), na.rm = TRUE)
  if (!is.finite(delta) || delta <= 0) {
    delta <- 1
  }

  region_labels <- tibble(
    x = c(Inf, x0, x0, -Inf),
    y = c(y0, -Inf, Inf, y0),
    label = base_labels$label
  )
  if (use_clamp) {
    region_labels <- region_labels |>
      dplyr$mutate(
        x = clamp(.data$x, x_rng, x_margin),
        y = clamp(.data$y, y_rng, y_margin)
      )
  }

  list(
    plot_draws = plot_draws,
    home_team = home_team,
    away_team = away_team,
    region_labels = region_labels,
    score_limits = score_limits
  )
}

export(make_joint_plot_prep, make_score_plot_prep)
