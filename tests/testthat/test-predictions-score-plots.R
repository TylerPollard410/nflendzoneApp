box::use(
  tibble[tibble],
  testthat[expect_s3_class, expect_true, test_that],
)

box::use(
  app / logic / predictions_games[build_score_prob_plot, make_score_plot_prep],
)

test_that("home/away score plot prep and plot build work", {
  game_draws <- tibble(
    mu_home = c(24, 21, 17, 35),
    mu_away = c(20, 17, 21, 14),
    mu_result = mu_home - mu_away,
    mu_total = mu_home + mu_away,
    y_home = c(27, 14, 10, 31),
    y_away = c(17, 14, 20, 21),
    y_result = y_home - y_away,
    y_total = y_home + y_away
  )

  probs <- list(
    home_team = "HOM",
    away_team = "AWY",
    mu = list(
      p_home_over = 0.55,
      p_home_under = 0.15,
      p_away_over = 0.2,
      p_away_under = 0.1,
      p_home_cover = 0.7,
      p_away_cover = 0.3,
      p_over = 0.75,
      p_under = 0.25
    ),
    y = list(
      p_home_over = 0.55,
      p_home_under = 0.15,
      p_away_over = 0.2,
      p_away_under = 0.1,
      p_home_cover = 0.7,
      p_away_cover = 0.3,
      p_over = 0.75,
      p_under = 0.25
    )
  )

  spread_line <- 3
  total_line <- 44.5

  prep_mu <- make_score_plot_prep(
    game_draws = game_draws,
    probs = probs,
    kind = "mu",
    spread_line_comp = spread_line,
    total_line_comp = total_line
  )
  expect_true(all(
    c("pred_home", "pred_away", "result_bin", "total_bin") %in%
      names(prep_mu$plot_draws)
  ))
  expect_true(nrow(prep_mu$region_labels) == 4)
  expect_s3_class(
    suppressWarnings(build_score_prob_plot(
      plot_prep = prep_mu,
      kind = "mu",
      palettes = list(team_colors_light = c(HOM = "grey80", AWY = "grey60")),
      spread_line = spread_line,
      total_line = total_line
    )),
    "ggplot"
  )

  prep_y <- make_score_plot_prep(
    game_draws = game_draws,
    probs = probs,
    kind = "y",
    spread_line_comp = spread_line,
    total_line_comp = total_line
  )
  expect_true(all(
    c("pred_home", "pred_away", "result_bin", "total_bin") %in%
      names(prep_y$plot_draws)
  ))
  expect_true(nrow(prep_y$region_labels) == 4)
  expect_s3_class(
    suppressWarnings(build_score_prob_plot(
      plot_prep = prep_y,
      kind = "y",
      palettes = list(team_colors_light = c(HOM = "grey80", AWY = "grey60")),
      spread_line = spread_line,
      total_line = total_line
    )),
    "ggplot"
  )
})
