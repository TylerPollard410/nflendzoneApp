box::use(
  ggplot2[ggplot_build],
  testthat[expect_s3_class, test_that],
  tibble[tibble],
)

box::use(
  app /
    logic /
    predictions /
    games /
    plot_prep[make_joint_plot_prep],
  app / logic / predictions / games / plots[build_joint_prob_plot],
)

test_that("joint result/total plot builds", {
  game_draws <- tibble(
    mu_result = c(-7, -3, 0, 3, 7),
    mu_total = c(38, 41, 44, 47, 50),
    y_result = c(-10, -4, 0, 4, 10),
    y_total = c(35, 40, 45, 50, 55)
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

  palettes <- list(
    result_fill_values = c(HOM = "grey80", AWY = "grey60", Push = "grey90"),
    total_fill_values = c(
      Under = "steelblue3",
      Over = "orange2",
      Push = "grey90"
    )
  )

  spread_line <- 3
  total_line <- 44.5

  prep_mu <- make_joint_plot_prep(
    game_draws = game_draws,
    probs = probs,
    kind = "mu",
    spread_line_comp = spread_line,
    total_line_comp = total_line
  )
  expect_s3_class(
    suppressWarnings({
      p <- build_joint_prob_plot(
        plot_prep = prep_mu,
        kind = "mu",
        palettes = palettes,
        spread_line = spread_line,
        total_line = total_line
      )
      ggplot_build(p)
      p
    }),
    "ggplot"
  )

  prep_y <- make_joint_plot_prep(
    game_draws = game_draws,
    probs = probs,
    kind = "y",
    spread_line_comp = spread_line,
    total_line_comp = total_line
  )
  expect_s3_class(
    suppressWarnings({
      p <- build_joint_prob_plot(
        plot_prep = prep_y,
        kind = "y",
        palettes = palettes,
        spread_line = spread_line,
        total_line = total_line
      )
      ggplot_build(p)
      p
    }),
    "ggplot"
  )
})
