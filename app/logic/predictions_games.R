box::use(
  cli[cli_warn],
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
    n,
    distinct
  ],
  ggnewscale[new_scale_fill],
  ggplot2[
    ggplot,
    aes,
    geom_text,
    geom_abline,
    geom_hline,
    geom_label,
    geom_vline,
    geom_histogram,
    labs,
    scale_fill_manual,
    scale_fill_viridis_c,
    stat_bin_hex,
    theme,
    theme_minimal,
    after_stat,
    element_blank,
    element_line,
    element_rect,
    element_text,
    rel
  ],
  ggside[
    geom_xsidehistogram,
    geom_ysidehistogram,
    geom_xsidetext,
    geom_ysidetext,
    theme_ggside_void
  ],
  grid[unit],
  plotly[
    ggplotly,
    subplot,
    plotly_empty,
    layout
  ],
  thematic[
    thematic_get_option,
    thematic_get_mixture,
    thematic_theme,
    thematic_with_theme
  ],
  tidyr[pivot_wider],
  posterior[
    Pr,
    rvar_rng,
    ndraws
  ],
  tidybayes[
    unnest_rvars,
    spread_rvars
  ],
  tibble[tibble],
  scales[percent, breaks_pretty, label_number],
  rlang[.data],
  stats[rnorm, rnbinom, setNames]
)

#' @export
make_team_palettes <- function(teams_data, light_amount = 0.25) {
  team_colors <- setNames(teams_data$team_color, teams_data$team_abbr)
  team_colors_light <- lighten(team_colors, amount = light_amount)
  list(
    team_colors = team_colors,
    team_colors_light = team_colors_light,
    result_fill_values = c(team_colors_light, Push = "grey70"),
    total_fill_values = c(
      Under = "steelblue3",
      Over = "orange2",
      Push = "grey70"
    )
  )
}

#' @export
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
get_team_strength_rvars <- function(nb_sum_data, teams, ndraws = 4000) {
  pred_context <- get_prediction_context(nb_sum_data)
  nb_sum_data |>
    ungroup() |>
    mutate(
      rvariable = rvar_rng(
        rnorm,
        n = n(),
        mean = mean,
        sd = sd,
        ndraws = ndraws
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
      filtered_season = pred_context$filter_season,
      filtered_week = pred_context$filter_week,
      predicted_season = pred_context$predict_season,
      predicted_week = pred_context$predict_week,
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

#' @export
compute_game_probabilities <- function(game_rvars, spread_line, total_line) {
  game_row <- game_rvars |>
    distinct(home_team, away_team, .keep_all = TRUE) |>
    dplyr::slice_head(n = 1)

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

#' Convert probability to American odds (implied / fair odds).
#'
#' @param p Numeric vector of probabilities in [0, 1]
#' @param digits Integer; rounding for returned odds
#' @return Integer vector (American odds); NA for invalid inputs
#' @export
prob_to_american_odds <- function(p, digits = 0L) {
  p <- as.numeric(p)

  out <- rep(NA_real_, length(p))
  ok <- is.finite(p) & !is.na(p) & p >= 0 & p <= 1

  # Extremes: certain or impossible
  out[ok & p == 1] <- -Inf
  out[ok & p == 0] <- Inf

  # Standard cases
  fav <- ok & p > 0 & p < 1 & p > 0.5
  dog <- ok & p > 0 & p < 1 & p < 0.5
  even <- ok & p == 0.5

  out[fav] <- -100 * (p[fav] / (1 - p[fav]))
  out[dog] <- 100 * ((1 - p[dog]) / p[dog])
  out[even] <- 100

  as.integer(round(out, digits = digits))
}

#' @export
make_game_labels <- function(home_team, away_team, probs_kind) {
  quad_labels <- tibble(
    x = c(Inf, Inf, -Inf, -Inf),
    y = c(Inf, -Inf, Inf, -Inf),
    hjust = c(1.1, 1.1, -0.1, -0.1),
    vjust = c(1.1, -0.1, 1.1, -0.1),
    label = c(
      paste0(
        home_team,
        " cover\nOver\n",
        percent(probs_kind$p_home_over, 0.1)
      ),
      paste0(
        home_team,
        " cover\nUnder\n",
        percent(probs_kind$p_home_under, 0.1)
      ),
      paste0(
        away_team,
        " cover\nOver\n",
        percent(probs_kind$p_away_over, 0.1)
      ),
      paste0(
        away_team,
        " cover\nUnder\n",
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

#' @export
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
      mutate(
        pred_result = .data$mu_result,
        pred_total = .data$mu_total
      )
  } else {
    plot_draws <- game_draws |>
      mutate(
        pred_result = .data$y_result,
        pred_total = .data$y_total
      )
  }

  plot_draws <- plot_draws |>
    mutate(
      result_bin = dplyr::case_when(
        pred_result > spread_line_comp ~ home_team,
        pred_result < spread_line_comp ~ away_team,
        TRUE ~ "Push"
      ),
      total_bin = dplyr::case_when(
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

#' @export
make_score_plot_prep <- function(
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
      mutate(
        pred_home = .data$mu_home,
        pred_away = .data$mu_away,
        pred_result = .data$mu_result,
        pred_total = .data$mu_total
      )
  } else {
    plot_draws <- game_draws |>
      mutate(
        pred_home = .data$y_home,
        pred_away = .data$y_away,
        pred_result = .data$y_result,
        pred_total = .data$y_total
      )
  }

  plot_draws <- plot_draws |>
    mutate(
      result_bin = dplyr::case_when(
        pred_result > spread_line_comp ~ home_team,
        pred_result < spread_line_comp ~ away_team,
        TRUE ~ "Push"
      ),
      total_bin = dplyr::case_when(
        pred_total > total_line_comp ~ "Over",
        pred_total < total_line_comp ~ "Under",
        TRUE ~ "Push"
      )
    )

  base_labels <- make_game_labels(home_team, away_team, probs_kind)$quad_labels

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

  x0 <- (total_line_comp + spread_line_comp) / 2
  y0 <- (total_line_comp - spread_line_comp) / 2
  delta <- 0.15 * max(diff(x_rng), diff(y_rng), na.rm = TRUE)
  if (!is.finite(delta) || delta <= 0) {
    delta <- 1
  }

  region_labels <- tibble(
    x = c(x0 + delta, x0, x0, x0 - delta),
    y = c(y0, y0 - delta, y0 + delta, y0),
    label = base_labels$label
  ) |>
    mutate(
      x = clamp(.data$x, x_rng, x_margin),
      y = clamp(.data$y, y_rng, y_margin)
    )

  list(
    plot_draws = plot_draws,
    home_team = home_team,
    away_team = away_team,
    region_labels = region_labels
  )
}

#' @export
build_joint_prob_plot <- function(
  plot_prep,
  kind,
  palettes,
  spread_line,
  total_line
  # base_size,
  # label_text_size
) {
  kind <- match.arg(kind, c("mu", "y"))
  hex_binwidth <- if (kind == "mu") c(1, 1) else c(2, 2)

  thematic_with_theme(
    thematic_theme(fg = "auto", bg = "auto", accent = "auto"),
    {
      # text_color <- as.character(thematic_get_option("fg"))[[1]]
      # plot_bg <- as.character(thematic_get_option("bg"))[[1]]
      # if (
      #   is.null(text_color) ||
      #     is.na(text_color) ||
      #     identical(text_color, "auto")
      # ) {
      #   text_color <- "#212529"
      # }
      # if (is.null(plot_bg) || is.na(plot_bg) || identical(plot_bg, "auto")) {
      #   plot_bg <- "#ffffff"
      # }

      # label_fill <- NULL #thematic_get_mixture(0.1, default = plot_bg)
      # if (is.null(label_fill) || is.na(label_fill)) {
      #   label_fill <- plot_bg
      # }

      # grid_color <- thematic_get_mixture(0.2, default = text_color)
      # if (is.null(grid_color) || is.na(grid_color)) {
      #   grid_color <- "#dee2e6"
      # }

      text_color <- thematic_get_option("fg")
      plot_bg <- thematic_get_option("bg")
      label_fill <- plot_bg
      grid_color <- thematic_get_mixture(0.2, default = text_color)

      plot_prep$plot_draws |>
        ggplot(aes(pred_result, pred_total)) +
        stat_bin_hex(binwidth = hex_binwidth) +
        scale_fill_viridis_c(
          breaks = breaks_pretty(6),
          labels = label_number(),
          name = "Count"
        ) +
        geom_vline(
          xintercept = spread_line,
          linetype = 2,
          color = "red"
        ) +
        geom_hline(
          yintercept = total_line,
          linetype = 2,
          color = "red"
        ) +
        geom_label(
          data = plot_prep$quad_labels,
          aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
          inherit.aes = FALSE,
          #size = label_text_size,
          fill = label_fill,
          color = text_color,
          alpha = 0.8,
          linewidth = 0
        ) +
        new_scale_fill() +
        geom_xsidehistogram(
          aes(y = after_stat(count), fill = result_bin),
          binwidth = 1,
          boundary = spread_line,
          alpha = 0.7
        ) +
        scale_fill_manual(values = palettes$result_fill_values) +
        new_scale_fill() +
        geom_ysidehistogram(
          aes(x = after_stat(count), fill = total_bin),
          binwidth = 1,
          boundary = total_line,
          alpha = 0.7
        ) +
        scale_fill_manual(values = palettes$total_fill_values) +
        geom_xsidetext(
          data = plot_prep$xside_labels,
          aes(x = x, y = Inf, label = label, hjust = hjust),
          inherit.aes = FALSE,
          vjust = 1.1,
          #size = label_text_size,
          color = text_color
        ) +
        geom_ysidetext(
          data = plot_prep$yside_labels,
          aes(x = Inf, y = y, label = label, vjust = vjust),
          inherit.aes = FALSE,
          hjust = 1.1,
          #size = label_text_size,
          color = text_color
        ) +
        labs(x = "Result", y = "Total") +
        #theme_minimal(base_size = base_size) +
        theme_minimal() +
        theme_ggside_void() +
        theme(
          plot.background = element_rect(fill = plot_bg, color = NA),
          panel.background = element_rect(fill = plot_bg, color = NA),
          text = element_text(color = text_color),
          strip.text = element_text(color = text_color, size = rel(1.1)),
          plot.title = element_text(color = text_color, size = rel(1.1)),
          axis.title = element_text(
            color = text_color,
            size = rel(1.1),
            face = "bold"
          ),
          axis.text = element_text(color = text_color, size = rel(1)),
          panel.grid = element_line(color = grid_color),
          ggside.axis.ticks.length = grid::unit(0, "points"),
          ggside.axis.minor.ticks.length = grid::unit(0, "pt"),
          ggside.panel.scale = 0.25,
          ggside.axis.line = element_blank(),
          #ggside.axis.ticks = element_blank(),
          ggside.axis.text = element_blank(),
          legend.position = "none"
        )
    }
  )
}

#' @export
build_score_prob_plot <- function(
  plot_prep,
  kind,
  palettes,
  spread_line,
  total_line
) {
  kind <- match.arg(kind, c("mu", "y"))
  hex_binwidth <- if (kind == "mu") c(1, 1) else c(2, 2)

  home_fill <- palettes$team_colors_light[[plot_prep$home_team]]
  away_fill <- palettes$team_colors_light[[plot_prep$away_team]]
  if (is.null(home_fill) || is.na(home_fill)) {
    home_fill <- "grey70"
  }
  if (is.null(away_fill) || is.na(away_fill)) {
    away_fill <- "grey70"
  }

  thematic_with_theme(
    thematic_theme(fg = "auto", bg = "auto", accent = "auto"),
    {
      text_color <- thematic_get_option("fg")
      plot_bg <- thematic_get_option("bg")
      label_fill <- plot_bg
      grid_color <- thematic_get_mixture(0.2, default = text_color)

      plot_prep$plot_draws |>
        ggplot(aes(pred_home, pred_away)) +
        stat_bin_hex(binwidth = hex_binwidth) +
        scale_fill_viridis_c(
          breaks = breaks_pretty(6),
          labels = label_number(),
          name = "Count"
        ) +
        geom_abline(
          intercept = -spread_line,
          slope = 1,
          linetype = 2,
          color = "red"
        ) +
        geom_abline(
          intercept = total_line,
          slope = -1,
          linetype = 2,
          color = "red"
        ) +
        geom_label(
          data = plot_prep$region_labels,
          aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          fill = label_fill,
          color = text_color,
          alpha = 0.85,
          linewidth = 0
        ) +
        geom_xsidehistogram(
          aes(y = after_stat(count)),
          binwidth = 1,
          alpha = 0.7,
          fill = home_fill
        ) +
        geom_ysidehistogram(
          aes(x = after_stat(count)),
          binwidth = 1,
          alpha = 0.7,
          fill = away_fill
        ) +
        labs(x = "Home points", y = "Away points") +
        theme_minimal() +
        theme_ggside_void() +
        theme(
          plot.background = element_rect(fill = plot_bg, color = NA),
          panel.background = element_rect(fill = plot_bg, color = NA),
          text = element_text(color = text_color),
          strip.text = element_text(color = text_color, size = rel(1.1)),
          plot.title = element_text(color = text_color, size = rel(1.1)),
          axis.title = element_text(
            color = text_color,
            size = rel(1.1),
            face = "bold"
          ),
          axis.text = element_text(color = text_color, size = rel(1)),
          panel.grid = element_line(color = grid_color),
          ggside.axis.ticks.length = grid::unit(0, "points"),
          ggside.axis.minor.ticks.length = grid::unit(0, "pt"),
          ggside.panel.scale = 0.25,
          ggside.axis.line = element_blank(),
          ggside.axis.text = element_blank(),
          legend.position = "none"
        )
    }
  )
}

#' @export
build_joint_prob_plot_int <- function(
  plot_prep,
  kind,
  palettes,
  spread_line,
  total_line
  # base_size,
  # label_text_size
) {
  kind <- match.arg(kind, c("mu", "y"))
  hex_binwidth <- if (kind == "mu") c(1, 1) else c(2, 2)

  thematic_with_theme(
    thematic_theme(fg = "auto", bg = "auto", accent = "auto"),
    {
      # text_color <- as.character(thematic_get_option("fg"))[[1]]
      # plot_bg <- as.character(thematic_get_option("bg"))[[1]]
      # if (
      #   is.null(text_color) ||
      #     is.na(text_color) ||
      #     identical(text_color, "auto")
      # ) {
      #   text_color <- "#212529"
      # }
      # if (is.null(plot_bg) || is.na(plot_bg) || identical(plot_bg, "auto")) {
      #   plot_bg <- "#ffffff"
      # }

      # label_fill <- NULL #thematic_get_mixture(0.1, default = plot_bg)
      # if (is.null(label_fill) || is.na(label_fill)) {
      #   label_fill <- plot_bg
      # }

      # grid_color <- thematic_get_mixture(0.2, default = text_color)
      # if (is.null(grid_color) || is.na(grid_color)) {
      #   grid_color <- "#dee2e6"
      # }
      text_color <- thematic_get_option("fg")
      plot_bg <- thematic_get_option("bg")
      label_fill <- plot_bg
      grid_color <- thematic_get_mixture(0.2, default = text_color)

      # text_color <- text_color <- "#212529"
      # plot_bg <- plot_bg <- "#ffffff"
      # label_fill <- plot_bg
      # grid_color <- grid_color <- "#dee2e6"

      p1 <- plot_prep$plot_draws |>
        ggplot(aes(pred_result, pred_total)) +
        stat_bin_hex(binwidth = hex_binwidth) +
        scale_fill_viridis_c(
          breaks = breaks_pretty(6),
          labels = label_number(),
          name = "Count"
        ) +
        geom_vline(
          xintercept = spread_line,
          linetype = 2,
          color = "red"
        ) +
        geom_hline(
          yintercept = total_line,
          linetype = 2,
          color = "red"
        ) +
        geom_text(
          data = plot_prep$quad_labels,
          aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust)
          #inherit.aes = FALSE
          #size = label_text_size,
          #fill = label_fill,
          #color = text_color
          #alpha = 0.8,
          #linewidth = 0
        ) +
        labs(x = "Result", y = "Total") +
        #theme_minimal(base_size = base_size) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = plot_bg, color = NA),
          panel.background = element_rect(fill = plot_bg, color = NA),
          text = element_text(color = text_color),
          strip.text = element_text(color = text_color, size = rel(1.1)),
          plot.title = element_text(color = text_color, size = rel(1.1)),
          axis.title = element_text(
            color = text_color,
            size = rel(1.1),
            face = "bold"
          ),
          ggside.axis.ticks.length = grid::unit(0, "points"),
          ggside.axis.minor.ticks.length = grid::unit(0, "pt"),
          axis.text = element_text(color = text_color, size = rel(1)),
          panel.grid = element_line(color = grid_color),
          legend.position = "none"
        )
      pl1 <- ggplotly(p1, type = "scatter")

      p2 <- plot_prep$plot_draws |>
        ggplot(aes(pred_result, pred_total)) +
        geom_histogram(
          aes(
            y = after_stat(count) / nrow(plot_prep$plot_draws),
            fill = result_bin
          ),
          binwidth = 1,
          boundary = spread_line,
          alpha = 0.7
        ) +
        scale_fill_manual(values = palettes$result_fill_values) +
        geom_text(
          data = plot_prep$xside_labels,
          aes(x = x, y = Inf, label = label, hjust = hjust),
          #inherit.aes = FALSE,
          vjust = 1.1
          #size = label_text_size,
          #color = text_color
        ) +
        labs(x = "Result", y = "Total") +
        #theme_minimal(base_size = base_size) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = plot_bg, color = NA),
          panel.background = element_rect(fill = plot_bg, color = NA),
          text = element_text(color = text_color),
          strip.text = element_text(color = text_color, size = rel(1.1)),
          plot.title = element_text(color = text_color, size = rel(1.1)),
          axis.title = element_text(
            color = text_color,
            size = rel(1.1),
            face = "bold"
          ),
          axis.text = element_text(color = text_color, size = rel(1)),
          panel.grid.major.x = element_line(color = grid_color),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.ticks.length = grid::unit(0, "points"),
          axis.minor.ticks.length = grid::unit(0, "pt"),
          axis.line = element_blank(),
          #axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none"
        )
      pl2 <- ggplotly(p2)

      p3 <- plot_prep$plot_draws |>
        ggplot(aes(pred_result, pred_total)) +
        geom_histogram(
          aes(
            x = after_stat(count) / nrow(plot_prep$plot_draws),
            fill = total_bin
          ),
          binwidth = 1,
          boundary = total_line,
          alpha = 0.7
        ) +
        scale_fill_manual(values = palettes$total_fill_values) +
        geom_text(
          data = plot_prep$yside_labels,
          aes(x = Inf, y = y, label = label, vjust = vjust),
          #inherit.aes = FALSE,
          hjust = 1.1
          #size = label_text_size,
          #color = text_color
        ) +
        labs(x = "Result", y = "Total") +
        #theme_minimal(base_size = base_size) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = plot_bg, color = NA),
          panel.background = element_rect(fill = plot_bg, color = NA),
          text = element_text(color = text_color),
          strip.text = element_text(color = text_color, size = rel(1.1)),
          plot.title = element_text(color = text_color, size = rel(1.1)),
          axis.title = element_text(
            color = text_color,
            size = rel(1.1),
            face = "bold"
          ),
          axis.text = element_text(color = text_color, size = rel(1)),
          panel.grid.major.y = element_line(color = grid_color),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks.length = grid::unit(0, "points"),
          axis.minor.ticks.length = grid::unit(0, "pt"),
          axis.line = element_blank(),
          #axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "none"
        )
      pl3 <- ggplotly(p3)

      subplot(
        pl2,
        plotly_empty(),
        pl1,
        pl3,
        nrows = 2,
        heights = c(0.2, 0.8),
        widths = c(0.8, 0.2),
        margin = 0,
        shareX = TRUE,
        shareY = TRUE,
        titleX = TRUE,
        titleY = TRUE
      ) |>
        layout(showlegend = FALSE)
    }
  )
}
