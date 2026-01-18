box::use(
  box[export],
  ggnewscale[new_scale_fill],
  ggplot2,
  ggplot2[after_stat],
  ggside,
  grid[unit],
  plotly[ggplotly, layout, plotly_empty, subplot],
  scales[breaks_pretty, label_number],
  thematic[
    thematic_get_mixture,
    thematic_get_option,
    thematic_theme,
    thematic_with_theme
  ],
)

build_joint_prob_plot <- function(
  plot_prep,
  kind,
  palettes,
  spread_line,
  total_line,
  show_spread_line = TRUE,
  show_total_line = TRUE,
  show_prob_labels = TRUE
) {
  kind <- match.arg(kind, c("mu", "y"))
  hex_binwidth <- if (kind == "mu") c(1, 1) else c(2, 2)

  thematic_with_theme(
    thematic_theme(fg = "auto", bg = "auto", accent = "auto"),
    {
      text_color <- thematic_get_option("fg")
      plot_bg <- thematic_get_option("bg")
      label_fill <- plot_bg
      grid_color <- thematic_get_mixture(0.2, default = text_color)

      spread_layer <- if (show_spread_line) {
        ggplot2$geom_vline(
          xintercept = spread_line,
          linetype = 2,
          color = "red"
        )
      } else {
        NULL
      }
      total_layer <- if (show_total_line) {
        ggplot2$geom_hline(
          yintercept = total_line,
          linetype = 2,
          color = "red"
        )
      } else {
        NULL
      }
      quad_label_layer <- if (show_prob_labels) {
        ggplot2$geom_label(
          data = plot_prep$quad_labels,
          ggplot2$aes(
            x = x,
            y = y,
            label = label,
            hjust = hjust,
            vjust = vjust
          ),
          inherit.aes = FALSE,
          fill = label_fill,
          color = text_color,
          alpha = 0.8,
          linewidth = 0
        )
      } else {
        NULL
      }
      xside_label_layer <- if (show_prob_labels) {
        ggside$geom_xsidetext(
          data = plot_prep$xside_labels,
          ggplot2$aes(x = x, y = Inf, label = label, hjust = hjust),
          inherit.aes = FALSE,
          vjust = 1.1,
          color = text_color
        )
      } else {
        NULL
      }
      yside_label_layer <- if (show_prob_labels) {
        ggside$geom_ysidetext(
          data = plot_prep$yside_labels,
          ggplot2$aes(x = Inf, y = y, label = label, vjust = vjust),
          inherit.aes = FALSE,
          hjust = 1.1,
          color = text_color
        )
      } else {
        NULL
      }

      plot_prep$plot_draws |>
        ggplot2$ggplot(ggplot2$aes(pred_result, pred_total)) +
        ggplot2$stat_bin_hex(binwidth = hex_binwidth) +
        ggplot2$scale_fill_viridis_c(
          breaks = breaks_pretty(6),
          labels = label_number(),
          name = "Count"
        ) +
        spread_layer +
        total_layer +
        quad_label_layer +
        new_scale_fill() +
        ggside$geom_xsidehistogram(
          ggplot2$aes(y = after_stat(count), fill = result_bin),
          binwidth = 1,
          boundary = spread_line,
          alpha = 0.7
        ) +
        ggplot2$scale_fill_manual(values = palettes$result_fill_values) +
        new_scale_fill() +
        ggside$geom_ysidehistogram(
          ggplot2$aes(x = after_stat(count), fill = total_bin),
          binwidth = 1,
          boundary = total_line,
          alpha = 0.7
        ) +
        ggplot2$scale_fill_manual(values = palettes$total_fill_values) +
        xside_label_layer +
        yside_label_layer +
        ggplot2$labs(x = "Result", y = "Total") +
        ggplot2$theme_minimal() +
        ggside$theme_ggside_void() +
        ggplot2$theme(
          plot.background = ggplot2$element_rect(fill = plot_bg, color = NA),
          panel.background = ggplot2$element_rect(fill = plot_bg, color = NA),
          text = ggplot2$element_text(color = text_color),
          strip.text = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1)
          ),
          plot.title = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1)
          ),
          axis.title = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1),
            face = "bold"
          ),
          axis.text = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1)
          ),
          aspect.ratio = 1,
          panel.grid = ggplot2$element_line(color = grid_color),
          ggside.axis.ticks.length = unit(0, "points"),
          ggside.axis.minor.ticks.length = unit(0, "pt"),
          ggside.panel.scale = 0.25,
          ggside.axis.line = ggplot2$element_blank(),
          ggside.axis.text = ggplot2$element_blank(),
          legend.position = "none"
        )
    }
  )
}

build_score_prob_plot <- function(
  plot_prep,
  kind,
  palettes,
  spread_line,
  total_line,
  show_spread_line = TRUE,
  show_total_line = TRUE,
  show_prob_labels = TRUE
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

      spread_layer <- if (show_spread_line) {
        ggplot2$geom_abline(
          intercept = -spread_line,
          slope = 1,
          linetype = 2,
          color = "red"
        )
      } else {
        NULL
      }
      total_layer <- if (show_total_line) {
        ggplot2$geom_abline(
          intercept = total_line,
          slope = -1,
          linetype = 2,
          color = "red"
        )
      } else {
        NULL
      }
      region_label_layer <- if (show_prob_labels) {
        ggplot2$geom_label(
          data = plot_prep$region_labels,
          ggplot2$aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          fill = label_fill,
          color = text_color,
          alpha = 0.85,
          angle = c(-90, 0, 0, 90),
          vjust = c("outward", "inward", "inward", "outward"),
          linewidth = 0
        )
      } else {
        NULL
      }

      plot_prep$plot_draws |>
        ggplot2$ggplot(ggplot2$aes(pred_home, pred_away)) +
        ggplot2$stat_bin_hex(binwidth = hex_binwidth) +
        ggplot2$scale_fill_viridis_c(
          breaks = breaks_pretty(6),
          labels = label_number(),
          name = "Count"
        ) +
        spread_layer +
        total_layer +
        region_label_layer +
        ggside$geom_xsidehistogram(
          ggplot2$aes(y = after_stat(count)),
          binwidth = 1,
          alpha = 0.7,
          fill = home_fill
        ) +
        ggside$geom_ysidehistogram(
          ggplot2$aes(x = after_stat(count)),
          binwidth = 1,
          alpha = 0.7,
          fill = away_fill
        ) +
        ggplot2$labs(x = "Home points", y = "Away points") +
        ggplot2$coord_equal(
          xlim = plot_prep$score_limits,
          ylim = plot_prep$score_limits
        ) +
        ggplot2$theme_minimal() +
        ggside$theme_ggside_void() +
        ggplot2$theme(
          plot.background = ggplot2$element_rect(fill = plot_bg, color = NA),
          panel.background = ggplot2$element_rect(fill = plot_bg, color = NA),
          text = ggplot2$element_text(color = text_color),
          strip.text = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1)
          ),
          plot.title = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1)
          ),
          axis.title = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1),
            face = "bold"
          ),
          axis.text = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1)
          ),
          panel.grid = ggplot2$element_line(color = grid_color),
          ggside.axis.ticks.length = unit(0, "points"),
          ggside.axis.minor.ticks.length = unit(0, "pt"),
          ggside.panel.scale = 0.25,
          ggside.axis.line = ggplot2$element_blank(),
          ggside.axis.text = ggplot2$element_blank(),
          legend.position = "none"
        )
    }
  )
}

build_score_combo_plot <- function(
  plot_prep,
  kind,
  palettes,
  spread_line,
  total_line,
  show_spread_line = TRUE,
  show_total_line = TRUE,
  show_prob_labels = TRUE
) {
  kind <- match.arg(kind, c("mu", "y"))
  hex_binwidth <- if (kind == "mu") c(1, 1) else c(2, 2)

  home_team <- plot_prep$home_team
  away_team <- plot_prep$away_team

  home_away_colors <- palettes$home_away_colors
  if (is.null(home_away_colors)) {
    home_away_colors <- list(home = "#35B779", away = "#482777")
  }

  home_fill <- home_away_colors[["home"]]
  away_fill <- home_away_colors[["away"]]
  if (is.null(home_fill) || is.na(home_fill)) {
    home_fill <- "#35B779"
  }
  if (is.null(away_fill) || is.na(away_fill)) {
    away_fill <- "#482777"
  }

  push_color <- palettes$result_fill_values[["Push"]]
  if (is.null(push_color) || is.na(push_color)) {
    push_color <- "grey70"
  }

  combo_levels <- c(
    "Home cover & Under",
    "Home cover & Over",
    "Push",
    "Away cover & Under",
    "Away cover & Over"
  )
  combo_colors <- c(
    "Home cover & Under" = home_fill,
    "Home cover & Over" = home_fill,
    "Push" = push_color,
    "Away cover & Under" = away_fill,
    "Away cover & Over" = away_fill
  )

  total_colors <- c(
    "Under" = palettes$total_fill_values[["Under"]],
    "Over" = palettes$total_fill_values[["Over"]],
    "Push" = push_color
  )
  if (is.null(total_colors[["Over"]]) || is.na(total_colors[["Over"]])) {
    total_colors[["Over"]] <- "#3B6FB6"
  }
  if (is.null(total_colors[["Under"]]) || is.na(total_colors[["Under"]])) {
    total_colors[["Under"]] <- "#C85B5B"
  }

  plot_data <- plot_prep$plot_draws
  cover_bin <- ifelse(
    plot_data$result_bin == home_team,
    "Home cover",
    ifelse(plot_data$result_bin == away_team, "Away cover", "Push")
  )
  total_bin <- ifelse(
    plot_data$total_bin %in% c("Over", "Under"),
    plot_data$total_bin,
    "Push"
  )
  push_mask <- plot_data$result_bin == "Push" | plot_data$total_bin == "Push"
  cover_bin[push_mask] <- "Push"
  total_bin[push_mask] <- "Push"

  combo_label <- ifelse(
    push_mask,
    "Push",
    paste(cover_bin, total_bin, sep = " & ")
  )
  plot_data$combo_label <- factor(combo_label, levels = combo_levels)
  plot_data$combo_home_order <- factor(
    combo_label,
    levels = c(
      "Home cover & Under",
      "Home cover & Over",
      "Push",
      "Away cover & Under",
      "Away cover & Over"
    )
  )
  plot_data$combo_away_order <- factor(
    combo_label,
    levels = c(
      "Away cover & Under",
      "Away cover & Over",
      "Push",
      "Home cover & Under",
      "Home cover & Over"
    )
  )
  plot_data$total_bin <- factor(total_bin, levels = c("Under", "Over", "Push"))
  legend_outline_colors <- c(
    "Home cover & Under" = total_colors[["Under"]],
    "Home cover & Over" = total_colors[["Over"]],
    "Push" = total_colors[["Push"]],
    "Away cover & Under" = total_colors[["Under"]],
    "Away cover & Over" = total_colors[["Over"]]
  )

  thematic_with_theme(
    thematic_theme(fg = "auto", bg = "auto", accent = "auto"),
    {
      text_color <- thematic_get_option("fg")
      plot_bg <- thematic_get_option("bg")
      label_fill <- plot_bg
      grid_color <- thematic_get_mixture(0.2, default = text_color)

      spread_layer <- if (show_spread_line) {
        ggplot2$geom_abline(
          intercept = -spread_line,
          slope = 1,
          linetype = 2,
          color = "red"
        )
      } else {
        NULL
      }
      total_layer <- if (show_total_line) {
        ggplot2$geom_abline(
          intercept = total_line,
          slope = -1,
          linetype = 2,
          color = "red"
        )
      } else {
        NULL
      }
      region_label_layer <- if (show_prob_labels) {
        ggplot2$geom_label(
          data = plot_prep$region_labels,
          ggplot2$aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          fill = label_fill,
          color = text_color,
          alpha = 0.85,
          angle = c(-90, 0, 0, 90),
          vjust = c("outward", "inward", "inward", "outward"),
          linewidth = 0
        )
      } else {
        NULL
      }

      ggplot2$ggplot(plot_data, ggplot2$aes(pred_home, pred_away)) +
        ggplot2$stat_bin_hex(
          ggplot2$aes(
            fill = combo_label,
            color = total_bin,
            alpha = after_stat(count)
          ),
          binwidth = hex_binwidth
        ) +
        ggplot2$scale_fill_manual(
          values = combo_colors,
          breaks = combo_levels,
          drop = FALSE,
          na.value = "transparent"
        ) +
        ggplot2$scale_color_manual(
          values = total_colors,
          breaks = c("Under", "Over", "Push"),
          guide = "none",
          drop = FALSE,
          na.value = "transparent"
        ) +
        ggplot2$scale_alpha_continuous(range = c(0.15, 0.9), guide = "none") +
        ggplot2$guides(
          fill = ggplot2$guide_legend(
            override.aes = list(
              color = unname(legend_outline_colors),
              alpha = 1,
              linewidth = 0.6
            )
          )
        ) +
        spread_layer +
        total_layer +
        region_label_layer +
        ggside$geom_xsidehistogram(
          ggplot2$aes(
            y = after_stat(count),
            fill = combo_home_order,
            color = total_bin,
            group = combo_home_order
          ),
          binwidth = 1,
          position = "stack",
          alpha = 0.85,
          linewidth = 0.5
        ) +
        ggside$geom_ysidehistogram(
          ggplot2$aes(
            x = after_stat(count),
            fill = combo_away_order,
            color = total_bin,
            group = combo_away_order
          ),
          binwidth = 1,
          position = "stack",
          alpha = 0.85,
          linewidth = 0.5
        ) +
        ggplot2$labs(
          x = "Home points",
          y = "Away points",
          fill = "Cover/Total"
        ) +
        ggplot2$coord_equal(
          xlim = plot_prep$score_limits,
          ylim = plot_prep$score_limits
        ) +
        ggplot2$theme_minimal() +
        ggside$theme_ggside_void() +
        ggplot2$theme(
          plot.background = ggplot2$element_rect(fill = plot_bg, color = NA),
          panel.background = ggplot2$element_rect(fill = plot_bg, color = NA),
          text = ggplot2$element_text(color = text_color),
          strip.text = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1)
          ),
          plot.title = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1)
          ),
          axis.title = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1),
            face = "bold"
          ),
          axis.text = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1)
          ),
          aspect.ratio = 1,
          panel.grid = ggplot2$element_line(color = grid_color),
          ggside.axis.ticks.length = unit(0, "points"),
          ggside.axis.minor.ticks.length = unit(0, "pt"),
          ggside.panel.scale = 0.25,
          ggside.axis.line = ggplot2$element_blank(),
          ggside.axis.text = ggplot2$element_blank(),
          legend.position = c(0.98, 0.98),
          legend.justification = c(1, 1),
          legend.background = ggplot2$element_rect(fill = plot_bg, color = NA)
        )
    }
  )
}

build_joint_prob_plot_int <- function(
  plot_prep,
  kind,
  palettes,
  spread_line,
  total_line,
  show_spread_line = TRUE,
  show_total_line = TRUE,
  show_prob_labels = TRUE
) {
  kind <- match.arg(kind, c("mu", "y"))
  hex_binwidth <- if (kind == "mu") c(1, 1) else c(2, 2)

  thematic_with_theme(
    thematic_theme(fg = "auto", bg = "auto", accent = "auto"),
    {
      text_color <- thematic_get_option("fg")
      plot_bg <- thematic_get_option("bg")
      grid_color <- thematic_get_mixture(0.2, default = text_color)

      spread_layer <- if (show_spread_line) {
        ggplot2$geom_vline(
          xintercept = spread_line,
          linetype = 2,
          color = "red"
        )
      } else {
        NULL
      }
      total_layer <- if (show_total_line) {
        ggplot2$geom_hline(
          yintercept = total_line,
          linetype = 2,
          color = "red"
        )
      } else {
        NULL
      }
      quad_label_layer <- if (show_prob_labels) {
        ggplot2$geom_text(
          data = plot_prep$quad_labels,
          ggplot2$aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust)
        )
      } else {
        NULL
      }

      p1 <- plot_prep$plot_draws |>
        ggplot2$ggplot(ggplot2$aes(pred_result, pred_total)) +
        ggplot2$stat_bin_hex(binwidth = hex_binwidth) +
        ggplot2$scale_fill_viridis_c(
          breaks = breaks_pretty(6),
          labels = label_number(),
          name = "Count"
        ) +
        spread_layer +
        total_layer +
        quad_label_layer +
        ggplot2$labs(x = "Result", y = "Total") +
        ggplot2$theme_minimal() +
        ggplot2$theme(
          plot.background = ggplot2$element_rect(fill = plot_bg, color = NA),
          panel.background = ggplot2$element_rect(fill = plot_bg, color = NA),
          text = ggplot2$element_text(color = text_color),
          strip.text = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1)
          ),
          plot.title = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1)
          ),
          axis.title = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1),
            face = "bold"
          ),
          ggside.axis.ticks.length = unit(0, "points"),
          ggside.axis.minor.ticks.length = unit(0, "pt"),
          axis.text = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1)
          ),
          panel.grid = ggplot2$element_line(color = grid_color),
          legend.position = "none"
        )
      pl1 <- ggplotly(p1, type = "scatter")

      xside_label_layer <- if (show_prob_labels) {
        ggplot2$geom_text(
          data = plot_prep$xside_labels,
          ggplot2$aes(x = x, y = Inf, label = label, hjust = hjust),
          vjust = 1.1
        )
      } else {
        NULL
      }
      p2 <- plot_prep$plot_draws |>
        ggplot2$ggplot(ggplot2$aes(pred_result, pred_total)) +
        ggplot2$geom_histogram(
          ggplot2$aes(
            y = after_stat(count) / nrow(plot_prep$plot_draws),
            fill = result_bin
          ),
          binwidth = 1,
          boundary = spread_line,
          alpha = 0.7
        ) +
        ggplot2$scale_fill_manual(values = palettes$result_fill_values) +
        xside_label_layer +
        ggplot2$labs(x = "Result", y = "Total") +
        ggplot2$theme_minimal() +
        ggplot2$theme(
          plot.background = ggplot2$element_rect(fill = plot_bg, color = NA),
          panel.background = ggplot2$element_rect(fill = plot_bg, color = NA),
          text = ggplot2$element_text(color = text_color),
          strip.text = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1)
          ),
          plot.title = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1)
          ),
          axis.title = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1),
            face = "bold"
          ),
          axis.text = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1)
          ),
          panel.grid.major.x = ggplot2$element_line(color = grid_color),
          panel.grid.major.y = ggplot2$element_blank(),
          panel.grid.minor.y = ggplot2$element_blank(),
          axis.ticks.length = unit(0, "points"),
          axis.minor.ticks.length = unit(0, "pt"),
          axis.line = ggplot2$element_blank(),
          axis.text.y = ggplot2$element_blank(),
          axis.title.y = ggplot2$element_blank(),
          legend.position = "none"
        )
      pl2 <- ggplotly(p2)

      yside_label_layer <- if (show_prob_labels) {
        ggplot2$geom_text(
          data = plot_prep$yside_labels,
          ggplot2$aes(x = Inf, y = y, label = label, vjust = vjust),
          hjust = 1.1
        )
      } else {
        NULL
      }
      p3 <- plot_prep$plot_draws |>
        ggplot2$ggplot(ggplot2$aes(pred_result, pred_total)) +
        ggplot2$geom_histogram(
          ggplot2$aes(
            x = after_stat(count) / nrow(plot_prep$plot_draws),
            fill = total_bin
          ),
          binwidth = 1,
          boundary = total_line,
          alpha = 0.7
        ) +
        ggplot2$scale_fill_manual(values = palettes$total_fill_values) +
        yside_label_layer +
        ggplot2$labs(x = "Result", y = "Total") +
        ggplot2$theme_minimal() +
        ggplot2$theme(
          plot.background = ggplot2$element_rect(fill = plot_bg, color = NA),
          panel.background = ggplot2$element_rect(fill = plot_bg, color = NA),
          text = ggplot2$element_text(color = text_color),
          strip.text = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1)
          ),
          plot.title = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1)
          ),
          axis.title = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1.1),
            face = "bold"
          ),
          axis.text = ggplot2$element_text(
            color = text_color,
            size = ggplot2$rel(1)
          ),
          panel.grid.major.y = ggplot2$element_line(color = grid_color),
          panel.grid.major.x = ggplot2$element_blank(),
          panel.grid.minor.x = ggplot2$element_blank(),
          axis.ticks.length = unit(0, "points"),
          axis.minor.ticks.length = unit(0, "pt"),
          axis.line = ggplot2$element_blank(),
          axis.text.x = ggplot2$element_blank(),
          axis.title.x = ggplot2$element_blank(),
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

export(
  build_joint_prob_plot,
  build_joint_prob_plot_int,
  build_score_combo_plot,
  build_score_prob_plot
)
