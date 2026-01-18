box::use(
  box[export],
  reactable,
)

#' `reactableTheme()` powered by Bootstrap CSS variables.
bs_reactable_theme <- function() {
  reactable$reactableTheme(
    color = "var(--bs-emphasis-color, var(--bs-body-color))",
    backgroundColor = "var(--bs-body-bg, #ffffff)",
    borderColor = "var(--bs-border-color, #dee2e6)",
    stripedColor = "var(--bs-tertiary-bg, rgba(0, 0, 0, 0.03))",
    highlightColor = "var(--bs-secondary-bg, rgba(0, 0, 0, 0.05))",
    headerStyle = list(
      fontWeight = 600,
      color = "var(--bs-emphasis-color, var(--bs-body-color))",
      backgroundColor = "var(--bs-tertiary-bg, rgba(0, 0, 0, 0.03))"
    ),
    cellStyle = list(
      color = "var(--bs-emphasis-color, var(--bs-body-color))"
    )
  )
}

export(bs_reactable_theme)
