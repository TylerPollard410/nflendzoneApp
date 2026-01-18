box::use(
  box[export],
  shiny,
)

#' Convert `bslib::input_dark_mode()` values to a simple `TRUE/FALSE`.
is_dark_mode_reactive <- function(dark_mode) {
  shiny$reactive({
    if (is.null(dark_mode)) {
      return(FALSE)
    }
    value <- dark_mode()
    if (is.null(value) || length(value) == 0L) {
      return(FALSE)
    }
    if (is.logical(value)) {
      return(isTRUE(value[[1]]))
    }
    if (is.character(value)) {
      return(tolower(value[[1]]) %in% c("dark", "1", "true", "yes", "on"))
    }
    FALSE
  })
}

export(is_dark_mode_reactive)
