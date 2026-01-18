box::use(
  box[export],
)

#' Check whether two values represent the same number.
#'
#' Handles `NULL`, `NA`, and numeric-ish inputs (characters are coerced).
same_number <- function(x, y) {
  if (is.null(x) || is.null(y)) {
    return(FALSE)
  }
  if (length(x) == 0L || length(y) == 0L) {
    return(FALSE)
  }

  x <- x[[1]]
  y <- y[[1]]

  if (is.na(x) || is.na(y)) {
    return(is.na(x) && is.na(y))
  }

  isTRUE(all.equal(as.numeric(x), as.numeric(y)))
}

export(same_number)
