box::use(
  box[export],
)

#' Convert American odds to implied probability.
#'
#' @param odds Numeric vector of American odds.
#' @return Numeric vector in [0, 1].
american_odds_to_prob <- function(odds) {
  odds <- as.numeric(odds)

  out <- rep(NA_real_, length(odds))
  out[odds == -Inf] <- 1
  out[odds == Inf] <- 0

  ok <- is.finite(odds) & !is.na(odds)
  fav <- ok & odds < 0
  dog <- ok & odds > 0
  even <- ok & odds == 0

  out[fav] <- abs(odds[fav]) / (abs(odds[fav]) + 100)
  out[dog] <- 100 / (odds[dog] + 100)
  out[even] <- 0.5

  out
}

#' Convert probability to American odds (implied / fair odds).
#'
#' @param p Numeric vector of probabilities in [0, 1].
#' @param digits Integer; rounding for returned odds.
#' @return Integer vector (American odds); `NA` for invalid inputs.
prob_to_american_odds <- function(p, digits = 0L) {
  p <- as.numeric(p)

  out <- rep(NA_real_, length(p))
  ok <- is.finite(p) & !is.na(p) & p >= 0 & p <= 1

  out[ok & p == 1] <- -Inf
  out[ok & p == 0] <- Inf

  fav <- ok & p > 0 & p < 1 & p > 0.5
  dog <- ok & p > 0 & p < 1 & p < 0.5
  even <- ok & p == 0.5

  out[fav] <- -100 * (p[fav] / (1 - p[fav]))
  out[dog] <- 100 * ((1 - p[dog]) / p[dog])
  out[even] <- 100

  as.integer(round(out, digits = digits))
}

export(american_odds_to_prob, prob_to_american_odds)
