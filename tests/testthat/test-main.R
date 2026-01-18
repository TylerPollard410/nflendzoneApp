box::use(
  shinytest2[AppDriver],
  testthat[test_that],
)

test_that("app loads", {
  app <- tryCatch(
    AppDriver$new(),
    error = function(error) {
      testthat::skip(paste("shinytest2 not available:", error$message))
    }
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle()

  app$expect_text("NFL EndZone Anaytics")
  app$expect_text("Standings")
  app$expect_text("Predictions")
})
