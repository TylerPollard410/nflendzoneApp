box::use(
  shiny[testServer],
  testthat[expect_lt, expect_match, expect_true, test_path, test_that],
  withr[local_dir],
)

local_dir(test_path("..", ".."))

box::use(
  app / main[server, ui],
)

test_that("main server works", {
  testServer(server, {
    #expect_true(grepl(x = output$message$html, pattern = "Check out Rhino docs!"))
    expect_true(TRUE)
  })
})

test_that("main ui keeps the dark mode toggle outside the collapsed nav", {
  html <- as.character(ui("app"))

  dark_mode_pos <- regexpr("bslib-input-dark-mode", html, fixed = TRUE)[[1]]
  collapse_pos <- regexpr(
    "<div class=\"navbar-collapse collapse\"",
    html,
    fixed = TRUE
  )[[1]]

  expect_match(html, "app-navbar-dark-toggle")
  expect_true(dark_mode_pos > 0)
  expect_true(collapse_pos > 0)
  expect_lt(dark_mode_pos, collapse_pos)
})
