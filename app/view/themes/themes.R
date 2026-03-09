box::use(
  bslib,
)

box::use(
  app / logic / themes[load_brand_variants],
)

# Build a Bootstrap theme from a resolved `brand_yml` object and keep the
# navbar background tied to the brand primary color.
build_brand_theme <- function(brand) {
  bslib$bs_theme(
    version = 5,
    preset = "bootstrap",
    brand = brand,
    "enable-gradients" = TRUE,
    "enable-shadows" = TRUE
  ) |>
    bslib$bs_add_variables(
      "navbar-bg" = "$primary",
      .where = "declarations"
    )
}

# Load the unified brand once so theme creation and brand asset usage stay in
# sync across the app.
brand_variants <- load_brand_variants("brand/brand.yml")

#' @export
brand_unified_yml <- brand_variants$unified
#' @export
brand_light_yml <- brand_variants$light
#' @export
brand_dark_yml <- brand_variants$dark

#' @export
theme_brand_light <- build_brand_theme(brand_light_yml)
#' @export
theme_brand_dark <- build_brand_theme(brand_dark_yml)
