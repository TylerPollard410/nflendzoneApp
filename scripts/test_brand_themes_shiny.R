library(shiny)
library(bslib)
library(brand.yml)
library(yaml)
library(sass)

box::use(
  app / logic / themes[load_brand_variants]
)

box::use(
  app / view / themes / themes,
  #app / view / themes / light[theme_light, brand_light_yml],
)

theme <- bs_theme(
  version = 5,
  #preset = "bootstrap",
  #brand = TRUE,
  brand = "brand/brand-light.yml",
  "enable-gradients" = TRUE,
  "enable-shadows" = TRUE
) |>
  bs_add_variables(
    "navbar-bg" = "$primary",
    .where = "declarations"
  ) |>
  # Use sass() to allow variable references
  bs_add_rules(sass(list(
    # Define dark mode using existing Sass variables
    "[data-bs-theme='dark'] {
        /* Reference your palette-defined variables directly */
        --bs-body-bg: $brand-tp-dark-bg;          /* Still need hex for dark bg if not in light YAML */
        --bs-body-color: $brand-tp-dark-fg;        /* Uses $white from Bootstrap/Brand */
        --bs-primary: $primary;         /* Uses $primary from Brand */
        
        /* Reference specific brand colors if they were defined in your palette */
        --bs-heading-color: $brand-tp-pink;    /* tp-pink */
        
        pre, code {
          background-color: #212529;
          color: #FF7A1A;
        }
      }",

    # Reuse the max-height variable or logic
    ".navbar-brand img {
        max-height: 48px;
        width: auto;
      }"
  )))

bs_theme_preview(
  theme = theme
)
