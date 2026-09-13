library(shiny)
library(bslib)
library(brand.yml)
library(yaml)
library(sass)
library(quarto)

# get brands
# bs_current_theme()
# bs_get_variables()
# bs_global_theme()
# bs_remove()

# bs_theme_preview(theme = bs_theme(version = 5, brand = FALSE))

# 1. Read the brand file to get the correct color value dynamically
my_brand <- read_brand_yml("_brand.yml")
my_brand <- read_yaml("_brand.yml")
brand_pluck(my_brand, "color", "tertiary")
brand_sass_defaults_bootstrap(my_brand)
brand_sass_fonts(my_brand)

get_brand_sem <- function(brand, sem) {
  brand_pluck(brand, "color", "palette", sem)
}

# create using bslib replciaitng brand
bslib_theme_light <- bs_theme(
  version = 5,
  preset = "shiny",
  brand = FALSE,
  bg = get_brand_sem(
    my_brand,
    brand_pluck(my_brand, "color", "background", "light")
  ),
  fg = get_brand_sem(
    my_brand,
    brand_pluck(my_brand, "color", "foreground", "light")
  ),
  primary = get_brand_sem(
    my_brand,
    brand_pluck(my_brand, "color", "primary")
  ),
  secondary = get_brand_sem(
    my_brand,
    brand_pluck(my_brand, "color", "secondary")
  ),
  success = get_brand_sem(my_brand, brand_pluck(my_brand, "color", "success")),
  info = get_brand_sem(my_brand, brand_pluck(my_brand, "color", "info")),
  warning = get_brand_sem(my_brand, brand_pluck(my_brand, "color", "warning")),
  danger = get_brand_sem(my_brand, brand_pluck(my_brand, "color", "danger")),
  #base_font = font_google("Space Grotesk", wght = 400, ital = 0),
  base_font = font_google(
    as_sass(brand_pluck(my_brand, "typography", "base")),
    local = FALSE
  ),
  code_font = font_google(
    as_sass(brand_pluck(my_brand, "typography", "monospace")),
    local = FALSE
  ),
  heading_font = font_google(
    as_sass(
      list(
        family = "Space Grotesk",
        weight = 600,
        "line-height" = 1.2,
        color = "#FF5BC8"
        #brand_pluck(my_brand, "typography", "headings"),
        #list(color = get_brand_sem(my_brand, "tp-pink"))
      )
    ),
    local = FALSE
  )
  #font_scale = NULL,
  #bootswatch = NULL
)
bs_theme_preview(bslib_theme_light)


bs_light_theme_base <- bs_theme(
  version = 5,
  preset = "bootstrap",
  brand = FALSE,
  bg = "#f8f8f8",
  fg = "#202020",
  primary = "#6B2EFF",
  secondary = "#03c7e8",
  success = "#6fff00ff",
  info = "#FFC857",
  warning = "#FF7A1A",
  danger = "#FF5BC8",
  #base_font = font_google("Space Grotesk", wght = 400, ital = 0),
  base_font = font_google(
    as_sass(
      list(
        family = "Space Grotesk",
        weight = 400,
        size = "1rem",
        "line-height" = 1.5
      )
    ),
    local = FALSE
  ),
  code_font = font_google(
    as_sass(
      list(
        family = "JetBrains Mono",
        weight = 400,
        size = "0.9rem"
      )
    ),
    local = FALSE
  ),
  heading_font = font_google(
    as_sass(
      list(
        family = "Space Grotesk",
        weight = 600,
        "line-height" = 1.2,
        color = "#FF5BC8"
      )
    ),
    local = FALSE
  )
  #font_scale = NULL,
  #bootswatch = NULL
)
bs_light_theme <- bs_light_theme_base |>
  bs_theme_update(
    `enable-gradients` = TRUE,
    `enable-shadows` = TRUE
  ) |>
  bs_add_rules(
    sass_file("styles.scss")
  )
bs_theme_preview(bs_light_theme)


bs_dark_theme_base <- bs_light_theme_base |>
  bs_theme_update(
    bg = "#202020",
    fg = "#f8f8f8"
  )
bs_dark_theme <- bs_dark_theme_base |>
  bs_theme_update(
    `enable-gradients` = TRUE,
    `enable-shadows` = TRUE
  ) |>
  bs_add_rules(
    sass_file("styles.scss")
  )
bs_theme_preview(bs_dark_theme_base)


# NEW LIGHT / DARK EXT ----
bs_light_ext <- read_brand_yml(
  "_extensions/TylerPollard410/brand/light-brand.yml"
)

bs_dark_ext <- read_brand_yml(
  "_extensions/TylerPollard410/brand/dark-brand.yml"
)

## Light bslib ----
bs_light_theme_ext <- bs_theme(
  version = 5,
  preset = "shiny",
  brand = "_extensions/TylerPollard410/brand/light-brand.yml"
) |>
  bs_theme_update(
    `enable-gradients` = TRUE,
    `enable-shadows` = TRUE
  )
bs_theme_preview(bs_light_theme_ext)

## Dark bslib ----
bs_dark_theme_ext <- bs_theme(
  version = 5,
  preset = "bootstrap",
  brand = "_extensions/TylerPollard410/brand/dark-brand.yml"
) |>
  bs_theme_update(
    `enable-gradients` = TRUE,
    `enable-shadows` = TRUE
  )
bs_theme_preview(bs_light_theme_ext)
