library(shiny)
library(bslib)
library(brand.yml)
library(htmltools)
library(sass)

build_brand_theme <- function(brand) {
  bs_theme(
    version = 5,
    preset = "bootstrap",
    brand = brand,
    "enable-gradients" = TRUE,
    "enable-shadows" = TRUE
  ) |>
    bs_add_variables(
      "navbar-bg" = "$primary",
      .where = "declarations"
    )
}

compile_theme_css <- function(theme, output = NULL) {
  sass_input <- as_sass(theme)

  css_text <- if (is.null(output)) {
    paste(sass(sass_input), collapse = "\n")
  } else {
    sass(sass_input, output = output)
    paste(readLines(output, warn = FALSE), collapse = "\n")
  }

  list(
    sass = sass_input,
    css = css_text,
    style_tag = htmltools::tags$style(htmltools::HTML(css_text))
  )
}

extract_css_block <- function(css_text, pattern) {
  match <- regexpr(pattern, css_text, perl = TRUE)

  if (match[[1]] < 0) {
    stop("Unable to find CSS block matching pattern: ", pattern, call. = FALSE)
  }

  regmatches(css_text, match)
}

parse_css_vars <- function(css_block) {
  block_body <- sub("^[^{]+\\{", "", css_block)
  block_body <- sub("\\}$", "", block_body)

  declarations <- strsplit(block_body, ";", fixed = TRUE)[[1]]
  declarations <- trimws(declarations)
  declarations <- declarations[nzchar(declarations)]
  declarations <- declarations[startsWith(declarations, "--bs-")]

  if (!length(declarations)) {
    return(setNames(character(), character()))
  }

  split_declarations <- strsplit(declarations, ":", fixed = TRUE)
  values <- vapply(
    split_declarations,
    function(x) trimws(paste(x[-1], collapse = ":")),
    character(1)
  )

  names(values) <- sub(
    "^--bs-",
    "",
    vapply(split_declarations, function(x) x[[1]], character(1))
  )

  values
}

extract_theme_artifacts <- function(theme, output = NULL) {
  compiled <- compile_theme_css(theme, output = output)

  light_block <- extract_css_block(
    compiled$css,
    '(?s):root\\s*,\\s*\\[data-bs-theme="light"\\]\\s*\\{.*?\\}'
  )
  dark_block <- extract_css_block(
    compiled$css,
    '(?s)\\[data-bs-theme="dark"\\]\\s*\\{.*?\\}'
  )

  list(
    sass = compiled$sass,
    css = compiled$css,
    style_tag = compiled$style_tag,
    light_block = light_block,
    dark_block = dark_block,
    light_vars = parse_css_vars(light_block),
    dark_vars = parse_css_vars(dark_block)
  )
}

compare_theme_vars <- function(light_artifacts, dark_artifacts, vars) {
  data.frame(
    variable = vars,
    light_theme_light_mode = unname(light_artifacts$light_vars[vars]),
    light_theme_dark_mode = unname(light_artifacts$dark_vars[vars]),
    dark_theme_light_mode = unname(dark_artifacts$light_vars[vars]),
    dark_theme_dark_mode = unname(dark_artifacts$dark_vars[vars]),
    row.names = NULL,
    check.names = FALSE
  )
}

output_css_dir <- NULL

# Unified ----
theme_brand_uni <- tryCatch({
  brand_uni_yml <- read_brand_yml("brand/brand.yml")
  build_brand_theme(brand_uni_yml)
}, error = function(e) {
  message("Skipping unified brand.yml: ", conditionMessage(e))
  NULL
})

# Light theme ----
brand_light_yml <- read_brand_yml("brand/brand-light.yml")
theme_brand_light <- build_brand_theme(brand_light_yml)

# Dark theme ----
brand_dark_yml <- read_brand_yml("brand/brand-dark.yml")
theme_brand_dark <- build_brand_theme(brand_dark_yml)

# Examine colors ----
light_artifacts <- extract_theme_artifacts(
  theme_brand_light,
  output = if (is.null(output_css_dir)) NULL else {
    file.path(output_css_dir, "brand-light.css")
  }
)
dark_artifacts <- extract_theme_artifacts(
  theme_brand_dark,
  output = if (is.null(output_css_dir)) NULL else {
    file.path(output_css_dir, "brand-dark.css")
  }
)

key_vars <- c(
  "body-bg",
  "body-color",
  "emphasis-color",
  "secondary-bg",
  "tertiary-bg",
  "border-color",
  "navbar-bg",
  "link-color",
  "link-hover-color",
  "table-bg",
  "table-color",
  "table-striped-bg",
  "table-hover-bg"
)

theme_level_vars <- list(
  light = bs_get_variables(theme_brand_light, key_vars),
  dark = bs_get_variables(theme_brand_dark, key_vars)
)

theme_var_comparison <- compare_theme_vars(
  light_artifacts,
  dark_artifacts,
  key_vars
)

print(theme_level_vars)
print(theme_var_comparison)

if (interactive()) {
  bs_theme_preview(
    theme_brand_light,
    with_themer = TRUE
  )
  bs_global_clear()
  bs_theme_preview(
    theme_brand_dark,
    with_themer = TRUE
  )
}
