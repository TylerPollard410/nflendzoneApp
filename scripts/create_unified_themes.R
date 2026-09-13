library(bslib)
library(yaml)
library(htmltools)
library(sass)

box::use(
  app / logic / themes[load_brand_variants]
)

KEY_VARS <- c(
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

compare_theme_vars <- function(
  light_artifacts,
  dark_artifacts,
  vars = KEY_VARS
) {
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

strip_brand_metadata <- function(brand) {
  if (is.null(names(brand))) {
    return(brand)
  }

  brand[names(brand) != "path"]
}

write_extracted_brands <- function(
  brand_split,
  output_dir = "brand",
  prefix = "brand-unified"
) {
  light_path <- file.path(output_dir, paste0(prefix, "-light.yml"))
  dark_path <- file.path(output_dir, paste0(prefix, "-dark.yml"))

  yaml::write_yaml(strip_brand_metadata(brand_split$light), light_path)
  yaml::write_yaml(strip_brand_metadata(brand_split$dark), dark_path)

  list(
    light = light_path,
    dark = dark_path
  )
}

run_unified_theme_workflow <- function(
  brand_path = "brand/brand.yml",
  output_css_dir = NULL,
  write_brand_files = FALSE,
  brand_output_dir = "brand"
) {
  brand_variants <- load_brand_variants(brand_path)

  if (!isTRUE(brand_variants$is_unified)) {
    stop("`brand_path` must point to a unified brand.yml file.", call. = FALSE)
  }

  brand_unified_yml <- brand_variants$unified
  brand_unified_split <- list(
    light = brand_variants$light_raw,
    dark = brand_variants$dark_raw
  )
  brand_extracted_light_yml <- brand_variants$light
  brand_extracted_dark_yml <- brand_variants$dark

  theme_extracted_light <- build_brand_theme(brand_extracted_light_yml)
  theme_extracted_dark <- build_brand_theme(brand_extracted_dark_yml)

  light_artifacts <- extract_theme_artifacts(
    theme_extracted_light,
    output = if (is.null(output_css_dir)) {
      NULL
    } else {
      file.path(output_css_dir, "brand-unified-light.css")
    }
  )
  dark_artifacts <- extract_theme_artifacts(
    theme_extracted_dark,
    output = if (is.null(output_css_dir)) {
      NULL
    } else {
      file.path(output_css_dir, "brand-unified-dark.css")
    }
  )

  theme_level_vars <- list(
    light = bs_get_variables(theme_extracted_light, KEY_VARS),
    dark = bs_get_variables(theme_extracted_dark, KEY_VARS)
  )

  extracted_brand_paths <- if (isTRUE(write_brand_files)) {
    write_extracted_brands(
      brand_unified_split,
      output_dir = brand_output_dir
    )
  } else {
    NULL
  }

  list(
    brand_unified_yml = brand_unified_yml,
    brand_unified_split = brand_unified_split,
    brand_extracted_light_yml = brand_extracted_light_yml,
    brand_extracted_dark_yml = brand_extracted_dark_yml,
    theme_extracted_light = theme_extracted_light,
    theme_extracted_dark = theme_extracted_dark,
    light_artifacts = light_artifacts,
    dark_artifacts = dark_artifacts,
    theme_level_vars = theme_level_vars,
    extracted_brand_paths = extracted_brand_paths,
    theme_var_comparison = compare_theme_vars(
      light_artifacts,
      dark_artifacts,
      KEY_VARS
    )
  )
}

brand_unified_path <- "brand/brand.yml"
workflow <- run_unified_theme_workflow(
  brand_unified_path,
  write_brand_files = TRUE
)

brand_unified_yml <- workflow$brand_unified_yml
brand_unified_split <- workflow$brand_unified_split
brand_extracted_light_yml <- workflow$brand_extracted_light_yml
brand_extracted_dark_yml <- workflow$brand_extracted_dark_yml
theme_extracted_light <- workflow$theme_extracted_light
theme_extracted_dark <- workflow$theme_extracted_dark
light_artifacts <- workflow$light_artifacts
dark_artifacts <- workflow$dark_artifacts
theme_level_vars <- workflow$theme_level_vars
extracted_brand_paths <- workflow$extracted_brand_paths
theme_var_comparison <- workflow$theme_var_comparison

print(theme_level_vars)
print(theme_var_comparison)
print(extracted_brand_paths)

# Optional output:
# workflow <- run_unified_theme_workflow(
#   brand_unified_path,
#   output_css_dir = "scripts",
#   write_brand_files = TRUE
# )
