box::use(
  brand.yml[as_brand_yml, read_brand_yml],
  purrr[map, modify, set_names],
  yaml[read_yaml],
)

.mode_keys <- c("light", "dark")

# Unified brand files currently load through `yaml::read_yaml()` as a single
# top-level list item, so normalize that shape before inspecting the contents.
.unwrap_brand_root <- function(x) {
  if (
    is.list(x) &&
      is.null(names(x)) &&
      length(x) == 1 &&
      is.list(x[[1]]) &&
      !is.null(names(x[[1]]))
  ) {
    return(x[[1]])
  }

  x
}

.is_mode_branch <- function(x, mode_keys = .mode_keys) {
  is.list(x) &&
    length(x) > 0 &&
    !is.null(names(x)) &&
    all(nzchar(names(x))) &&
    all(names(x) %in% mode_keys)
}

.extract_brand_mode <- function(x, mode = c("light", "dark")) {
  mode <- match.arg(mode)
  x <- .unwrap_brand_root(x)

  if (!is.list(x)) {
    return(x)
  }

  if (.is_mode_branch(x)) {
    keep <- x[[mode]]

    # If only one mode is present, keep the available value rather than
    # preserving a dangling light/dark wrapper.
    if (is.null(keep)) {
      keep <- x[[setdiff(names(x), mode)[[1]]]]
    }

    return(.extract_brand_mode(keep, mode))
  }

  modify(x, .extract_brand_mode, mode = mode)
}

.split_brand_modes <- function(brand) {
  set_names(.mode_keys) |>
    map(~ .extract_brand_mode(brand, .x))
}

# Recursively scan the parsed YAML so standalone files can bypass the unified
# extraction path and unified files can be validated before conversion.
.find_mode_branches <- function(x, path = character()) {
  if (!is.list(x)) {
    return(character())
  }

  hits <- character()

  if (.is_mode_branch(x)) {
    hits <- paste(path, collapse = ".")
  }

  child_names <- names(x)
  if (is.null(child_names)) {
    child_names <- as.character(seq_along(x))
  }

  child_hits <- unlist(
    Map(
      f = function(name, value) {
        .find_mode_branches(value, c(path, name))
      },
      name = child_names,
      value = x
    ),
    use.names = FALSE
  )

  c(hits, child_hits)
}

#' Load brand objects from a unified or standalone brand YAML file.
#'
#' Unified brand files return the normalized raw YAML in `unified` plus
#' extracted `brand_yml` objects in `light` and `dark`. Standalone brand files
#' return the parsed `brand_yml` in `brand`, and mirror that object into
#' `light` and `dark` so downstream theme code can use a consistent interface.
#'
#' @param path Path to a unified or standalone brand YAML file.
#' @return A list with source path, source type, raw unified splits when
#'   available, and `brand_yml` objects for theme construction.
#' @export
load_brand_variants <- function(path) {
  source_path <- normalizePath(path, mustWork = TRUE)
  raw_brand <- .unwrap_brand_root(read_yaml(source_path))
  raw_brand$path <- source_path

  if (!length(.find_mode_branches(raw_brand))) {
    brand <- read_brand_yml(source_path)

    return(list(
      source_path = source_path,
      is_unified = FALSE,
      unified = NULL,
      brand = brand,
      light = brand,
      dark = brand,
      light_raw = NULL,
      dark_raw = NULL
    ))
  }

  split_brand <- .split_brand_modes(raw_brand)
  light_brand <- as_brand_yml(split_brand$light)
  dark_brand <- as_brand_yml(split_brand$dark)

  light_brand$path <- source_path
  dark_brand$path <- source_path

  stopifnot(length(.find_mode_branches(split_brand$light)) == 0)
  stopifnot(length(.find_mode_branches(split_brand$dark)) == 0)

  list(
    source_path = source_path,
    is_unified = TRUE,
    unified = raw_brand,
    brand = NULL,
    light = light_brand,
    dark = dark_brand,
    light_raw = split_brand$light,
    dark_raw = split_brand$dark
  )
}
