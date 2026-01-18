box::use(
  box[export],
  cli[cli_warn],
  gh[gh_token],
  piggyback[pb_download_url],
  rlang[abort],
  stringr[regex, str_detect, str_replace_all],
  tools[file_ext_fn = file_ext, file_path_sans_ext],
)

pb_download_url_szn_wk <- function(
  tag,
  repo,
  seasons = TRUE,
  weeks = TRUE,
  asset_ext = NULL,
  .token = gh_token()
) {
  normalize_ext <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
      abort(
        "`asset_ext` must be NULL or a single string like '.rds' or 'rds'."
      )
    }
    x <- trimws(x)
    if (x == "") {
      abort("`asset_ext` must be NULL or a non-empty string.")
    }
    if (!startsWith(x, ".")) {
      x <- paste0(".", x)
    }
    tolower(x)
  }

  validate_sel <- function(x, arg_name) {
    if (is.null(x) || isTRUE(x)) {
      return(invisible(TRUE))
    }
    if (
      is.numeric(x) &&
        length(x) >= 1L &&
        all(is.finite(x)) &&
        all(x == as.integer(x))
    ) {
      return(invisible(TRUE))
    }
    abort(paste0(
      "`",
      arg_name,
      "` must be NULL, TRUE, an integer, or an integer vector."
    ))
  }

  validate_sel(seasons, "seasons")
  validate_sel(weeks, "weeks")
  asset_ext <- normalize_ext(asset_ext)

  all_urls <- pb_download_url(
    file = NULL,
    repo = repo,
    tag = tag,
    .token = .token
  )

  all_files <- basename(all_urls)

  if (!is.null(asset_ext)) {
    keep_ext <- tolower(paste0(".", file_ext_fn(all_files))) == asset_ext
    all_urls <- all_urls[keep_ext]
    all_files <- all_files[keep_ext]
  }

  stem <- file_path_sans_ext(all_files)

  prefix_rx <- paste0(
    "^(?:",
    str_replace_all(tag, "([\\W])", "\\\\\\1"),
    "/)?",
    str_replace_all(tag, "([\\W])", "\\\\\\1")
  )

  select_urls <- function(idx, what) {
    out <- all_urls[idx]
    if (length(out) == 0L) {
      cli_warn(
        "No matching assets found for {.val {what}} in release {.val {tag}} ({.val {repo}})."
      )
    }
    out
  }

  if (is.null(seasons)) {
    idx <- str_detect(stem, regex(paste0(prefix_rx, "$")))
    return(select_urls(idx, "all-seasons aggregate (tag.ext or tag/tag.ext)"))
  }

  season_vals <- if (isTRUE(seasons)) NULL else as.integer(seasons)

  weeks_mode <- if (isTRUE(weeks) || is.null(weeks)) "both" else "week_only"
  week_vals <- if (weeks_mode == "week_only") as.integer(weeks) else NULL

  if (weeks_mode == "both") {
    if (is.null(season_vals)) {
      idx <- str_detect(
        stem,
        regex(paste0(prefix_rx, "_\\d+(?:_\\d+)?$"))
      )
      return(select_urls(idx, "all seasons (season- and week-level assets)"))
    }

    season_alt <- paste0(season_vals, collapse = "|")
    idx <- str_detect(
      stem,
      regex(paste0(prefix_rx, "_(", season_alt, ")(?:_\\d+)?$"))
    )
    return(select_urls(idx, "selected seasons (season- and week-level assets)"))
  }

  if (is.null(season_vals)) {
    week_alt <- paste0(week_vals, collapse = "|")
    idx <- str_detect(
      stem,
      regex(paste0(prefix_rx, "_\\d+_(", week_alt, ")$"))
    )
    return(select_urls(idx, "all seasons, selected weeks (week-level assets)"))
  }

  season_alt <- paste0(season_vals, collapse = "|")
  week_alt <- paste0(week_vals, collapse = "|")
  idx <- str_detect(
    stem,
    regex(paste0(prefix_rx, "_(", season_alt, ")_(", week_alt, ")$"))
  )
  select_urls(idx, "selected seasons and weeks (week-level assets)")
}

export(pb_download_url_szn_wk)
