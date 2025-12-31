box::use(
  cli[cli_warn],
  dplyr,
  gh[gh_token],
  nflreadr[
    load_schedules,
    most_recent_season,
    clean_team_abbrs,
    clean_homeaway
  ],
  piggyback[pb_download_url],
  rlang[arg_match, abort],
  stringr[str_extract, str_detect, regex, str_replace_all],
  tools[file_ext, file_path_sans_ext],
  utils[head]
)

#' @param df A data frame with `season` and `week` columns
#' @export
add_week_seq <- function(df) {
  # 1) Extract distinct season/week combinations, sorted
  weeks_tbl <- df |>
    dplyr$distinct(season, week) |>
    dplyr$arrange(season, week) |>
    dplyr$mutate(week_seq = dplyr$row_number())

  # 2) Join back onto the original data frame
  df |>
    dplyr$left_join(weeks_tbl, by = c("season", "week")) |>
    dplyr$relocate(week_seq, .after = week)
}

#' @param seasons Integer vector of seasons to include (default: 2006 through the
#'   most recent available season).
#' @export
load_game_data <- function(seasons = 2006:most_recent_season()) {
  seasons <- unique(as.integer(seasons))
  games <- load_schedules(seasons = seasons)
  games |>
    dplyr$filter(season %in% seasons) |>
    dplyr$mutate(
      home_team = clean_team_abbrs(home_team),
      away_team = clean_team_abbrs(away_team),
      # season type: REG vs POST
      season_type = ifelse(game_type == "REG", "REG", "POST"),
      # betting probabilities from American odds
      home_spread_prob = ifelse(
        home_spread_odds < 0,
        abs(home_spread_odds) / (abs(home_spread_odds) + 100),
        100 / (home_spread_odds + 100)
      ),
      away_spread_prob = ifelse(
        away_spread_odds < 0,
        abs(away_spread_odds) / (abs(away_spread_odds) + 100),
        100 / (away_spread_odds + 100)
      ),
      under_prob = ifelse(
        under_odds < 0,
        abs(under_odds) / (abs(under_odds) + 100),
        100 / (under_odds + 100)
      ),
      over_prob = ifelse(
        over_odds < 0,
        abs(over_odds) / (abs(over_odds) + 100),
        100 / (over_odds + 100)
      ),
      home_moneyline_prob = ifelse(
        home_moneyline < 0,
        abs(home_moneyline) / (abs(home_moneyline) + 100),
        100 / (home_moneyline + 100)
      ),
      away_moneyline_prob = ifelse(
        away_moneyline < 0,
        abs(away_moneyline) / (abs(away_moneyline) + 100),
        100 / (away_moneyline + 100)
      ),
      # cover flags and winner
      spreadCover = dplyr$case_when(
        result > spread_line ~ TRUE,
        result < spread_line ~ FALSE,
        TRUE ~ NA
      ),
      totalCover = dplyr$case_when(
        total > total_line ~ TRUE,
        total < total_line ~ FALSE,
        TRUE ~ NA
      ),
      winner = dplyr$case_when(
        result > 0 ~ home_team,
        result < 0 ~ away_team,
        TRUE ~ NA_character_
      ),
      # time of day based on gametime ("HH:MM:SS" or similar)
      gamehour = as.numeric(str_extract(gametime, "[:digit:]+(?=:)")),
      time_of_day = dplyr$case_when(
        gamehour < 15 ~ "Day",
        dplyr$between(gamehour, 15, 18) ~ "Evening",
        gamehour > 18 ~ "Night",
        TRUE ~ NA_character_
      )
    ) |> # remove helper and order columns
    dplyr$select(-gamehour) |> # drop intermediate field
    dplyr$relocate(season_type, .after = game_type) |>
    dplyr$relocate(home_spread_prob, .after = home_spread_odds) |>
    dplyr$relocate(away_spread_prob, .after = away_spread_odds) |>
    dplyr$relocate(under_prob, .after = under_odds) |>
    dplyr$relocate(over_prob, .after = over_odds) |>
    dplyr$relocate(home_moneyline_prob, .after = home_moneyline) |>
    dplyr$relocate(away_moneyline_prob, .after = away_moneyline) |>
    dplyr$relocate(spreadCover, .after = spread_line) |>
    dplyr$relocate(totalCover, .after = total_line) |>
    dplyr$relocate(winner, .after = result) |>
    dplyr$relocate(time_of_day, .after = gametime) |>
    add_week_seq() |>
    dplyr$select(
      -old_game_id,
      -gsis,
      -nfl_detail_id,
      -pfr,
      -pff,
      -espn,
      -ftn,
      -away_qb_id,
      -home_qb_id,
      -stadium_id
    )
}

#' Compute Long-Format Team-Game Data from Game Schedule
#'
#' Takes a game-level data frame and generates a long-format team-game data set
#' with per-team rolling records and game stats.
#'
#' @param game_df Data frame or tibble of game schedule data. Must contain columns:
#'   `season`, `game_id`, `team`, `opponent`, `result`, `spread_line`,
#'   `team_score`, `opponent_score`, `winner`, and `location`.
#'
#' @return A tibble in long format with one row per team-game, including:
#'   - Per-team cumulative statistics (`team_GP`, `team_W`, `team_L`, `team_T`,
#'     `team_PF`, `team_PFG`, `team_PA`, `team_PAG`)
#'   - `winner` flag (TRUE if the team won, FALSE if lost, NA if tie or missing)
#'   - `locationID` (1 or 2, indicating home vs. away after reshaping)
#'
#' @details
#' This function reshapes the input `game_df` to long format (one row per team per game)
#' using `clean_homeaway()` to handle home/away labeling. It then groups by `season` and `team`
#' to compute running tallies for games played, wins, losses, ties, and points for/against,
#' as well as per-game averages. Afterward, it assigns a `locationID` within each `game_id`
#' to distinguish the two sides.
#'
#' @seealso \code{\link{clean_homeaway}}
#'
#' @export
load_game_data_long <- function(game_df) {
  game_df |>
    clean_homeaway(invert = c("result", "spread_line")) |>
    dplyr$group_by(season, team) |>
    dplyr$mutate(
      team_GP = dplyr$row_number(),
      winner = ifelse(
        team == winner,
        TRUE,
        ifelse(opponent == winner, FALSE, NA)
      ),
      team_W = cumsum(result > 0),
      team_L = cumsum(result < 0),
      team_T = team_GP - team_W - team_L,
      team_PF = cumsum(team_score),
      team_PFG = team_PF / team_GP,
      team_PA = cumsum(opponent_score),
      team_PAG = team_PA / team_GP
    ) |>
    dplyr$mutate(
      team_W = ifelse(is.na(dplyr$lag(team_W)), 0, dplyr$lag(team_W)),
      team_L = ifelse(is.na(dplyr$lag(team_L)), 0, dplyr$lag(team_L)),
      team_T = ifelse(is.na(dplyr$lag(team_T)), 0, dplyr$lag(team_T))
    ) |>
    dplyr$ungroup() |>
    dplyr$group_by(game_id) |>
    dplyr$mutate(
      locationID = dplyr$row_number(),
      .after = location
    ) |>
    dplyr$ungroup()
}

#' @export
pb_download_url_szn_wk <- function(
  tag,
  repo,
  seasons = TRUE,
  weeks = TRUE,
  file_ext = NULL,
  .token = gh_token()
) {
  # surl_type <- arg_match(url_type, values = c("browser", "api"))

  normalize_ext <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
      abort(
        "`file_ext` must be NULL or a single string like '.rds' or 'rds'."
      )
    }
    x <- trimws(x)
    if (x == "") {
      abort("`file_ext` must be NULL or a non-empty string.")
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
  file_ext <- normalize_ext(file_ext)

  all_urls <- pb_download_url(
    file = NULL,
    repo = repo,
    tag = tag,
    .token = .token
  )

  all_files <- basename(all_urls)

  if (!is.null(file_ext)) {
    keep_ext <- tolower(paste0(".", file_ext(all_files))) == file_ext
    all_urls <- all_urls[keep_ext]
    all_files <- all_files[keep_ext]
  }

  stem <- file_path_sans_ext(all_files)

  # Accept either "tag" prefix (what pb returns) or "tag/tag" (your theoretical layout)
  # i.e. match: tag, tag_2025, tag_2025_17  OR  tag/tag, tag/tag_2025, tag/tag_2025_17
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

  # seasons == NULL => prefer top aggregation: tag(.ext) or tag/tag(.ext)
  if (is.null(seasons)) {
    idx <- str_detect(stem, regex(paste0(prefix_rx, "$")))
    return(select_urls(idx, "all-seasons aggregate (tag.ext or tag/tag.ext)"))
  }

  season_vals <- if (isTRUE(seasons)) NULL else as.integer(seasons)

  weeks_mode <- if (isTRUE(weeks) || is.null(weeks)) "both" else "week_only"
  week_vals <- if (weeks_mode == "week_only") as.integer(weeks) else NULL

  if (weeks_mode == "both") {
    # tag_<season>  OR tag_<season>_<week>
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

  # week_only: tag_<season>_<week>
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
