box::use(
  box[export],
  cli[cli_warn],
  dplyr[distinct],
  nflreadr[get_current_season, get_current_week, load_from_url, load_teams],
  rlang[abort],
)

box::use(
  app / logic / data / github_assets[pb_download_url_szn_wk],
  app / logic / data / import[load_game_data, load_game_data_long],
)

current_season <- get_current_season()

all_seasons <- 2006:current_season

current_week <- get_current_week()

teams_data <- load_teams(current = TRUE)

teams <- teams_data$team_abbr

game_data <- load_game_data(seasons = all_seasons)

game_data_long <- load_game_data_long(game_df = game_data)

season_weeks_df <- game_data |> distinct(season, week, week_seq)

github_data_repo <- "TylerPollard410/nflendzoneData"

base_repo_url <- paste0(
  "https://github.com/",
  github_data_repo,
  "/releases/download/"
)

load_release_asset <- function(path) {
  load_from_url(paste0(base_repo_url, path))
}

latest_prediction_asset_url <- function(urls, tag) {
  stems <- sub("\\.[^.]+$", "", basename(urls))
  matches <- regmatches(
    stems,
    regexec(paste0("^", tag, "_([0-9]{4})(?:_([0-9]+))?$"), stems)
  )

  seasons <- vapply(
    matches,
    function(x) if (length(x) >= 2L) as.integer(x[[2]]) else NA_integer_,
    integer(1)
  )
  weeks <- vapply(
    matches,
    function(x) {
      if (length(x) >= 3L && !is.na(x[[3]])) {
        as.integer(x[[3]])
      } else {
        0L
      }
    },
    integer(1)
  )

  keep <- !is.na(seasons)
  if (!any(keep)) {
    abort(paste0(
      "No usable prediction assets were found in the ",
      github_data_repo,
      " release for ",
      tag,
      "."
    ))
  }

  candidates <- urls[keep]
  order_idx <- order(seasons[keep], weeks[keep])
  candidates[order_idx[[length(order_idx)]]]
}

prediction_summary_urls <- pb_download_url_szn_wk(
  "team_strength_negbinom_summary",
  repo = github_data_repo,
  seasons = current_season,
  weeks = TRUE,
  asset_ext = "rds",
  warn_empty = FALSE
)

if (length(prediction_summary_urls) == 0L) {
  all_prediction_summary_urls <- pb_download_url_szn_wk(
    "team_strength_negbinom_summary",
    repo = github_data_repo,
    seasons = TRUE,
    weeks = TRUE,
    asset_ext = "rds"
  )

  if (length(all_prediction_summary_urls) == 0L) {
    abort(paste0(
      "No team strength prediction assets were found in ",
      github_data_repo,
      "."
    ))
  }

  prediction_summary_urls <- latest_prediction_asset_url(
    all_prediction_summary_urls,
    "team_strength_negbinom_summary"
  )

  cli_warn(paste0(
    "No prediction summary asset was found for season ",
    current_season,
    "; using the latest published asset instead: ",
    basename(prediction_summary_urls),
    "."
  ))
}

season_standings_data <- load_release_asset(
  "season_standings/season_standings.rds"
)

team_features_data <- load_release_asset("team_features/team_features.rds")

team_strength_negbinom_summary <- load_from_url(
  prediction_summary_urls
)

export(
  current_season,
  all_seasons,
  current_week,
  teams_data,
  teams,
  game_data,
  game_data_long,
  season_weeks_df,
  github_data_repo,
  base_repo_url,
  season_standings_data,
  team_features_data,
  team_strength_negbinom_summary
)
