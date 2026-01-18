box::use(
  dplyr[distinct],
  nflreadr[load_teams, get_current_season, get_current_week, load_from_url],
)

box::use(
  app /
    logic /
    data_import_functions[
      load_game_data,
      load_game_data_long,
      pb_download_url_szn_wk
    ],
)

#' @export
current_season <- get_current_season()

#' @export
all_seasons <- 2006:current_season

#' @export
current_week <- get_current_week()

#' @export
teams_data <- load_teams(current = TRUE)

#' @export
teams <- teams_data$team_abbr

#' @export
game_data <- load_game_data(seasons = all_seasons)

#' @export
game_data_long <- load_game_data_long(game_df = game_data)

#' @export
season_weeks_df <- game_data |> distinct(season, week, week_seq)

#' @export
github_data_repo <- "TylerPollard410/nflendzoneData"

#' @export
base_repo_url <- paste0(
  "https://github.com/",
  github_data_repo,
  "/releases/download/"
)

load_release_asset <- function(path) {
  load_from_url(paste0(base_repo_url, path))
}

#' @export
season_standings_data <- load_release_asset(
  "season_standings/season_standings.rds"
)

#' @export
team_features_data <- load_release_asset("team_features/team_features.rds")

#' @export
team_strength_negbinom_summary <- load_from_url(
  pb_download_url_szn_wk(
    "team_strength_negbinom_summary",
    repo = github_data_repo,
    seasons = current_season,
    weeks = TRUE,
    file_ext = "rds"
  )
)
