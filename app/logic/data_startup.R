box::use(
  dplyr[select, arrange, distinct],
  nflreadr[load_teams, most_recent_season, rds_from_url],
)

box::use(
  app / logic / data_import_functions[load_game_data, load_game_data_long],
)

#' @export
all_seasons <- 2006:most_recent_season()

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
base_repo_url <- "https://github.com/TylerPollard410/nflendzoneData/releases/download/"
# season_standings_data <- readRDS(url(paste0(base_repo_url, "season_standings/season_standings.rds")))
# feature_long_data <- readRDS(url(paste0(base_repo_url, "feature_long/feature_long.rds")))

#' @export
season_standings_data <- rds_from_url(paste0(
  base_repo_url,
  "season_standings/season_standings.rds"
))

#' @export
team_features_data <- rds_from_url(paste0(
  base_repo_url,
  "team_features/team_features.rds"
))
