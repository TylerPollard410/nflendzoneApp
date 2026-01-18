box::use(
  box[export],
  dplyr[distinct],
  nflreadr[get_current_season, get_current_week, load_from_url, load_teams],
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

season_standings_data <- load_release_asset(
  "season_standings/season_standings.rds"
)

team_features_data <- load_release_asset("team_features/team_features.rds")

team_strength_negbinom_summary <- load_from_url(
  pb_download_url_szn_wk(
    "team_strength_negbinom_summary",
    repo = github_data_repo,
    seasons = current_season,
    weeks = TRUE,
    asset_ext = "rds"
  )
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
