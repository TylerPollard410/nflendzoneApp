box::use(
  box[export],
  dplyr,
  nflreadr[
    clean_homeaway,
    clean_team_abbrs,
    load_schedules,
    most_recent_season
  ],
  stringr[str_extract],
)

box::use(
  app / logic / utils / odds[american_odds_to_prob],
)

#' @param df A data frame with `season` and `week` columns.
add_week_seq <- function(df) {
  weeks_tbl <- df |>
    dplyr$distinct(season, week) |>
    dplyr$arrange(season, week) |>
    dplyr$mutate(week_seq = dplyr$row_number())

  df |>
    dplyr$left_join(weeks_tbl, by = c("season", "week")) |>
    dplyr$relocate(week_seq, .after = week)
}

#' @param seasons Integer vector of seasons to include (default: 2006 through the
#'   most recent available season).
load_game_data <- function(seasons = 2006:most_recent_season()) {
  seasons <- unique(as.integer(seasons))
  games <- load_schedules(seasons = seasons)
  drop_cols <- c(
    "old_game_id",
    "gsis",
    "nfl_detail_id",
    "pfr",
    "pff",
    "espn",
    "ftn",
    "away_qb_id",
    "home_qb_id",
    "stadium_id"
  )

  games |>
    dplyr$filter(season %in% seasons) |>
    dplyr$mutate(
      home_team = clean_team_abbrs(home_team),
      away_team = clean_team_abbrs(away_team),
      # season type: REG vs POST
      season_type = ifelse(game_type == "REG", "REG", "POST"),
      # betting probabilities from American odds
      home_spread_prob = american_odds_to_prob(home_spread_odds),
      away_spread_prob = american_odds_to_prob(away_spread_odds),
      under_prob = american_odds_to_prob(under_odds),
      over_prob = american_odds_to_prob(over_odds),
      home_moneyline_prob = american_odds_to_prob(home_moneyline),
      away_moneyline_prob = american_odds_to_prob(away_moneyline),
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
    ) |>
    dplyr$select(-gamehour) |>
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
    dplyr$select(-dplyr$any_of(drop_cols))
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
#' @return A tibble in long format with one row per team-game.
#'
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
      team_W = cumsum(dplyr$coalesce(result > 0, FALSE)),
      team_L = cumsum(dplyr$coalesce(result < 0, FALSE)),
      team_T = team_GP - team_W - team_L,
      team_PF = cumsum(dplyr$coalesce(team_score, 0)),
      team_PFG = team_PF / team_GP,
      team_PA = cumsum(dplyr$coalesce(opponent_score, 0)),
      team_PAG = team_PA / team_GP
    ) |>
    dplyr$mutate(
      team_W = dplyr$lag(team_W, default = 0),
      team_L = dplyr$lag(team_L, default = 0),
      team_T = dplyr$lag(team_T, default = 0)
    ) |>
    dplyr$ungroup() |>
    dplyr$group_by(game_id) |>
    dplyr$mutate(
      locationID = dplyr$row_number(),
      .after = location
    ) |>
    dplyr$ungroup()
}

export(add_week_seq, load_game_data, load_game_data_long)
