box::use(
  dplyr,
  stringr[str_extract],
  nflreadr[
    load_schedules,
    most_recent_season,
    clean_team_abbrs,
    clean_homeaway
  ],
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

#' @param seasons Integer vector of seasons to include (default: 2006 through the most recent available season).
#' @export
load_game_data <- function(seasons = 2006:most_recent_season()) {
  games <- load_schedules(seasons = TRUE) # load schedule for specified seasons
  games |>
    dplyr$filter(season >= min(seasons)) |> # ensure only seasons at or after the minimum
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
#'   - Per-team cumulative statistics (`team_GP`, `team_W`, `team_L`, `team_T`, `team_PF`, `team_PFG`, `team_PA`, `team_PAG`)
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
