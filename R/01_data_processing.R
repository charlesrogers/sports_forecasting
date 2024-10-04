# 01_data_processing.R

library(tidyverse)

# Function to read and preprocess historical game data
read_historical_data <- function(file_path) {
  # Read the CSV file
  data <- read_csv(file_path)
  
  # Check if required columns are present
  required_columns <- c("date", "home_team", "away_team", "home_score", "away_score")
  if (!all(required_columns %in% colnames(data))) {
    stop("The historical data file is missing required columns.")
  }
  
  # Convert date column to Date type and calculate win/loss
  data <- data %>%
    mutate(
      date = as.Date(date),
      home_win = as.integer(home_score > away_score)
    )
  
  # Calculate team performance metrics
  team_performance <- data %>%
    bind_rows(
      data %>% select(team = home_team, win = home_win),
      data %>% mutate(win = 1 - home_win) %>% select(team = away_team, win)
    ) %>%
    group_by(team) %>%
    summarise(
      games_played = n(),
      wins = sum(win),
      win_rate = wins / games_played
    )
  
  # Join team performance back to the original data
  data <- data %>%
    left_join(team_performance, by = c("home_team" = "team")) %>%
    rename(home_win_rate = win_rate) %>%
    left_join(team_performance, by = c("away_team" = "team")) %>%
    rename(away_win_rate = win_rate)
  
  return(data)
}

# Function to input new game details
input_new_game <- function() {
  cat("Enter details for the new game:\n")
  home_team <- readline("Home Team: ")
  away_team <- readline("Away Team: ")
  game_date <- readline("Game Date (YYYY-MM-DD): ")
  
  new_game <- tibble(
    date = as.Date(game_date),
    home_team = home_team,
    away_team = away_team
  )
  
  return(new_game)
}