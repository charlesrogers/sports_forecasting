# create_dummy_data.R

library(tidyverse)

# Set seed for reproducibility
set.seed(123)

# Create dummy historical data
create_dummy_historical_data <- function(n_games = 1000) {
  teams <- c("Team A", "Team B", "Team C", "Team D", "Team E")
  
  tibble(
    date = seq.Date(as.Date("2020-01-01"), by = "day", length.out = n_games),
    home_team = sample(teams, n_games, replace = TRUE),
    away_team = sample(teams, n_games, replace = TRUE),
    home_score = rpois(n_games, lambda = 2),
    away_score = rpois(n_games, lambda = 2)
  ) %>%
    filter(home_team != away_team)  # Ensure home and away teams are different
}

# Generate dummy data
dummy_data <- create_dummy_historical_data()

# Write to CSV
write_csv(dummy_data, "data/historical_games.csv")

print("Dummy data has been created and saved to 'data/historical_games.csv'")