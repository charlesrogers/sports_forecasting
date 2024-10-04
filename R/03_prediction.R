# 03_prediction.R

library(tidyverse)

# Function to make predictions using individual models
predict_individual_models <- function(models, data) {
  predictions <- sapply(models, function(model) predict(model, newdata = data, type = "response"))
  return(as.data.frame(predictions))
}

# Function to make predictions for a new game
predict_new_game <- function(new_game, individual_models, ensemble_model, team_performance) {
  # Add team performance data to new game
  new_game <- new_game %>%
    left_join(team_performance, by = c("home_team" = "team")) %>%
    rename(home_win_rate = win_rate) %>%
    left_join(team_performance, by = c("away_team" = "team")) %>%
    rename(away_win_rate = win_rate)
  
  # Check if we have data for both teams
  if (is.na(new_game$home_win_rate) || is.na(new_game$away_win_rate)) {
    stop("No historical data for one or both teams. Unable to make prediction.")
  }
  
  # Make individual model predictions
  individual_predictions <- predict_individual_models(individual_models, new_game)
  
  # Make ensemble prediction
  ensemble_prediction <- predict(ensemble_model, newdata = individual_predictions, type = "response")
  
  # Calculate additional metrics
  home_win_prob <- ensemble_prediction
  away_win_prob <- 1 - home_win_prob
  
  # Simulate game totals (you may want to adjust these based on your data)
  home_team_total <- rnorm(1, mean = 5, sd = 1.5)
  away_team_total <- rnorm(1, mean = 5, sd = 1.5)
  full_game_total <- home_team_total + away_team_total
  home_win_margin <- home_team_total - away_team_total
  
  # Calculate run line probabilities (adjust as needed)
  home_runline_fav <- pnorm(1.5, mean = home_win_margin, sd = 2)
  away_runline_dog <- 1 - home_runline_fav
  away_runline_fav <- pnorm(-1.5, mean = home_win_margin, sd = 2)
  home_runline_dog <- 1 - away_runline_fav
  
  # Combine all predictions and metrics
  metrics <- tibble(
    Metric = c("win_prob", "team_total", "full_game_total", "win_margin", "runline_fav", "runline_dog"),
    !!new_game$home_team := c(home_win_prob, home_team_total, full_game_total, home_win_margin, home_runline_fav, home_runline_dog),
    !!new_game$away_team := c(away_win_prob, away_team_total, full_game_total, -home_win_margin, away_runline_fav, away_runline_dog)
  )
  
  return(metrics)
}