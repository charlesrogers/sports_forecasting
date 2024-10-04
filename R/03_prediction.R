# 03_prediction.R

library(tidyverse)

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
  individual_predictions <- sapply(individual_models, function(model) {
    predict(model, newdata = new_game, type = "response")
  })
  
  # Make ensemble prediction
  ensemble_prediction <- predict(ensemble_model, newdata = as.data.frame(t(individual_predictions)), type = "response")
  
  # Combine all predictions
  all_predictions <- c(individual_predictions, Ensemble = ensemble_prediction)
  
  return(all_predictions)
}