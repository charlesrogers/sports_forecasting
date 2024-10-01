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
  
  # Make individual model predictions
  individual_predictions <- predict_individual_models(individual_models, new_game)
  
  # Make ensemble prediction
  ensemble_prediction <- predict(ensemble_model, newdata = individual_predictions, type = "response")
  
  # Combine all predictions
  all_predictions <- c(individual_predictions, Ensemble = ensemble_prediction)
  
  return(all_predictions)
}