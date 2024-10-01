# main.R

# Load required libraries
library(tidyverse)
library(caret)
library(pROC)

# Source all the R scripts
source("R/01_data_processing.R")
source("R/02_model_training.R")
source("R/03_prediction.R")
source("R/04_evaluation.R")
source("R/05_visualization.R")

# Main function to run the program
main <- function() {
  # Read and preprocess historical data
  historical_data <- read_historical_data("data/historical_games.csv")
  
  # Split data into training and testing sets
  set.seed(123)
  train_indices <- sample(1:nrow(historical_data), 0.8 * nrow(historical_data))
  train_data <- historical_data[train_indices, ]
  test_data <- historical_data[-train_indices, ]
  
  # Train individual models
  individual_models <- train_individual_models(train_data)
  
  # Make predictions using individual models on training data
  train_predictions <- predict_individual_models(individual_models, train_data)
  
  # Train ensemble model
  ensemble_model <- train_ensemble_model(train_data, train_predictions)
  
  # Evaluate models on test data
  evaluation_results <- evaluate_models(test_data, individual_models, ensemble_model)
  
  # Print evaluation results
  print(evaluation_results)
  
  # Plot ROC curve for ensemble model
  test_predictions <- predict_individual_models(individual_models, test_data)
  ensemble_prediction <- predict(ensemble_model, newdata = test_predictions, type = "response")
  plot_roc_curve(test_data$home_win, ensemble_prediction)
  
  # Input new game and make predictions
  new_game <- input_new_game()
  team_performance <- historical_data %>%
    group_by(team = home_team) %>%
    summarise(win_rate = mean(home_win))
  
  new_game_predictions <- predict_new_game(new_game, individual_models, ensemble_model, team_performance)
  
  # Plot predictions for the new game
  plot_model_predictions(new_game_predictions)
  
  # Print predictions
  cat("\nPredictions for the new game:\n")
  print(new_game_predictions)
}

# Run the main function
main()