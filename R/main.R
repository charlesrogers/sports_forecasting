# main.R

# Load required libraries
library(tidyverse)
library(caret)
library(pROC)
library(performance)

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
  
  # Compare model performance
  models_list <- c(individual_models, list(Ensemble = ensemble_model))
  performance_comparison <- compare_performance(models_list, metrics = c("AIC", "BIC", "R2", "RMSE"))
  print(performance_comparison)
  
  # Calculate team performance
  team_performance <- historical_data %>%
    group_by(team = home_team) %>%
    summarise(win_rate = mean(home_win))
  
  # Console input for prediction
  cat("Available teams:\n")
  cat(paste(unique(c(historical_data$home_team, historical_data$away_team)), collapse = ", "), "\n\n")
  
  home_team <- readline("Enter Home Team: ")
  away_team <- readline("Enter Away Team: ")
  
  new_game <- tibble(
    home_team = home_team,
    away_team = away_team
  )
  
  predictions <- predict_new_game(new_game, individual_models, ensemble_model, team_performance)
  print(predictions)
  
  # Return the models and data for use in the Quarto document
  return(list(
    historical_data = historical_data,
    individual_models = individual_models,
    ensemble_model = ensemble_model,
    team_performance = team_performance,
    performance_comparison = performance_comparison
  ))
}

# Run the main function and save the results
results <- main()
save(results, file = "results.RData")