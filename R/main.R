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

# Function to input new game details
input_new_game <- function(team_names) {
  cat("Available teams:\n")
  cat(paste(team_names, collapse = ", "), "\n\n")
  
  home_team <- readline("Enter Home Team: ")
  while(!(home_team %in% team_names)) {
    cat("Invalid team name. Please try again.\n")
    home_team <- readline("Enter Home Team: ")
  }
  
  away_team <- readline("Enter Away Team: ")
  while(!(away_team %in% team_names) || away_team == home_team) {
    if(away_team == home_team) {
      cat("Away team must be different from home team. Please try again.\n")
    } else {
      cat("Invalid team name. Please try again.\n")
    }
    away_team <- readline("Enter Away Team: ")
  }
  
  new_game <- tibble(
    home_team = home_team,
    away_team = away_team
  )
  
  return(new_game)
}

# Main function to run the program
main <- function() {
  # Read and preprocess historical data
  tryCatch({
    historical_data <- read_historical_data("data/historical_games.csv")
  }, error = function(e) {
    cat("Error reading historical data:", e$message, "\n")
    cat("Please ensure 'data/historical_games.csv' exists and contains the required columns.\n")
    return(NULL)
  })
  
  if (is.null(historical_data)) return()
  
  # Validate data
  if (nrow(historical_data) < 100) {
    cat("Warning: The historical dataset is quite small. Results may not be reliable.\n")
  }
  
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
  
  # Plot model comparison
  plot(performance_comparison)
  
  # Plot ROC curve for ensemble model
  test_predictions <- predict_individual_models(individual_models, test_data)
  ensemble_prediction <- predict(ensemble_model, newdata = test_predictions, type = "response")
  plot_roc_curve(test_data$home_win, ensemble_prediction)
  
  # Get unique team names from historical data
  team_names <- unique(c(historical_data$home_team, historical_data$away_team))
  
  # Input new game and make predictions
  new_game <- input_new_game(team_names)
  team_performance <- historical_data %>%
    group_by(team = home_team) %>%
    summarise(win_rate = mean(home_win))
  
  tryCatch({
    new_game_predictions <- predict_new_game(new_game, individual_models, ensemble_model, team_performance)
    
    # Plot predictions for the new game
    plot_model_predictions(new_game_predictions)
    
    # Print predictions
    cat("\nPredictions for the new game:\n")
    print(new_game_predictions)
  }, error = function(e) {
    cat("Error making predictions for the new game:", e$message, "\n")
    cat("This might be due to missing team data or other issues.\n")
  })
  
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
quarto::quarto_render("index.qmd")