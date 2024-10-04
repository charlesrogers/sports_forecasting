# main.R

source("logging.R")
source("R/01_data_processing.R")
source("R/02_model_training.R")
source("R/03_prediction.R")
source("R/04_evaluation.R")
source("R/05_visualization.R")

library(tidyverse)
library(caret)
library(pROC)
library(performance)
library(randomForest)
library(xgboost)
library(nnet)

main <- function() {
  log_message("Starting main function")
  
  tryCatch({
    log_message("Reading and preprocessing historical data")
    historical_data <- read_historical_data("data/historical_games.csv")
    
    log_message("Splitting data into training and testing sets")
    set.seed(123)
    train_indices <- sample(1:nrow(historical_data), 0.8 * nrow(historical_data))
    train_data <- historical_data[train_indices, ]
    test_data <- historical_data[-train_indices, ]
    
    log_message("Training individual models")
    individual_models <- train_individual_models(train_data)
    log_message(sprintf("Individual models trained: %s", paste(names(individual_models), collapse = ", ")))
    
    log_message("Making predictions using individual models on training data")
    train_predictions <- predict_individual_models(individual_models, train_data)
    
    log_message("Training ensemble model")
    ensemble_model <- train_ensemble_model(train_data, train_predictions)
    
    log_message("Evaluating models on test data")
    evaluation_results <- evaluate_models(test_data, individual_models, ensemble_model)
    log_message("Evaluation results:")
    print(evaluation_results)
    
    log_message("Comparing model performance")
    models_list <- c(individual_models, list(Ensemble = ensemble_model))
    performance_comparison <- compare_performance(models_list, metrics = c("AIC", "BIC", "R2", "RMSE"))
    log_message("Performance comparison:")
    print(performance_comparison)
    
    log_message("Calculating team performance")
    team_performance <- historical_data %>%
      group_by(team = home_team) %>%
      summarise(win_rate = mean(home_win))
    
    log_message("Starting console input for prediction")
    cat("Available teams:\n")
    cat(paste(unique(c(historical_data$home_team, historical_data$away_team)), collapse = ", "), "\n\n")
    
    home_team <- readline("Enter Home Team: ")
    away_team <- readline("Enter Away Team: ")
    
    new_game <- tibble(
      home_team = home_team,
      away_team = away_team
    )
    
    log_message("Making prediction for new game")
    predictions <- predict_new_game(new_game, individual_models, ensemble_model, team_performance)
    
    if (!is.null(predictions)) {
      log_message("Prediction successful")
      cat("\nHead-to-head Prediction:\n")
      print(predictions, row.names = FALSE)
      
      # Save predictions to a dataframe
      df.head_to_head <- predictions
      assign("df.head_to_head", df.head_to_head, envir = .GlobalEnv)
      log_message("Predictions saved to df.head_to_head")
    } else {
      log_message("Failed to generate predictions", "ERROR")
    }
    
    log_message("Saving results")
    results <- list(
      historical_data = historical_data,
      individual_models = individual_models,
      ensemble_model = ensemble_model,
      team_performance = team_performance,
      performance_comparison = performance_comparison
    )
    save(results, file = "results.RData")
    
    log_message("Main function completed")
    return(results)
  }, error = function(e) {
    log_message(sprintf("Error in main function: %s", e$message), "ERROR")
    return(NULL)
  })
}

log_message("Running main function")
results <- main()