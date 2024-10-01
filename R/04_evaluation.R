# 04_evaluation.R

library(tidyverse)
library(pROC)

# Function to calculate model performance metrics
calculate_metrics <- function(actual, predicted) {
  confusion_matrix <- table(Actual = actual, Predicted = predicted > 0.5)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  roc_obj <- roc(actual, predicted)
  auc <- auc(roc_obj)
  
  metrics <- list(
    accuracy = accuracy,
    auc = auc,
    confusion_matrix = confusion_matrix
  )
  
  return(metrics)
}

# Function to evaluate all models
evaluate_models <- function(data, individual_models, ensemble_model) {
  individual_predictions <- predict_individual_models(individual_models, data)
  ensemble_prediction <- predict(ensemble_model, newdata = individual_predictions, type = "response")
  
  all_predictions <- cbind(individual_predictions, Ensemble = ensemble_prediction)
  
  evaluation_results <- lapply(all_predictions, function(pred) {
    calculate_metrics(data$home_win, pred)
  })
  
  return(evaluation_results)
}