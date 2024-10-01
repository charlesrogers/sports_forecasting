# 02_model_training.R

library(tidyverse)
library(caret)

# Function to train individual models
train_individual_models <- function(data) {
  models <- list()
  
  # Common formula for all models
  formula <- home_win ~ home_win_rate + away_win_rate
  
  # Train models
  models$BTM <- glm(formula, data = data, family = binomial())
  models$TOOR <- glm(formula, data = data, family = binomial())
  models$GSSD <- glm(formula, data = data, family = binomial())
  models$ZSD <- glm(formula, data = data, family = binomial())
  models$PRP <- glm(formula, data = data, family = binomial())
  
  return(models)
}

# Function to make predictions using individual models
predict_individual_models <- function(models, data) {
  predictions <- sapply(models, function(model) predict(model, newdata = data, type = "response"))
  return(as.data.frame(predictions))
}

# Function to train ensemble model
train_ensemble_model <- function(data, individual_predictions) {
  ensemble_data <- cbind(actual_result = data$home_win, individual_predictions)
  
  ensemble_model <- glm(actual_result ~ ., 
                        data = ensemble_data, 
                        family = binomial())
  
  return(ensemble_model)
}