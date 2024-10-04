# R/02_model_training.R

source("logging.R")

train_individual_models <- function(data) {
  log_message("Starting individual model training")
  models <- list()
  
  # Define model types
  model_types <- c("GLM", "RF", "XGB", "NN")
  
  for (model_type in model_types) {
    log_message(sprintf("Training %s model", model_type))
    models[[model_type]] <- tryCatch({
      if (model_type == "GLM") {
        model <- glm(home_win ~ home_win_rate + away_win_rate, data = data, family = binomial())
      } else if (model_type == "RF") {
        model <- randomForest::randomForest(as.factor(home_win) ~ home_win_rate + away_win_rate, data = data)
      } else if (model_type == "XGB") {
        model <- xgboost::xgboost(data = as.matrix(data[, c("home_win_rate", "away_win_rate")]), 
                                  label = data$home_win, 
                                  nrounds = 100, 
                                  objective = "binary:logistic",
                                  verbose = 0)
      } else if (model_type == "NN") {
        model <- nnet::nnet(home_win ~ home_win_rate + away_win_rate, data = data, size = 5, maxit = 1000)
      }
      log_message(sprintf("%s model trained successfully", model_type))
      model
    }, error = function(e) {
      log_message(sprintf("Error training %s model: %s", model_type, e$message), "ERROR")
      NULL
    })
  }
  
  models <- models[!sapply(models, is.null)]
  log_message(sprintf("Completed training. Models available: %s", paste(names(models), collapse = ", ")))
  
  if (length(models) == 0) {
    log_message("No models were successfully trained", "ERROR")
    stop("No models were successfully trained.")
  }
  
  return(models)
}

train_ensemble_model <- function(data, individual_predictions) {
  log_message("Starting ensemble model training")
  log_message(sprintf("Dimensions of data: %d rows, %d columns", nrow(data), ncol(data)))
  log_message(sprintf("Dimensions of individual_predictions: %d rows, %d columns", nrow(individual_predictions), ncol(individual_predictions)))
  
  if (nrow(data) != nrow(individual_predictions)) {
    log_message("Mismatch in number of rows between data and individual_predictions", "ERROR")
    stop("Mismatch in number of rows between data and individual_predictions")
  }
  
  ensemble_data <- cbind(actual_result = data$home_win, individual_predictions)
  
  ensemble_model <- tryCatch({
    model <- glm(actual_result ~ ., data = ensemble_data, family = binomial())
    log_message("Ensemble model trained successfully")
    model
  }, error = function(e) {
    log_message(sprintf("Error in training ensemble model: %s", e$message), "ERROR")
    NULL
  })
  
  if (is.null(ensemble_model)) {
    log_message("Failed to train ensemble model", "ERROR")
    stop("Failed to train ensemble model.")
  }
  
  return(ensemble_model)
}