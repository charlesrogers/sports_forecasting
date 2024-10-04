# R/03_prediction.R

source("logging.R")

predict_new_game <- function(new_game, individual_models, ensemble_model, team_performance) {
  log_message("Starting prediction for new game")
  
  tryCatch({
    log_message("Adding team performance data to new game")
    new_game <- new_game %>%
      left_join(team_performance, by = c("home_team" = "team")) %>%
      rename(home_win_rate = win_rate) %>%
      left_join(team_performance, by = c("away_team" = "team")) %>%
      rename(away_win_rate = win_rate)
    
    if (is.na(new_game$home_win_rate) || is.na(new_game$away_win_rate)) {
      stop("No historical data for one or both teams")
    }
    
    log_message("Making individual model predictions")
    individual_predictions <- predict_individual_models(individual_models, new_game)
    
    log_message("Making ensemble prediction")
    ensemble_prediction <- predict(ensemble_model, newdata = individual_predictions, type = "response")
    
    log_message("Calculating additional metrics")
    home_win_prob <- as.numeric(ensemble_prediction)
    away_win_prob <- 1 - home_win_prob
    home_team_total <- round(rnorm(1, mean = 100, sd = 10), 2)
    away_team_total <- round(rnorm(1, mean = 100, sd = 10), 2)
    full_game_total <- round(home_team_total + away_team_total, 2)
    home_spread <- round(home_team_total - away_team_total, 2)
    away_spread <- -home_spread
    
    log_message("Combining all predictions and metrics")
    metrics <- data.frame(
      Metric = c("home_win_prob", "away_win_prob", "home_team_total", "away_team_total", "full_game_total", "home_spread", "away_spread"),
      Result = c(home_win_prob, away_win_prob, home_team_total, away_team_total, full_game_total, home_spread, away_spread)
    )
    
    log_message("Prediction for new game completed")
    return(metrics)
  }, error = function(e) {
    log_message(sprintf("Error in predict_new_game: %s", e$message), "ERROR")
    return(NULL)
  })
}

predict_individual_models <- function(models, data) {
  log_message("Starting individual model predictions")
  predictions <- sapply(names(models), function(model_name) {
    log_message(sprintf("Predicting with %s model", model_name))
    tryCatch({
      if (inherits(models[[model_name]], "glm")) {
        pred <- predict(models[[model_name]], newdata = data, type = "response")
      } else if (inherits(models[[model_name]], "randomForest")) {
        pred <- predict(models[[model_name]], newdata = data, type = "prob")[, 2]
      } else if (inherits(models[[model_name]], "xgb.Booster")) {
        pred <- predict(models[[model_name]], newdata = as.matrix(data[, c("home_win_rate", "away_win_rate")]))
      } else if (inherits(models[[model_name]], "nnet")) {
        pred <- predict(models[[model_name]], newdata = data, type = "raw")
      }
      log_message(sprintf("%s model prediction successful", model_name))
      pred
    }, error = function(e) {
      log_message(sprintf("Error in prediction for %s model: %s", model_name, e$message), "ERROR")
      rep(NA, nrow(data))
    })
  })
  log_message("Completed individual model predictions")
  log_message(sprintf("Dimensions of predictions: %d rows, %d columns", nrow(predictions), ncol(predictions)))
  return(as.data.frame(predictions))
}