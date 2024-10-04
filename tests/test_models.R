# tests/test_models.R

library(testthat)
library(tidyverse)

# Source the necessary files
source("logging.R")
source("R/02_model_training.R")
source("R/03_prediction.R")

# Sample data for testing
sample_data <- tibble(
  home_team = c("A", "B", "C"),
  away_team = c("B", "C", "A"),
  home_win = c(1, 0, 1),
  home_win_rate = c(0.6, 0.5, 0.7),
  away_win_rate = c(0.5, 0.7, 0.6)
)

test_that("Individual models can be trained", {
  models <- train_individual_models(sample_data)
  expect_type(models, "list")
  expect_true(all(c("TOOR", "GSSD", "ZSD", "PRP") %in% names(models)))
})

test_that("Individual models can make predictions", {
  models <- train_individual_models(sample_data)
  predictions <- predict_individual_models(models, sample_data)
  expect_s3_class(predictions, "data.frame")
  expect_equal(nrow(predictions), nrow(sample_data))
  expect_true(all(c("TOOR", "GSSD", "ZSD", "PRP") %in% names(predictions)))
})

test_that("Ensemble model can be trained", {
  models <- train_individual_models(sample_data)
  predictions <- predict_individual_models(models, sample_data)
  ensemble_model <- train_ensemble_model(sample_data, predictions)
  expect_s3_class(ensemble_model, "glm")
})

test_that("New game prediction works", {
  models <- train_individual_models(sample_data)
  predictions <- predict_individual_models(models, sample_data)
  ensemble_model <- train_ensemble_model(sample_data, predictions)
  team_performance <- sample_data %>%
    group_by(team = home_team) %>%
    summarise(win_rate = mean(home_win))
  
  new_game <- tibble(home_team = "A", away_team = "B")
  result <- predict_new_game(new_game, models, ensemble_model, team_performance)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 7)  # 7 metrics
  expect_true(all(c("Metric", "Result") %in% names(result)))
})