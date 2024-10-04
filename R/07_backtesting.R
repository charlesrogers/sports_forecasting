# backtesting.R

library(tidyverse)

# Function to perform backtesting
backtest <- function(data, models, ensemble_model, cutoff = 0) {
  results <- data %>%
    mutate(
      individual_predictions = pmap(list(home_team, away_team, home_win_rate, away_win_rate), 
                                    ~predict_individual_models(models, tibble(home_team = ..1, away_team = ..2, 
                                                                              home_win_rate = ..3, away_win_rate = ..4))),
      ensemble_prediction = map_dbl(individual_predictions, ~predict(ensemble_model, newdata = as.data.frame(t(.)), type = "response")),
      expected_value = ensemble_prediction - (1 / home_odds),
      bet = expected_value > cutoff,
      outcome = ifelse(home_score > away_score, 1, 0),
      profit = ifelse(bet, ifelse(outcome == 1, home_odds - 1, -1), 0)
    )
  
  summary <- results %>%
    summarise(
      total_bets = sum(bet),
      total_wins = sum(bet & outcome == 1),
      profit = sum(profit),
      roi = profit / sum(bet),
      win_rate = total_wins / total_bets
    )
  
  return(list(results = results, summary = summary))
}

# Function to calculate p-value (based on Joseph Buchdahl's method)
calculate_p_value <- function(wins, bets, odds) {
  observed_yield <- (wins * (odds - 1) - (bets - wins)) / bets
  expected_yield <- 1 / odds - 1
  se_yield <- sqrt((odds^2 * wins + (bets - wins)) / (bets^3))
  t_statistic <- (observed_yield - expected_yield) / se_yield
  p_value <- 1 - pnorm(t_statistic)
  return(p_value)
}

# Load the data and models
load("results.RData")

# Perform backtesting
backtest_results <- backtest(results$historical_data, results$individual_models, results$ensemble_model, cutoff = 0.05)

# Print summary
print(backtest_results$summary)

# Calculate p-value
p_value <- calculate_p_value(backtest_results$summary$total_wins, 
                             backtest_results$summary$total_bets, 
                             mean(results$historical_data$home_odds, na.rm = TRUE))

print(paste("P-value:", p_value))

# Plot cumulative profit
ggplot(backtest_results$results, aes(x = 1:nrow(backtest_results$results), y = cumsum(profit))) +
  geom_line() +
  labs(title = "Cumulative Profit", x = "Number of Bets", y = "Profit")
```