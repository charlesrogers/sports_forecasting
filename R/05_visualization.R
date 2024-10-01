# 05_visualization.R

library(tidyverse)
library(ggplot2)

# Function to create a bar plot of model predictions
plot_model_predictions <- function(predictions) {
  plot_data <- tibble(
    model = names(predictions),
    win_probability = predictions
  )
  
  ggplot(plot_data, aes(x = model, y = win_probability, fill = model)) +
    geom_bar(stat = "identity") +
    labs(title = "Win Probability by Model",
         x = "Model",
         y = "Win Probability") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set3") +
    geom_text(aes(label = sprintf("%.2f%%", win_probability * 100)),
              vjust = -0.5, size = 3)
}

# Function to create ROC curve
plot_roc_curve <- function(actual, predicted) {
  roc_data <- roc(actual, predicted)
  
  plot(roc_data, main = "ROC Curve")
  text(0.75, 0.25, sprintf("AUC: %.3f", auc(roc_data)), adj = c(0, 1), font = 2)
}