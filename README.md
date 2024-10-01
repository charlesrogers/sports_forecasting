# Sports Game Forecasting Program

This R program provides a way to input two teams playing in a given game and receive forecasts from multiple models simultaneously.

## Requirements

- R (version 4.0.0 or higher)
- Required packages: tidyverse, caret, pROC

## Installation

1. Clone this repository or download the files.
2. Install the required packages by running:

```R
install.packages(c("tidyverse", "caret", "pROC"))
```

## Usage

1. Ensure you have a CSV file named `historical_games.csv` in the `data/` directory with the following columns:
   - date
   - home_team
   - away_team
   - home_score
   - away_score

2. Run the main script:

```
Rscript main.R
```

3. Follow the prompts to input a new game.

4. The program will display evaluation metrics for all models, plot an ROC curve for the ensemble model, and show predictions for the new game.

## File Structure

- `R/`: Contains individual R scripts for data processing, model training, prediction, evaluation, and visualization.
- `data/`: Should contain your `historical_games.csv` file.
- `main.R`: The main script that runs the entire program.

## Customization

You can modify the individual models in `02_model_training.R` to use different algorithms or features as needed.