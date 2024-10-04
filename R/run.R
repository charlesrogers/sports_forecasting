
library(testthat)
test_file("tests/test_models.R")
source("main.R")
quarto::quarto_render("index.qmd")
source("R/07_backtesting.R")