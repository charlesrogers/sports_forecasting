# run_tests.R

library(testthat)

# Get the current script's directory
current_dir <- getwd()

# Set the working directory to the project root (assuming run_tests.R is in the project root)
setwd(current_dir)

# Source necessary files
source("logging.R")

# Check if the 'tests' directory exists
if (!dir.exists("tests")) {
  stop("'tests' directory not found. Please ensure it exists in the project root.")
}

# Run all test files
test_dir("tests", reporter = "summary")

