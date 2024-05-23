library(testthat)
library(UWBiost561)
# Define a helper function to check if all required columns are present in the results
check_required_columns <- function(results_df) {
  required_columns <- c("n", "density", "trial", "method", "status", "clique_size", "edge_density", "valid", "time_elapsed")
  all(required_columns %in% colnames(results_df))
}

# Unit test 1: Basic functionality test with minimal input
test_that("Basic functionality test1", {
  levels_n <- c(5)
  num_trials <- 1
  alpha_vec <- c(0.5)

  result <- run_simulation(levels_n, num_trials, alpha_vec, time_limit = 1)

  # Check that the result is a nested list
  expect_type(result, "list")
})

test_that("Basic functionality test2", {
  levels_n <- c(5)
  num_trials <- 1
  alpha_vec <- c(0.5)

  result <- run_simulation(levels_n, num_trials, alpha_vec, time_limit = 1)
  expect_true(is.list(result[[1]]))
  expect_true(is.list(result[[1]][[1]]))
  expect_true(is.list(result[[1]][[1]][[1]]))
})

test_that("Handling different graph sizes", {
  levels_n <- c(5, 7, 10)
  num_trials <- 1
  alpha_vec <- c(0.5)

  result <- run_simulation(levels_n, num_trials, alpha_vec, time_limit = 1)

  # Check that the result is a nested list
  expect_type(result, "list")

  # Check that the result has the required structure
  expect_true(is.list(result[[1]]))
  expect_true(is.list(result[[1]][[1]]))
  expect_true(is.list(result[[1]][[1]][[1]]))
})

#Verify result structure and content
test_that("Verify result structure and content", {
  levels_n <- c(5)
  num_trials <- 1
  alpha_vec <- c(0.5)

  result <- run_simulation(levels_n, num_trials, alpha_vec, time_limit = 1)

  # Check that the result is a nested list
  expect_type(result, "list")

  # Check that the result has the required structure
  expect_true(is.list(result[[1]]))
  expect_true(is.list(result[[1]][[1]]))
  expect_true(is.list(result[[1]][[1]][[1]]))
})
