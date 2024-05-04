context("Testing compute_maximal_partial_clique")


# Test that the output list has the required names and order
test_that("Output list has required names and correct order", {
  adj_mat <- matrix(sample(0:1, 25, replace = TRUE), 5, 5)
  diag(adj_mat) <- 1
  result <- compute_maximal_partial_clique(adj_mat, 0.6)
  expect_named(result, c("clique_idx", "edge_density"))
})

# Test that clique_idx contains no duplicates and is within bounds
test_that("clique_idx contains no duplicates and values are within bounds", {
  n <- 10
  adj_mat <- matrix(sample(0:1, n*n, replace = TRUE), n, n)
  diag(adj_mat) <- 1
  result <- compute_maximal_partial_clique(adj_mat, 0.5)
  expect_true(length(unique(result$clique_idx)) == length(result$clique_idx))
  expect_true(all(result$clique_idx >= 1 & result$clique_idx <= n))
})

# Test expected output structure
test_that("Output structure is correct", {
  adj_mat <- diag(10)
  result <- compute_maximal_partial_clique(adj_mat, 0.5)
  expect_is(result, "list")
  expect_named(result, c("clique_idx", "edge_density"))
})

# Test edge density calculation
test_that("Edge density calculation is accurate", {
  n <- 5
  adj_mat <- matrix(0, n, n)
  diag(adj_mat) <- 1
  adj_mat[1:3, 1:3] <- 1  # Create a fully connected subgraph of size 3
  result <- compute_maximal_partial_clique(adj_mat, 0.8)
  expected_density <- 1  # Fully connected
  expect_equal(result$edge_density, expected_density)
})

# Test recovery of inserted clique
test_that("Function can recover an inserted clique", {
  n <- 10
  adj_mat <- matrix(0, n, n)
  diag(adj_mat) <- 1
  adj_mat[1:5, 1:5] <- 1  # Create a fully connected subgraph of size 5
  result <- compute_maximal_partial_clique(adj_mat, 0.8)
  expect_true(all(result$clique_idx %in% 1:5))
  expect_equal(length(result$clique_idx), 5)
})
