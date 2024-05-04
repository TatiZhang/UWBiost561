context("Testing generate_partial_clique")

test_that("generate_partial_clique works", {
  set.seed(10)
  res <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9)

  expect_true(is.list(res))
  expect_true(is.matrix(res$adj_mat))
  expect_true(all(dim(res$adj_mat) == c(10,10)))
})


# Test the stability of generated clique across multiple runs
test_that("Generated clique is stable with fixed seed", {
  set.seed(100)
  result1 <- generate_partial_clique(n = 10, clique_fraction = 0.3, clique_edge_density = 0.9)
  set.seed(100)
  result2 <- generate_partial_clique(n = 10, clique_fraction = 0.3, clique_edge_density = 0.9)
  expect_equal(result1$adj_mat, result2$adj_mat)
})

