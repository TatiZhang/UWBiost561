#' Generate a Random Graph with a Partial Clique
#'
#' @param n Integer, the number of nodes in the graph.
#' @param clique_fraction Numeric, the fraction of nodes that are part of the partial clique.
#' @param clique_edge_density Numeric, the density of edges within the partial clique.
#'
#' @return A list containing the adjacency matrix `adj_mat` which has the specified partial clique.
#'         The matrix is symmetric, with zeroes and ones, and ones on its diagonal.

#' @export

generate_partial_clique <- function(n, clique_fraction, clique_edge_density) {
  # Input validation
  stopifnot(is.numeric(n), n >= 1, n%%1 == 0,
            is.numeric(clique_fraction), clique_fraction >= 0, clique_fraction <= 1,
            is.numeric(clique_edge_density), clique_edge_density >= 0, clique_edge_density <= 1)

  # Initialize an empty adjacency matrix
  adj_mat <- matrix(0, nrow = n, ncol = n)
  diag(adj_mat) <- 1  # Set diagonal to 1

  # Determine the size of the partial clique
  clique_size <- round(n * clique_fraction)

  # Determine the number of edges in the partial clique based on the edge density
  total_possible_edges <- clique_size * (clique_size - 1) / 2
  num_edges_to_add <- round(clique_edge_density * total_possible_edges)

  # Generate the partial clique
  if (clique_size > 1) {
    # Select clique nodes
    clique_nodes <- sample(n, clique_size)

    # Randomly add edges between these nodes based on the specified edge density
    edges_possible <- combn(clique_nodes, 2, simplify = TRUE)
    edges_to_add <- sample(ncol(edges_possible), num_edges_to_add, replace = FALSE)
    selected_edges <- edges_possible[, edges_to_add]

    # Symmetrically add edges
    adj_mat[selected_edges[1, ], selected_edges[2, ]] <- 1
    adj_mat[selected_edges[2, ], selected_edges[1, ]] <- 1
  }

  # Return results in a list
  list(adj_mat = adj_mat)
}

# The function can be saved in a file named generate_partial_clique.R in the R folder of your UWBiost561 package.
