#' Generate a random adjacency matrix with a large partial clique.
#'
#' This function generates a random adjacency matrix with a large partial clique,
#' based on the specified parameters.
#'
#' @param n The number of nodes in the graph.
#' @param clique_fraction The fraction of nodes that are part of the partial clique.
#'   Must be a single numeric between 0 and 1 (inclusive).
#' @param clique_edge_density The edge density among the nodes in the partial clique.
#'   Must be a single numeric between 0 and 1 (inclusive).
#' @param ... Additional arguments (currently unused).
#'
#' @return A list with the following components:
#'    \describe{
#'     \item{adj_mat}{The random adjacency matrix constructed with the partial clique.
#'     - A symmetric matrix with only values 0 or 1.
#'     - 1’s along its diagonal.
#'     - No row- or column-names.}
#'   }
#' @export

generate_partial_clique <- function(n, clique_fraction, clique_edge_density, ...) {

  # Check if n is a positive integer
  if (n%%1!=0 || n <= 0) {
    stop("Argument 'n' must be a positive integer.")
  }

  # Check if clique_fraction is a single numeric between 0 and 1
  if (!is.numeric(clique_fraction) || length(clique_fraction) != 1 || clique_fraction < 0 || clique_fraction > 1) {
    stop("Argument 'clique_fraction' must be a single numeric between 0 and 1.")
  }

  # Check if clique_edge_density is a single numeric between 0 and 1
  if (!is.numeric(clique_edge_density) || length(clique_edge_density) != 1 || clique_edge_density < 0 || clique_edge_density > 1) {
    stop("Argument 'clique_edge_density' must be a single numeric between 0 and 1.")
  }

  # Calculate the number of nodes in the partial clique
  m <- round(n * clique_fraction)

  # Initialize adjacency matrix for the partial clique
  adj_mat <- matrix(0, nrow = n, ncol = n)

  # Generate all possible edge pairs within the partial clique
  edge_pairs <- combn(1:m, 2)

  # Calculate the number of edges in the partial clique
  num_edges <- round(clique_edge_density * m * (m - 1) / 2)

  # Randomly select indices for edges within the partial clique
  selected_indices <- sample(ncol(edge_pairs), num_edges)

  # Set edges within the partial clique
  for (i in selected_indices) {
    adj_mat[edge_pairs[1, i], edge_pairs[2, i]] <- 1
    adj_mat[edge_pairs[2, i], edge_pairs[1, i]] <- 1
  }

  # Set diagonal elements to 1
  diag(adj_mat) <- 1

  # Return the adjacency matrix as a list
  return(list(adj_mat = adj_mat))
}
