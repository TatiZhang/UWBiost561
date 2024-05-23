#' Generate a random adjacency matrix with a partial clique
#'
#' @param n number of nodes, positive integer
#' @param clique_fraction fraction of nodes in the partial clique,
#' between 0 and 1
#' @param clique_edge_density edge density among the partial clique,
#' between 0 and 1
#'
#' @return description
#'
#' @export
generate_partial_clique <- function(n = 10,
                                    clique_fraction = 0.5,
                                    clique_edge_density = 0.5){
  adj_mat <- matrix(sample(c(0,1),
                           size = n^2,
                           replace = TRUE,
                           prob = c(0.9,0.1)),
                    nrow = n,
                    ncol = n)
  diag(adj_mat) <- 1
  m <- round(n*clique_fraction)
  idx <- 1:m
  combn_mat <- utils::combn(m, 2)
  col_idx <- sample(1:ncol(combn_mat), round(clique_edge_density*ncol(combn_mat)))
  combn_mat <- combn_mat[,col_idx,drop = FALSE]

  for(k in 1:ncol(combn_mat)){
    i <- combn_mat[1,k]
    j <- combn_mat[2,k]

    adj_mat[i,j] <- 1
    adj_mat[j,i] <- 1
  }

  adj_mat <- adj_mat + t(adj_mat)
  adj_mat[adj_mat != 0] <- 1

  return(list(adj_mat = adj_mat))
}
