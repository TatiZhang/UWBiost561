#' Run Simulation Study for Maximal Partial Clique Implementations
#'
#' This function runs a simulation study to evaluate the performance of 25 different implementations of the compute_maximal_partial_clique function.
#'
#' @param levels_n A vector of integers representing different sizes of the graph (number of nodes).
#' @param num_trials An integer representing the number of trials to run for each combination of graph size and edge density.
#' @param alpha_vec A vector of numeric values representing different edge densities for the partial clique.
#' @param time_limit A numeric value representing the maximum time allowed for each implementation to run (in seconds). Default is 15.
#'
#' @return A list containing the results of the simulation study (`level_trial_list`), the alpha values used (`alpha_vec`), the date of the run (`date_of_run`), and the session information (`session_info`).
#'
#' @details The function generates random adjacency matrices using the generate_partial_clique function, runs each of the 25 implementations of compute_maximal_partial_clique on these matrices, and stores the results.
#'
#' @import UWBiost561
#' @export
run_simulation <- function(levels_n, num_trials, alpha_vec, time_limit = 15) {
  imp_numbers <- 1:25

  level_trial_list <- lapply(alpha_vec, function(alpha) {
    print(paste("Value of alpha:", alpha))

    trial_list <- lapply(1:num_trials, function(trial) {
      print(paste("Working on trial:", trial))
      set.seed(trial) # to freeze the randomness of adj_mat

      n_results <- lapply(levels_n, function(n) {
        # generate the data
        data <- generate_partial_clique(n = n, clique_fraction = 0.5, clique_edge_density = 0.5)
        adj_mat <- data$adj_mat

        result_list <- lapply(imp_numbers, function(imp_number) {
          set.seed(trial) # to freeze the randomness of the method
          cat('*')
          result <- tryCatch({
            UWBiost561::compute_maximal_partial_clique_master(
              adj_mat = adj_mat,
              alpha = alpha,
              number = imp_number,
              time_limit = time_limit
            )
          }, error = function(e) {
            list(status = "error", clique_idx = integer(), edge_density = NA, time_elapsed = NA, message = e$message)
          })

          if (is.list(result) && !is.null(result$clique_idx)) {
            actual_density <- calculate_edge_density(result$clique_idx, adj_mat)
            is_valid_clique <- actual_density >= alpha
            result$valid <- is_valid_clique
            result$edge_density <- actual_density
          } else {
            result$valid <- FALSE
            result$edge_density <- NA
          }
          if (is.null(result$time_elapsed)) result$time_elapsed <- NA
          return(result)
        })

        names(result_list) <- paste("Implementation:", imp_numbers)
        return(result_list)
      })

      names(n_results) <- paste("n:", levels_n)
      return(n_results)
    })

    names(trial_list) <- paste("Trial:", 1:num_trials)
    print("====")

    return(trial_list)
  })

  names(level_trial_list) <- paste0("alpha:", alpha_vec)

  # Save the results along with session info and run date
  date_of_run <- Sys.time()
  session_info <- devtools::session_info()

  save(level_trial_list, alpha_vec, date_of_run, session_info,
       file = "simulation_results.RData")

  return(list(level_trial_list = level_trial_list,
              alpha_vec = alpha_vec,
              date_of_run = date_of_run,
              session_info = session_info))
}

# Helper function to calculate edge density
calculate_edge_density <- function(clique_indices, adj_mat) {
  num_nodes <- length(clique_indices)
  if (num_nodes <= 1) return(1)  # Single-node case returns density of 1
  subgraph <- adj_mat[clique_indices, clique_indices]
  num_edges <- (sum(subgraph) - num_nodes) / 2  # Exclude self-loops
  max_edges <- num_nodes * (num_nodes - 1) / 2
  return(num_edges / max_edges)
}

# Example usage
levels_n <- c(10, 20, 30, 40, 50)
num_trials <- 5
alpha_vec <- c(0.5, 0.75, 0.95)
time_limit <- 30

# Run the simulation study
simulation_results <- run_simulation(levels_n, num_trials, alpha_vec, time_limit)

# Print a summary of the results
str(simulation_results)
