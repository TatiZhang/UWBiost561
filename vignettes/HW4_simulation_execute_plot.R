# Please run this after running hw4-demo_bayes_execute.slurm
# Clear the environment
rm(list=ls())

# Load the saved simulation results
load("~/HW4_simulation.RData")

# Extract the required objects from the loaded results
level_trial_list <- simulation_results$level_trial_list
alpha_vec <- simulation_results$alpha_vec

# Validate the loaded data
if (is.null(level_trial_list)) {
  stop("Error: level_trial_list is NULL")
} else {
  print("Data loaded successfully")
  print(head(level_trial_list))
}

# Plotting
library(ggplot2)
library(cowplot)

# Print the structure of the input data
print(str(level_trial_list))
print(str(alpha_vec))

# Loop over the levels
ggplot_list <- lapply(1:length(level_trial_list), function(level_number){
  level_name <- names(level_trial_list)[level_number]
  trial_list <- level_trial_list[[level_number]]
  alpha <- alpha_vec[level_number]

  print(paste("Processing alpha:", alpha, "with", length(trial_list), "trials"))

  # First tabulate the results across trials
  trial_mat <- sapply(trial_list, function(trial){
    # Get the results for different 'n' values in the trial
    result_list <- trial[[1]]
    n_values <- names(result_list)

    print(paste("Alpha:", alpha, "- Result List:", str(result_list)))

    if (is.null(result_list) || length(result_list) == 0) {
      print(paste("Alpha:", alpha, "- Result List is NULL or empty"))
      return(rep(FALSE, 25))  # Assuming there are 25 methods
    }

    # Enumerate only the clique indices of each method
    clique_list <- lapply(result_list, function(x){x$clique_idx})
    print(paste("Alpha:", alpha, "- Clique List:", str(clique_list)))

    # Check which implementations had a valid partial clique
    bool_vec <- sapply(result_list, function(x) { if (is.null(x$valid)) FALSE else x$valid })
    print(paste("Alpha:", alpha, "- Bool Vec:", bool_vec))

    # Find which implementations had the largest clique
    valid_idx <- which(bool_vec == TRUE)
    print(paste("Alpha:", alpha, "- Valid Index:", valid_idx))

    clique_list_valid <- list()
    size_vec_valid <- numeric(0)

    if (length(valid_idx) > 0) {
      clique_list_valid <- clique_list[valid_idx]

      # Ensure all elements in clique_list_valid are numeric vectors
      clique_list_valid <- Filter(function(x) is.numeric(x) && length(x) > 0, clique_list_valid)
      print(paste("Alpha:", alpha, "- Valid Clique List:", str(clique_list_valid)))

      size_vec_valid <- sapply(clique_list_valid, function(clique) length(clique))
    }

    print(paste("Alpha:", alpha, "- Valid cliques sizes:", size_vec_valid))

    if(length(size_vec_valid) == 0) {
      max_size <- 0
    } else {
      max_size <- max(size_vec_valid)
    }

    winning_methods <- names(clique_list_valid)[which(size_vec_valid == max_size)]
    print(paste("Alpha:", alpha, "- Winning Methods:", winning_methods))

    # Prepare the tabulation for this trial
    winner_vec <- rep(FALSE, length(result_list))
    names(winner_vec) <- names(result_list)
    if (length(winning_methods) > 0) {
      winner_vec[winning_methods] <- TRUE
    }

    return(winner_vec)
  })

  if (is.null(trial_mat) || length(trial_mat) == 0) {
    print(paste("Alpha:", alpha, "- No valid trials found"))
    tabulate_vec <- rep(0, 25)
  } else {
    tabulate_vec <- rowSums(trial_mat)
  }

  print(paste("Alpha:", alpha, "- Tabulate Vec:", tabulate_vec))

  method_names <- as.character(1:25)
  df <- data.frame(method = method_names,
                   number_wins = tabulate_vec)

  gg <- ggplot2::ggplot(df, ggplot2::aes(x=method, y=number_wins))
  gg <- gg + ggplot2::geom_bar(stat = "identity")
  gg <- gg + ggplot2::scale_x_discrete(limits = method_names)
  gg <- gg + ggplot2::labs(x = "Method",
                           y = "Number of wins",
                           title = paste("For alpha =", alpha))
  return(gg)
})

plot_all <- cowplot::plot_grid(plotlist = ggplot_list, ncol = 1)

# Save the plot to a file
ggplot2::ggsave(plot_all, file = "~/UWBiost561/vignettes/HW4_simulation_execute_plot.png",
                height = 7, width = 9, units = "in")
