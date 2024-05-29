library(UWBiost561)
# Define the simulation parameters
levels_n <- c(10, 20, 30, 40, 50) # Different sizes of the graph
num_trials <- 5                   # Number of trials for each combination
alpha_vec <- c(0.5, 0.75, 0.95)   # Different edge densities for the partial clique
time_limit <- 30                  # Maximum time allowed for each implementation to run (in seconds)

# Run the simulation study
simulation_results <- run_simulation(levels_n, num_trials, alpha_vec, time_limit)

# Save the simulation results to an .RData file
save(simulation_results, file = "~/HW4_simulation.RData")

# Optional: print a summary of the results to console
print("Simulation study completed. Results have been saved to ~/HW4_simulation.RData.")
