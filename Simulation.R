##############################################################
# Simulation at different settings
##############################################################

# Define parameter values
design = c(0,1) # 0: Midpoint CRXO, 1: Multiple CRXO
ts <- c(4,6,8)  # Number of time periods
clusters_per_seq <- c(20)  # Number of clusters in each sequence
m <- c(20)  # Number of participants in each cluster in each period
icc <- c(0.01, 0.05, 0.1)  # Intra-cluster correlation
cac <- c(0.5, 0.8, 0.95, 1)  # Cluster autocorrelation
Treatment_effect <- c(0.1)  # Treatment effect

nrep <- 1000

# Generate all possible parameter combinations
simulation_params <- expand.grid(
  Design = design,
  Time = ts, 
  K = clusters_per_seq, 
  m = m, 
  Effect = Treatment_effect,
  ICC = icc, 
  CAC = cac
)

num_output_metrics <- 1  # Adjust this based on the actual number of outputs from "CRT_Power" function
simulation_power <- matrix(NA, nrow = nrow(simulation_params), ncol = num_output_metrics) 


# Loop through each parameter combination and run the simulation
set.seed(2208)
for (i in 1:nrow(simulation_params)) {
  
  # Extract parameters from the current row of simulation_params
  params <- simulation_params[i, ]
  
  # Tracking the simulation
  cat("Running simulation", i, "of", nrow(simulation_params), "\n")
  cat("  Params: ", paste(names(simulation_params), simulation_params[i, ], sep = " = ", collapse = ", "), "\n")
  
  # Tracking time for each simulation
  iter_start <- Sys.time()
  
  # Run the simulation function with extracted parameters
  simulation_power[i, ] <- CRT_Power(nrep = nrep, Design = params$Design, Ts = params$Time, n_clust_per_seq = params$K, 
                                     m = params$m, Treat_effect = params$Effect, ICC = params$ICC, CAC = params$CAC)
  
  iter_end <- Sys.time()
  cat("Duration of last iteration:", round(difftime(iter_end, iter_start, units = "secs"), 2), "seconds\n\n")
}

# Convert results to a data frame and merge with parameter values
simulation_results <- cbind(simulation_params, as.data.frame(simulation_power))

# Assign column names for results
colnames(simulation_results)[(ncol(simulation_params) + 1):ncol(simulation_results)] <- 
  c("Power")

simulation_results

write.csv(simulation_results, file = "simulation_results.csv", row.names = FALSE)

