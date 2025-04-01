# Required packages
library(lme4)

###################### Midpoint CRXO Design Matrix #################################

Midpoint_CRXO_Desmat <- function(Ts, n_clust_per_seq) {
  # Ts: Number of time periods (must be even)
  # n_clust_per_seq: Number of clusters per sequence (must be integer)
  
  if (Ts %% 2 != 0) {
    stop("Number of time periods (Ts) must be even for a midpoint crossover.")
  }
  
  clusters <- 2 * n_clust_per_seq  # Total clusters
  CRXO <- matrix(nrow = clusters, ncol = Ts)
  
  # Define two midpoint crossover sequences
  seq1 <- c(rep(0, Ts / 2), rep(1, Ts / 2))  # Control → Treatment
  seq2 <- c(rep(1, Ts / 2), rep(0, Ts / 2))  # Treatment → Control
  
  # Assign clusters
  for (i in 1:n_clust_per_seq) {
    CRXO[i, ] <- seq1
  }
  for (i in (n_clust_per_seq + 1):clusters) {
    CRXO[i, ] <- seq2
  }
  
  return(CRXO)
}

# Example
Midpoint_CRXO_Desmat(4, 5)

###################### Multiple CRXO Design Matrix #################################

CRXO_Desmat <- function(Ts, n_clust_per_seq) {
  # Ts: Number of time periods
  # n_clust_per_seq: number of clusters per sequence
  # Total clusters = 2 sequences × n_clust_per_seq
  clusters <- 2 * n_clust_per_seq
  
  # Initialize design matrix
  CRXO <- matrix(nrow = clusters, ncol = Ts)
  
  # Define two alternating sequences
  seq1 <- rep(c(0, 1), length.out = Ts)
  seq2 <- rep(c(1, 0), length.out = Ts)
  
  # Assign first n_clust_per_seq clusters to seq1, the rest to seq2
  for (i in 1:n_clust_per_seq) {
    CRXO[i, ] <- seq1
  }
  for (i in (n_clust_per_seq + 1):clusters) {
    CRXO[i, ] <- seq2
  }
  
  return(CRXO)
}

#Example
CRXO_Desmat(Ts = 6, n_clust_per_seq = 3)



################################# Power Simulation #############################################

CRT_Power <- function(nrep, Design, Ts, n_clust_per_seq, m, Treat_effect, ICC, CAC){
  # nrep: number of replications
  # Design: 0 = Midpoint CRXO, 1 = Multiple CRXO
  # Ts: Number of time periods (must be even)
  # n_clust_per_seq: Number of clusters per sequence (must be integer)
  # m: number of observations per cluster per period
  # TimeEffsInd: 0 = shared time effects, 1 = separate time effects
  # Treat_effect: treatment effect
  # ICC: intra-cluster correlation
  # CAC: cluster autocorrelation
  
  # Generate the design matrix
  if (Design == 0) {
    # Multiple crossover CRXO
    DesMatrix <- Midpoint_CRXO_Desmat(Ts, n_clust_per_seq)
  } 
  else if (Design == 1) {
    # Midpoint crossover CRXO
    DesMatrix <- CRXO_Desmat(Ts, n_clust_per_seq)
  }
  
  # Shared time effects across clusters
  TimeEffects <- 0.1 * matrix(data = seq(1:ncol(DesMatrix)), 
                              nrow = nrow(DesMatrix), ncol = ncol(DesMatrix), byrow = TRUE) # 0.1 for period 1 represents the overall intercept in the model
  
  clusters <- 2 * n_clust_per_seq  # Total clusters
  
  # Model results from simulated data
  output <- replicate(nrep, CRT_Model(DesMatrix, clusters, m, TimeEffects, Treat_effect, ICC, CAC))

  Power <- NULL
  
  #Rejection percentage
  Power <-  sum(abs(output[1,])/output[2,]>1.96)/nrep    # Hypothesis test
  
  return(Power)
}

############################## Model Fitting ################################
CRT_Model <- function(DesMatrix, clusters, m, Teffs, Treat_effect, ICC, CAC){
  # Teffs: Time effects matrix
  
  #Simulate the data:
  simulated_dataset <- CRT_Dataset(DesMatrix, clusters, m, Teffs, Treat_effect, ICC, CAC)
  
  # Fit a model
  # Model results from simulated data
  if (CAC == 1) {
    fit <- lmer(Y ~ treat_vec + time_vec + (1|cluster_vec) , simulated_dataset)
  } 
  else if (CAC != 1) {
    fit <- lmer(Y ~ treat_vec + time_vec + (1|cluster_vec) + (1|cluster_vec:time_vec), simulated_dataset)
  }
  
  return(c(fixef(fit)[2], sqrt(vcov(fit)[2,2])))
}

# A function to generate one dataset from a longitudinal CRT with design matrix given by DesMatrix
CRT_Dataset <- function(DesMatrix, clusters, m, Teffs, Treat_effect, ICC, CAC){
  
  # Generate variances based on correlation structure
  # Assume total variance of 1 (for simplified interpretation)
  sigma_eps2 = 1 -ICC # error variance
  sigmaA2 = CAC*ICC # variance of cluster random effect
  sigmaG2 = ICC*(1-CAC) # variance of cluster-period random effects
  
  # n_period = total number of periods
  n_period = ncol(Teffs)  # number of period
  
  # Generate a vector for the design and a vector for the time effects
  fulldesmat <- DesMatrix[rep(1:nrow(DesMatrix), 1), ] # no sorting here
  treat_vec <- rep(as.vector(t(fulldesmat)), each = m)
  
  fulltimemat <- Teffs[rep(1:nrow(Teffs), 1), ]
  Time_eff <- rep(as.vector(t(fulltimemat)), each = m)
  
  # Error terms:
  epsi = rnorm(clusters * n_period * m, mean=0, sd=sqrt(sigma_eps2)) # epsilon (error term)
  
  # Cluster random effects:
  clus_rand_eff <- rnorm(clusters,mean=0, sd=sqrt(sigmaA2))
  clus_rand_effect <- rep(clus_rand_eff, each=n_period*m) # one for each participant in each cluster
  
  # For block exchangeable correlation structure, generating cluster by time random effect, when CAC = 1, it turns out to be 0 and becomes exchangeable correlation structure
  clus_time_rand_eff = rnorm(n_period*clusters, mean=0, sd=sqrt(sigmaG2))
  clus_time_rand_effect <- rep(clus_time_rand_eff, each=m)
  
  # Simulating outcomes:
  Y = Treat_effect*treat_vec + Time_eff + clus_rand_effect + clus_time_rand_effect + epsi # intercept value is from Time_eff : 0.1
  
  # Preparing remaining variables for the simulated dataset
  clusterVi <- rep(seq(1 : clusters), each = n_period * m)
  cluster_vec = factor(clusterVi)
  
  timeVi <- rep(seq(1:n_period), each=m)
  timeVi <- rep(timeVi, times=clusters)
  time_vec = factor(timeVi)
  
  full_data = data.frame(Y, cluster_vec, time_vec, treat_vec)
  
  return(full_data)  
}

set.seed(5248)
# Example of running the function
CRT_Power(nrep = 1000, Design = 0, Ts = 4, n_clust_per_seq = 20, m = 20, Treat_effect = 0.1, ICC = 0.05, CAC = 1)

