## Simulating SDs for Power Analyses ##

# This is the code used to simulate the standard deviations assuming different levels 
# of inter-item correlation. The resulting SDs are then stored as the outcome_sds 
# object in the mida_sims.R file used for the power analyses.  

# Loading packages 

library(MASS) # Needed for mvrnorm
library(dplyr) # For data manipulation 

# Defining means for each condition 

condition_means <- c(
  EC = 5.7, EC_DK = 3.7, EC_CER = 3.6, EC_DK_CER = 2.7,
  Likert = 7.5, Likert_DK = 3.7, DDS = 7.7, DDS_DK = 4,
  DDS_CER = 4.8, DDS_DK_CER = 3
)

rhos <- c(0.15, 0.3, 0.45) #  Values for inter-item correlations can be inserted here
n_items <- 20 # 20-item scale 
N <- 6500

# Main function 

simulate_sd <- function(mean_score, rho, n_items, N) {
  p <- mean_score / n_items
  mu <- qnorm(p)
  Sigma <- matrix(rho, n_items, n_items)
  diag(Sigma) <- 1
  errors <- mvrnorm(N, mu = rep(0, n_items), Sigma = Sigma)
  X <- sweep(errors, 2, mu, "+")
  binary <- (X > 0) * 1
  total_scores <- rowSums(binary)
  return(sd(total_scores))
}

# Running simulations using the function 

set.seed(1234)
results_list <- list()

for (grp in names(condition_means)) {
  for (rho in rhos) {
    sds <- replicate(1000, simulate_sd(condition_means[[grp]], rho, n_items, N))
    
    # Calculating quantiles for SDs
    
    quantiles_sd <- quantile(sds, c(.25, .5, .75))
    
    results_list[[length(results_list) + 1]] <- data.frame(
      group = grp,
      rho = rho,
      mean_score = condition_means[[grp]],
      q25_sd = quantiles_sd[1],  # 25th percentile
      q50_sd = quantiles_sd[2],  # Median (50th percentile)
      q75_sd = quantiles_sd[3]   # 75th percentile
    )
  }
}

# Binding into dataframe 

results_df <- bind_rows(results_list)

# Adding estimated alpha values reported in the paper 

results_df <- results_df %>%
  mutate(alpha = (n_items * rho) / (1 + (n_items - 1) * rho))

# Saving median value for simulated SDs. These are the exact vectors used in the mida_sims.R file 

sims_sds_15 <- results_df[results_df$rho == 0.15, "q50_sd"]
sims_sds_30 <- results_df[results_df$rho == 0.30, "q50_sd"]
sims_sds_45 <- results_df[results_df$rho == 0.45, "q50_sd"]

# Check SDs for different scenarios like this:

# e.g., for rho == 15: 
# results_df %>% filter(rho == 0.15) %>% 
#  dplyr::select(group, mean_score, q50_sd, alpha)



