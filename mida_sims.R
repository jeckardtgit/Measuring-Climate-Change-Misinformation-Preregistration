## Design Declaration and Power Analysis ##

# Here, we declare our design according to the MIDA Framework by Blair, Cooper, Coppock and Humphreys
# and estimate power across different sample sizes. 

# Loading packages

library(DeclareDesign) # Package for declaring designs following the MIDA approach described in the paper
library(tidyverse) # For data manipulation 

##### Population ####

N <- 6500
outcome_means <- c(5.7,3.7,3.6,2.7,7.5,3.7,7.7,4,4.8,3) # The expected means do not vary across inter-item correlation scenarios 
outcome_sds <- sims_sds_30 # This can be replaced with a vector SDs under any inter-item correlation scenario -> generated using the latent_sims_sd.R file

population <- declare_population(
  N = N, 
  Y = sample(0:20, N, replace = TRUE)  
)

#### Potential Outcomes ####

potential_outcomes <- declare_potential_outcomes(
  Y_EC_none_none = outcome_means[1L] + rnorm(N,0,outcome_sds[1L]),
  Y_EC_DK_none = outcome_means[2L] + rnorm(N,0,outcome_sds[2L]),
  Y_EC_none_CER = outcome_means[3L] + rnorm(N,0,outcome_sds[3L]),
  Y_EC_DK_CER = outcome_means[4L] + rnorm(N,0,outcome_sds[4L]),
  Y_Likert_none_none = outcome_means[5L] + rnorm(N,0,outcome_sds[5L]),
  Y_Likert_DK_none = outcome_means[6L] + rnorm(N,0,outcome_sds[6L]),
  Y_DDS_none_none = outcome_means[7L] + rnorm(N,0,outcome_sds[7L]),
  Y_DDS_DK_none = outcome_means[8L] + rnorm(N,0,outcome_sds[8L]),
  Y_DDS_none_CER = outcome_means[9L] + rnorm(N,0,outcome_sds[9L]),
  Y_DDS_DK_CER = outcome_means[10L] + rnorm(N,0,outcome_sds[10L]))

#### Reveal ####

reveal_Y <- declare_reveal(handler = function(data) {
  levels_T1 <- c("EC", "Likert", "DDS")
  levels_T2 <- c("none", "DK")  # T2: "none" or "DK"
  levels_T3 <- c("none", "CER")  # T3: "none" or "CER"

  potential_cols <- mapply(function(T1, T2, T3) paste(T1, T2, T3, sep = "_"),
                           data$T1, data$T2, data$T3)
  

  potential_cols <- paste("Y", potential_cols, sep = "_")
  
  data$Y <- NA_real_
  
  for (i in seq_along(potential_cols)) {
    col_name <- potential_cols[i]
    
    if (data$T1[i] == "Likert" && data$T3[i] == "CER") {
      next  # Skip this iteration
    }
    
    # Assign values to Y if the column exists
    if (col_name %in% colnames(data)) {
      data$Y[i] <- data[[col_name]][i]
    }
  }
  
  data <- data[!is.na(data$Y), ]
  
  return(data)
})


#### Inquiry ####


estimands <- declare_inquiry(
  
  # Coefficients used for linear hypothesis tests, defining for clarity
  
  beta_Likert_DK = mean( 
    (Y_Likert_DK_none - Y_EC_DK_none) - (Y_Likert_none_none - Y_EC_none_none)
  ),
  
  beta_DDS_DK = mean(
    (Y_DDS_DK_none - Y_EC_DK_none) - (Y_DDS_none_none - Y_EC_none_none)
  ),
  
  # DK:CER 
  
  beta_DK_CER = mean((Y_EC_DK_CER - Y_EC_none_CER) - (Y_EC_DK_none - Y_EC_none_none)),
  
  # DDS:CER #
  
  beta_DDS_CER = mean( 
    ( Y_DDS_none_CER - Y_EC_none_CER ) - ( Y_DDS_none_none - Y_EC_none_none)
  ),
  
  # DDS:DK:CER
  
  beta_DDS_DK_CER = mean(
    (Y_DDS_DK_CER - Y_DDS_DK_none - Y_DDS_none_CER + Y_DDS_none_none) -
      (Y_EC_DK_CER - Y_EC_DK_none - Y_EC_none_CER + Y_EC_none_none)
  ),
  
  # Hypothesis Tests Using Coefficients in Model #
  
  ### Test H1a ###
  
  `Likert vs. EC` = mean( 
    Y_Likert_none_none - Y_EC_none_none
  ),
  
  ### Test H1b ###
  
  `DDS vs. EC` = mean(Y_DDS_none_none  - Y_EC_none_none), 
  
  
  ### Test 1 H2a ###
  
  `EC DK vs. EC` = mean(Y_EC_DK_none - Y_EC_none_none), 
  
  
  ### Test 1 H2b ###
  
  `EC CER vs. EC` = mean( 
    Y_EC_none_CER  - Y_EC_none_none
  ),
  # Linear hypothesis tests #
  
  ### Test 2 H2a ###
  
  `Likert DK vs. Likert` = mean( 
    (Y_Likert_DK_none - Y_Likert_none_none) - (Y_EC_DK_none - Y_EC_none_none)
  ) + mean(Y_EC_DK_none - Y_EC_none_none), 
  
  ### Test 3 H2a ###
  
  `DDS DK vs. DDS` = mean( 
    (Y_DDS_DK_none - Y_DDS_none_none) - (Y_EC_DK_none - Y_EC_none_none) 
  ) + mean(Y_EC_DK_none - Y_EC_none_none),
  
  ### Test 2 H2b ###
  
  `DDS CER vs. DDS` = mean( 
    ( Y_DDS_none_CER - Y_EC_none_CER ) - ( Y_DDS_none_none - Y_EC_none_none)
  ) + mean( 
    Y_EC_none_CER  - Y_EC_none_none
  ),
  
  ### Test 1 H2c ###
  
  `EC DK CER vs. EC DK` = mean(
    (Y_EC_DK_CER - Y_EC_none_CER) -  (Y_EC_DK_none - Y_EC_none_none) 
    
  ) + mean( Y_EC_none_CER  - Y_EC_none_none),
  
  ### Test 2 H2c ###
  
  `EC DK CER vs. EC CER` =  mean(Y_EC_DK_none - Y_EC_none_none) + mean(
    (Y_EC_DK_CER - Y_EC_none_CER) -  (Y_EC_DK_none - Y_EC_none_none)),
  
  ### Test 3 H2c ###
  
  `DDS DK CER vs. DDS DK` = mean(Y_DDS_none_CER - Y_EC_none_CER) - mean(Y_DDS_none_none - Y_EC_none_none) + 
    mean(Y_EC_none_CER - Y_EC_none_none) + 
    mean((Y_EC_DK_CER - Y_EC_none_CER) - (Y_EC_DK_none - Y_EC_none_none) +
           mean(
             (Y_DDS_DK_CER - Y_DDS_DK_none - Y_DDS_none_CER + Y_DDS_none_none) -
               (Y_EC_DK_CER - Y_EC_DK_none - Y_EC_none_CER + Y_EC_none_none)
           )), 
  
  ### Test 4 H2c ###
  
  `DDS DK CER vs. DDS CER` = mean( 
    ( Y_DDS_DK_none - Y_EC_DK_none ) - ( Y_DDS_none_none - Y_EC_none_none)
  ) + mean(Y_EC_DK_none - Y_EC_none_none)  +
    mean((Y_EC_DK_CER - Y_EC_none_CER) - (Y_EC_DK_none - Y_EC_none_none) +
           mean(
             (Y_DDS_DK_CER - Y_DDS_DK_none - Y_DDS_none_CER + Y_DDS_none_none) -
               (Y_EC_DK_CER - Y_EC_DK_none - Y_EC_none_CER + Y_EC_none_none)
           ))
)


assignment_factors <- declare_assignment(Z = complete_ra(N, 
                                                         conditions = 1:10, prob_each = rep(1/10, each = 10)),
                                         Z_cond_prob = obtain_condition_probabilities(assignment = Z, conditions = 1:10,
                                                                                      prob_each = rep(1/10, each = 10)))

assignment <- declare_step(
  fabricate,
  T1 = case_when(
    Z %in% c(1L, 2L, 3L, 4L) ~ "EC",    
    Z %in% c(5L, 6L)         ~ "Likert", 
    Z %in% c(7L,8L,9L,10L)   ~ "DDS"   
  ),
  T2 = case_when(
    Z %in% c(2L, 4L, 6L, 8L, 10L) ~ "DK",  
    TRUE                          ~ "none" 
  ),
 
  T3 = case_when(
    T1 == "Likert"            ~ "none",    
    Z %in% c(3L, 4L, 9L, 10L)  ~ "CER",    
    TRUE                      ~ "none"
  )
)

#### Estimator ####

estimator <- declare_estimator(handler = label_estimator(function(data) {     
  data$T1 <- relevel(as.factor(data$T1), ref = "EC")   # Setting T1 reference   
  data$T2 <- relevel(as.factor(data$T2), ref = "none")    # Setting T2 reference   
  data$T3 <- relevel(as.factor(data$T3), ref = "none")   # Setting T3 reference         
  
  # Model
  
  mod <- lm_robust(formula = Y ~ T1 * T2 * T3, data = data, weights = 1 / data$Z_cond_prob)    
  
  # This set of coefficients will be used for all tests 
  
  coefs <- coef(mod)
  vcov_mat <- vcov(mod)  
  
  est_h2a_likert <- coefs["T1Likert:T2DK"] + coefs["T2DK"]
  
  se_h2a_likert <- sqrt(vcov_mat["T1Likert:T2DK", "T1Likert:T2DK"] +
                          vcov_mat["T2DK", "T2DK"] +
                          2 * vcov_mat["T1Likert:T2DK", "T2DK"])
  
  t_stat_h2a_likert <- est_h2a_likert / se_h2a_likert
  
  p_value_h2a_likert <- 2 * pt(abs(t_stat_h2a_likert), df.residual(mod), lower.tail = FALSE)
  
  # Creating this once for confidence interval calculation
  
  t_crit <- qt(1 - 0.05/2, 3240)  # Two-sided
  
  ci_lower_h2a_likert <- est_h2a_likert - t_crit * se_h2a_likert
  ci_upper_h2a_likert <- est_h2a_likert + t_crit * se_h2a_likert
  
  # DDS 
  
  est_h2a_dds <- coefs["T1DDS:T2DK"] + coefs["T2DK"]
  
  se_h2a_dds <- sqrt(vcov_mat["T1DDS:T2DK", "T1DDS:T2DK"] +
                       vcov_mat["T2DK", "T2DK"] +
                       2 * vcov_mat["T1DDS:T2DK", "T2DK"])
  
  t_stat_h2a_dds <- est_h2a_dds / se_h2a_dds
  
  p_value_h2a_dds <- 2 * pt(abs(t_stat_h2a_dds), df.residual(mod), lower.tail = FALSE)
  
  ci_lower_h2a_dds <- est_h2a_dds - t_crit * se_h2a_dds
  ci_upper_h2a_dds <- est_h2a_dds + t_crit * se_h2a_dds
  
  # H2b 
  
  # EC
  
  # Confirmed if CER is negative, no additional test needed
  
  # DDS
  
  est_h2b_dds <- coefs["T1DDS:T3CER"] + coefs["T3CER"]
  
  se_h2b_dds <- sqrt(vcov_mat["T1DDS:T3CER", "T1DDS:T3CER"] +
                       vcov_mat["T3CER", "T3CER"] +
                       2 * vcov_mat["T1DDS:T3CER", "T3CER"])
  
  t_stat_h2b_dds <- est_h2b_dds / se_h2b_dds
  
  p_value_h2b_dds <- 2 * pt(abs(t_stat_h2b_dds), df.residual(mod), lower.tail = FALSE)
  
  ci_lower_h2b_dds <- est_h2b_dds - t_crit * se_h2b_dds
  ci_upper_h2b_dds <- est_h2b_dds + t_crit * se_h2b_dds
  
  # H2c
  
  # EC DK versus EC DK CER 
  
  est_h2c_ec_dk <- coefs["T3CER"] + coefs["T2DK:T3CER"]
  
  se_h2c_ec_dk <- sqrt(
    vcov_mat["T3CER", "T3CER"] +
      vcov_mat["T2DK:T3CER", "T2DK:T3CER"] +
      2 * vcov_mat["T3CER", "T2DK:T3CER"]
  )
  
  t_stat_h2c_ec_dk <- est_h2c_ec_dk / se_h2c_ec_dk
  
  p_value_h2c_ec_dk <- 2 * pt(abs(t_stat_h2c_ec_dk), df.residual(mod), lower.tail = FALSE)
  
  ci_lower_h2c_ec_dk <- est_h2c_ec_dk - t_crit * se_h2c_ec_dk
  ci_upper_h2c_ec_dk <- est_h2c_ec_dk + t_crit * se_h2c_ec_dk
  
  
  
  # EC CER versus EC DK CER 
  
  est_h2c_ec_cer <- coefs["T2DK"] + coefs["T2DK:T3CER"]
  
  
  se_h2c_ec_cer <- sqrt(
    vcov_mat["T2DK", "T2DK"] +
      vcov_mat["T2DK:T3CER", "T2DK:T3CER"] +
      2 * vcov_mat["T2DK", "T2DK:T3CER"]
  )
  
  t_stat_h2c_ec_cer<- est_h2c_ec_cer / se_h2c_ec_cer
  
  p_value_h2c_ec_cer <- 2 * pt(abs(t_stat_h2c_ec_cer), df.residual(mod), lower.tail = FALSE)
  
  ci_lower_h2c_ec_cer <- est_h2c_ec_cer - t_crit * se_h2c_ec_cer
  ci_upper_h2c_ec_cer <- est_h2c_ec_cer + t_crit * se_h2c_ec_cer
  
  # DDS DK versus DDS DK CER 
  
  est_h2c_dds_dk <- (coefs["T1DDS:T3CER"] + 
                       coefs["T1DDS:T2DK:T3CER"] +       
                       coefs["T3CER"] + coefs["T2DK:T3CER"])
  
  se_h2c_dds_dk <- sqrt(vcov_mat["T1DDS:T3CER", "T1DDS:T3CER"] +
                          vcov_mat["T3CER", "T3CER"] +
                          vcov_mat["T1DDS:T2DK:T3CER", "T1DDS:T2DK:T3CER"] +
                          vcov_mat["T2DK:T3CER", "T2DK:T3CER"] +
                          2 * vcov_mat["T1DDS:T3CER", "T3CER"] +
                          2 * vcov_mat["T1DDS:T3CER", "T1DDS:T2DK:T3CER"] +
                          2 * vcov_mat["T1DDS:T3CER", "T2DK:T3CER"] +
                          2 * vcov_mat["T3CER", "T1DDS:T2DK:T3CER"] +
                          2 * vcov_mat["T3CER", "T2DK:T3CER"] +
                          2 * vcov_mat["T1DDS:T2DK:T3CER", "T2DK:T3CER"])
  
  t_stat_h2c_dds_dk  <- est_h2c_dds_dk  / se_h2c_dds_dk 
  
  p_value_h2c_dds_dk <- 2 * pt(abs(t_stat_h2c_dds_dk), df.residual(mod), lower.tail = FALSE)
  
  ci_lower_h2c_dds_dk <- est_h2c_dds_dk - t_crit * se_h2c_dds_dk
  ci_upper_h2c_dds_dk <- est_h2c_dds_dk + t_crit * se_h2c_dds_dk
  
  
  # DDS CER versus DDS DK CER 
  
  est_h2c_dds_cer <- (coefs["T1DDS:T2DK"] + 
                        coefs["T1DDS:T2DK:T3CER"] +       
                        coefs["T2DK"] + coefs["T2DK:T3CER"])
  
  se_h2c_dds_cer <- sqrt(vcov_mat["T1DDS:T2DK", "T1DDS:T2DK"] +
                           vcov_mat["T1DDS:T2DK:T3CER", "T1DDS:T2DK:T3CER"] +
                           vcov_mat["T2DK", "T2DK"] +
                           vcov_mat["T2DK:T3CER", "T2DK:T3CER"] +
                           2 * vcov_mat["T1DDS:T2DK", "T1DDS:T2DK:T3CER"] +
                           2 * vcov_mat["T1DDS:T2DK", "T2DK"] +
                           2 * vcov_mat["T1DDS:T2DK", "T2DK:T3CER"] +
                           2 * vcov_mat["T1DDS:T2DK:T3CER", "T2DK"] +
                           2 * vcov_mat["T1DDS:T2DK:T3CER", "T2DK:T3CER"] +
                           2 * vcov_mat["T2DK", "T2DK:T3CER"])
  
  
  t_stat_h2c_dds_cer <- est_h2c_dds_cer / se_h2c_dds_cer
  
  p_value_h2c_dds_cer <- 2 * pt(abs(t_stat_h2c_dds_cer), df.residual(mod), lower.tail = FALSE)
  
  ci_lower_h2c_dds_cer <- est_h2c_dds_cer - t_crit * se_h2c_dds_cer
  ci_upper_h2c_dds_cer <- est_h2c_dds_cer + t_crit * se_h2c_dds_cer
  
  # Combining all these test results 
  
  results_contrast_tests <- data.frame(
    term_or_comparison = c("Likert DK vs. Likert", "DDS DK vs. DDS", "DDS CER vs. DDS", 
                           "EC DK CER vs. EC DK", "EC DK CER vs. EC CER", "DDS DK CER vs. DDS DK", "DDS DK CER vs. DDS CER"),
    estimate = c(est_h2a_likert, est_h2a_dds, est_h2b_dds,
                 est_h2c_ec_dk, est_h2c_ec_cer, est_h2c_dds_dk, est_h2c_dds_cer),
    std.error = c(se_h2a_likert, se_h2a_dds, se_h2b_dds,
                  se_h2c_ec_dk, se_h2c_ec_cer, se_h2c_dds_dk, se_h2c_dds_cer),
    statistic = c(t_stat_h2a_likert, t_stat_h2a_dds, t_stat_h2b_dds,
                  t_stat_h2c_ec_dk, t_stat_h2c_ec_cer, t_stat_h2c_dds_dk, t_stat_h2c_dds_cer),
    p.value =  c(p_value_h2a_likert, p_value_h2a_dds, p_value_h2b_dds,
                 p_value_h2c_ec_dk, p_value_h2c_ec_cer, p_value_h2c_dds_dk, p_value_h2c_dds_cer),
    conf.low = c(ci_lower_h2a_likert, ci_lower_h2a_dds, ci_lower_h2b_dds,
                 ci_lower_h2c_ec_dk, ci_lower_h2c_ec_cer, ci_lower_h2c_dds_dk, ci_lower_h2c_dds_cer),
    conf.high = c(ci_upper_h2a_likert, ci_upper_h2a_dds, ci_upper_h2b_dds,
                  ci_upper_h2c_ec_dk, ci_upper_h2c_ec_cer, ci_upper_h2c_dds_dk, ci_upper_h2c_dds_cer), 
    df = 3240, 
    hyp_tested = c("H2a", "H2a", "H2b", "H2c", "H2c", "H2c", "H2c"), 
    outcome = c("Y"), 
    inquiry = c("Likert DK vs. Likert", "DDS DK vs. DDS", "DDS CER vs. DDS", 
                "EC DK CER vs. EC DK", "EC DK CER vs. EC CER", "DDS DK CER vs. DDS DK", "DDS DK CER vs. DDS CER")
  ) 
  
  # Now extracting the relevant model coefficients... 
  
  mod_coefs <- broom::tidy(mod) %>%
    mutate(df = 3240, hyp_tested = case_when(term =="T1Likert" ~ "H1a", 
                                             term == "T1DDS" ~ "H1b", 
                                             term == "T2DK"~ "H2a", 
                                             term == "T3CER" ~ "H2b", 
                                             TRUE ~ "No hypothesis formulated"),
           inquiry = case_when(term == "T1Likert" ~ "Likert vs. EC", 
                               term == "T1DDS" ~ "DDS vs. EC", 
                               term == "T2DK" ~ "EC DK vs. EC", 
                               term == "T3CER" ~ "EC CER vs. EC", 
                               term == "T1Likert:T2DK" ~  "beta_Likert_DK", 
                               term == "T1DDS:T2DK" ~  "beta_DDS_DK",
                               term == "T2DK:T3CER" ~ "beta_DK_CER",
                               term == "T1DDS:T3CER" ~  "beta_DDS_CER",
                               term == "T1DDS:T2DK:T3CER" ~ "beta_DDS_DK_CER")) %>% 
    rename(term_or_comparison = term) 
  
  
  
  # Now combining both 
  
  res_combined <- bind_rows(mod_coefs, results_contrast_tests) %>%
    filter(!is.na(inquiry)) %>%
    mutate(
      num_hypotheses = sum(hyp_tested != "No hypothesis formulated", na.rm = TRUE),
      
      p_bonferroni = ifelse(
        hyp_tested != "No hypothesis formulated", 
        pmin(p.value * num_hypotheses, 1), 
        NA
      ),
      
      p_holm = ifelse(
        hyp_tested != "No hypothesis formulated", 
        p.adjust(p.value, method = "holm"), 
        NA
      )
    ) %>%
    dplyr::select(-num_hypotheses)  
  
  
  return(res_combined)
  
}))

design <- population + potential_outcomes + assignment_factors + 
  assignment + reveal_Y + estimands + estimator

set.seed(1234)

res <- run_design(design)

res %>% # For correct order according to hypotheses
  mutate(
    hyp_tested = factor(hyp_tested, levels = c("No hypothesis formulated", "H1a", "H1b", "H2a", "H2b", "H2c"))
  ) %>%
  arrange(
    ifelse(inquiry == "beta_Likert_DK", 13,
           ifelse(inquiry == "beta_DDS_DK", 14,
                  ifelse(inquiry == "beta_DK_CER", 15,
                         ifelse(inquiry == "beta_DDS_CER", 16,
                                ifelse(inquiry == "beta_DDS_DK_CER", 17,
                                       hyp_tested)))))
  )

#### Diagnosis ####

diagnosands <- declare_diagnosands(
  power = mean(p.value < 0.05),
  bonferroni_power = mean(p.value * 11 < 0.05), 
  holm_power = mean(p_holm < 0.05, na.rm = TRUE)
)

(diagnosis <- diagnose_design(design, 
                              diagnosands = diagnosands)) 

set.seed(1234)

diagnosis <- 
  design |>
  redesign(N = seq(1300, 6500, 1300)) |>
  diagnose_design(diagnosands = diagnosands)

dta <- diagnosis$diagnosands_df
dta <- na.omit(dta) 

# Get power for different sample sizes in dta object by selecting design for the desired sample size, 
# holm_power for power using Holm-corrected p-values and inquiry for the respective test, 
# e.g.:

# dta %>% select(design, inquiry, holm_power)





