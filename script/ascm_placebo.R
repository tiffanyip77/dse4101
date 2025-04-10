# This is a placebo test for the Augmented Synthentic Control method
# Ensure that you have run ascm.R before running this script

# Create a directory for placebo tests
dir.create("ascm_output/placebo", showWarnings = FALSE)

#############################################
### Placebo-in-Time Test                 ###
#############################################
cat("Running placebo-in-time test...\n")

# Shift treatment timing backward by 5 years 
# If a state was treated in 1970, we'll pretend it was treated in 1965
placebo_time_data <- raw_data %>%
  filter(!(state_name %in% states_to_remove)) %>%
  mutate(
    real_treatment_year = ifelse(lfdivlaw < 2000, lfdivlaw, Inf),
    # Create placebo treatment 5 years before actual treatment
    placebo_treatment_year = ifelse(real_treatment_year < Inf, 
                                    real_treatment_year - 5, 
                                    Inf),
    # Limit to pre-treatment periods only
    placebo_treated = ifelse(placebo_treatment_year < Inf & 
                               year >= placebo_treatment_year & 
                               year < real_treatment_year,
                             1, 0)
  )

# Run ascm with placebo treatment timing
placebo_time_model <- multisynth(
  div_rate ~ placebo_treated,
  state_name,
  year,
  placebo_time_data,
  n.leads = 10,  # Fewer leads since we're only looking at pre-treatment periods
  n.lags = 5
)

# Examine and save results
placebo_time_summary <- summary(placebo_time_model)
print(placebo_time_summary)

# Plot placebo-in-time effects
placebo_time_plot <- plot(placebo_time_model, levels = "Average")
print(placebo_time_plot)
ggsave("ascm_output/placebo/placebo_in_time.png", placebo_time_plot, width = 10, height = 8)

# Calculate average placebo effect (should be close to zero if parallel trends holds)
placebo_time_effects <- placebo_time_summary$att %>%
  filter(Time >= 0) # Focus on "post-treatment" periods in the placebo

placebo_time_avg_effect <- mean(placebo_time_effects$Estimate)
placebo_time_max_effect <- max(abs(placebo_time_effects$Estimate))

cat("Placebo-in-time average effect:", placebo_time_avg_effect, "\n")
cat("Placebo-in-time maximum absolute effect:", placebo_time_max_effect, "\n\n")

#############################################
### Placebo-in-Space Test                ###
#############################################
cat("Running placebo-in-space test...\n")

# Get list of never-treated states
never_treated_states <- divorce_asc %>%
  filter(treatment_year == Inf) %>%
  pull(state_name) %>%
  unique()

# Number of states to assign placebo treatment to
n_placebo_states <- ceiling(length(never_treated_states) / 2)  # Half of never-treated states

# Randomly select states for placebo treatment
set.seed(123)  # For reproducibility
placebo_states <- sample(never_treated_states, n_placebo_states)

# Create artificial treatment years (randomly assigned between 1965-1975)
placebo_years <- sample(1965:1975, n_placebo_states, replace = TRUE)
placebo_treatment_df <- data.frame(
  state_name = placebo_states,
  placebo_year = placebo_years
)

# Create dataset with placebo treatments
placebo_space_data <- divorce_asc %>%
  left_join(placebo_treatment_df, by = "state_name") %>%
  mutate(
    placebo_treated = case_when(
      state_name %in% placebo_states & year >= placebo_year ~ 1,
      TRUE ~ 0
    )
  )

# Filter to only untreated states (both real and placebo)
placebo_space_data_filtered <- placebo_space_data %>%
  filter(treatment_year == Inf)  # Only use never-treated states

# Run ascm model with placebo treatment
if (nrow(placebo_space_data_filtered) > 0) {
  placebo_space_model <- multisynth(
    div_rate ~ placebo_treated,
    state_name,
    year,
    placebo_space_data_filtered,
    n.leads = 15,
    n.lags = 5
  )
  
  # Examine and save results
  placebo_space_summary <- summary(placebo_space_model)
  print(placebo_space_summary)
  
  # Plot placebo-in-space effects
  placebo_space_plot <- plot(placebo_space_model, levels = "Average")
  print(placebo_space_plot)
  ggsave("ascm_output/placebo/placebo_in_space.png", placebo_space_plot, width = 10, height = 8)
  
  # Calculate average placebo effect (should be close to zero if model is valid)
  placebo_space_effects <- placebo_space_summary$att %>%
    filter(Time >= 0)  # Focus on post-treatment periods
  
  placebo_space_avg_effect <- mean(placebo_space_effects$Estimate)
  placebo_space_max_effect <- max(abs(placebo_space_effects$Estimate))
  
  cat("Placebo-in-space average effect:", placebo_space_avg_effect, "\n")
  cat("Placebo-in-space maximum absolute effect:", placebo_space_max_effect, "\n\n")
} else {
  cat("Not enough never-treated states to run placebo-in-space test.\n\n")
}

#############################################
### Placebo Outcome Test (marriage rate)  ###
#############################################
cat("Running placebo outcome test with marriage rate...\n")

# Use married_annual as the placebo outcome
# This variable should not be affected by divorce laws in the short term
placebo_outcome_data <- divorce_asc %>%
  filter(!is.na(married_annual))  # Ensure we have marriage data

if (nrow(placebo_outcome_data) > 0) {
  # Run ascm model on marriage rates instead of divorce rates
  placebo_outcome_model <- multisynth(
    married_annual ~ treated,
    state_name,
    year,
    placebo_outcome_data,
    n.leads = 15,
    n.lags = 5
  )
  
  # Examine and save results
  placebo_outcome_summary <- summary(placebo_outcome_model)
  print(placebo_outcome_summary)
  
  # Plot placebo outcome effects
  placebo_outcome_plot <- plot(placebo_outcome_model, levels = "Average")
  print(placebo_outcome_plot)
  ggsave("ascm_output/placebo/placebo_outcome_marriage.png", placebo_outcome_plot, width = 10, height = 8)
  
  # Calculate average placebo effect (should be close to zero if causal mechanism is divorce-specific)
  placebo_outcome_effects <- placebo_outcome_summary$att %>%
    filter(Time >= 0 & Time <= 5)  # Focus on immediate post-treatment periods
  
  placebo_outcome_avg_effect <- mean(placebo_outcome_effects$Estimate)
  placebo_outcome_max_effect <- max(abs(placebo_outcome_effects$Estimate))
  
  cat("Placebo outcome (marriage rate) average effect:", placebo_outcome_avg_effect, "\n")
  cat("Placebo outcome (marriage rate) maximum absolute effect:", placebo_outcome_max_effect, "\n\n")
} else {
  cat("Marriage rate data not available for placebo outcome test.\n\n")
}

#############################################
### Summary of All Placebo Tests          ###
#############################################

# Create summary table of placebo test results
placebo_summary <- data.frame(
  Test = c("Placebo-in-Time", "Placebo-in-Space", "Placebo Outcome (Marriage Rate)"),
  Effect_Size = c(
    placebo_time_avg_effect,
    if(exists("placebo_space_avg_effect")) placebo_space_avg_effect else NA,
    if(exists("placebo_outcome_avg_effect")) placebo_outcome_avg_effect else NA
  ),
  P_Value = c(
    NA, 
    NA, 
    NA
  ),
  Interpretation = c(
    ifelse(abs(placebo_time_avg_effect) < 0.1, "Passed (small effect)", "Failed (large effect)"),
    if(exists("placebo_space_avg_effect")) 
      ifelse(abs(placebo_space_avg_effect) < 0.1, "Passed (small effect)", "Failed (large effect)") 
    else "Not calculated",
    if(exists("placebo_outcome_avg_effect")) 
      ifelse(abs(placebo_outcome_avg_effect) < 0.1, "Passed (small effect)", "Failed (large effect)") 
    else "Not calculated"
  )
)

# Print and save summary
print(placebo_summary)
write.csv(placebo_summary, "ascm_output/placebo/placebo_tests_summary.csv", row.names = FALSE)

# Save a text summary of the placebo tests
sink("ascm_output/placebo/placebo_tests_results.txt")
cat("===========================================\n")
cat("PLACEBO TESTS FOR ascm ANALYSIS\n")
cat("===========================================\n\n")

cat("1. PLACEBO-IN-TIME TEST\n")
cat("   Description: This test artificially shifts treatment 5 years earlier\n")
cat("   Average effect:", round(placebo_time_avg_effect, 4), "\n")
cat("   Max absolute effect:", round(placebo_time_max_effect, 4), "\n")
cat("   Interpretation:", ifelse(abs(placebo_time_avg_effect) < 0.1, 
                                 "PASSED - No significant pre-treatment effects", 
                                 "FAILED - Significant pre-treatment effects detected"), "\n\n")

if(exists("placebo_space_avg_effect")) {
  cat("2. PLACEBO-IN-SPACE TEST\n")
  cat("   Description: This test randomly assigns treatment to untreated states\n")
  cat("   Average effect:", round(placebo_space_avg_effect, 4), "\n")
  cat("   Max absolute effect:", round(placebo_space_max_effect, 4), "\n")
  cat("   Interpretation:", ifelse(abs(placebo_space_avg_effect) < 0.1, 
                                   "PASSED - No significant effects from placebo treatment", 
                                   "FAILED - Significant effects detected from placebo treatment"), "\n\n")
} else {
  cat("2. PLACEBO-IN-SPACE TEST\n")
  cat("   Not enough never-treated states to conduct this test\n\n")
}

if(exists("placebo_outcome_avg_effect")) {
  cat("3. PLACEBO OUTCOME TEST (MARRIAGE RATE)\n")
  cat("   Description: This test applies the same model to marriage rates instead of divorce rates\n")
  cat("   Average effect:", round(placebo_outcome_avg_effect, 4), "\n")
  cat("   Max absolute effect:", round(placebo_outcome_max_effect, 4), "\n")
  cat("   Interpretation:", ifelse(abs(placebo_outcome_avg_effect) < 0.1, 
                                   "PASSED - No significant effects on marriage rates", 
                                   "FAILED - Significant effects detected on marriage rates"), "\n\n")
} else {
  cat("3. PLACEBO OUTCOME TEST\n")
  cat("   Marriage rate data not available for this test\n\n")
}

cat("===========================================\n")
cat("OVERALL ASSESSMENT\n")
cat("===========================================\n")

# Fixed calculation to handle NA values properly with na.rm=TRUE
passed_tests <- sum(
  !is.na(placebo_time_avg_effect) && abs(placebo_time_avg_effect) < 0.1,
  if(exists("placebo_space_avg_effect") && !is.na(placebo_space_avg_effect)) abs(placebo_space_avg_effect) < 0.1 else 0,
  if(exists("placebo_outcome_avg_effect") && !is.na(placebo_outcome_avg_effect)) abs(placebo_outcome_avg_effect) < 0.1 else 0
)

# Count total valid tests (excluding those that couldn't be calculated)
total_tests <- 1 + # Time placebo (always calculated)
  (exists("placebo_space_avg_effect") && !is.na(placebo_space_avg_effect)) + 
  (exists("placebo_outcome_avg_effect") && !is.na(placebo_outcome_avg_effect))

cat("Passed", passed_tests, "out of", total_tests, "placebo tests\n")

# Safe comparison using isTRUE to handle edge cases
if(isTRUE(passed_tests == total_tests) && total_tests > 0) {
  cat("CONCLUSION: Results appear robust - all placebo tests passed\n")
} else if(isTRUE(passed_tests >= total_tests / 2) && total_tests > 0) {
  cat("CONCLUSION: Results appear moderately robust - majority of placebo tests passed\n")
} else if(total_tests > 0) {
  cat("CONCLUSION: Results require caution - several placebo tests failed\n")
} else {
  cat("CONCLUSION: Unable to determine robustness - no valid placebo tests completed\n")
}
sink()