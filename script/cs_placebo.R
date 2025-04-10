# This is a placebo test for the Callaway and Sant'Anna's 2021 method
# Ensure that you have run modern_did.R before running this script

# Clear any existing placebo variables to avoid conflicts
rm(list = ls(pattern = "^placebo"))

#############################################
### PLACEBO TESTS FOR C&S ESTIMATOR       ###
#############################################
cat("\nRunning alternative placebo tests for Callaway & Sant'Anna...\n")

# Create placebo directory if it doesn't exist
dir.create("modern_output/placebo", showWarnings = FALSE)

#############################################
### 1. PLACEBO-IN-SPACE TEST              ###
#############################################
# This approach assigns fake treatment to never-treated units

cat("1. Running placebo-in-space test...\n")

# Identify never-treated units
never_treated <- cs_data %>%
  filter(first_treat == 0) %>%
  pull(id) %>%
  unique()

if(length(never_treated) >= 4) {
  # Randomly select half of never-treated states for placebo treatment
  set.seed(123) # For reproducibility
  n_placebo <- floor(length(never_treated)/2)
  placebo_units <- sample(never_treated, n_placebo)
  
  # Assign random treatment years within our sample period
  possible_years <- 1965:1975
  placebo_years <- sample(possible_years, n_placebo, replace = TRUE)
  
  # Create dataframe with placebo treatment timing
  placebo_assignment <- data.frame(
    id = placebo_units,
    placebo_treat_year = placebo_years
  )
  
  # Create new dataset with only never-treated units
  placebo_space_data <- cs_data %>%
    filter(first_treat == 0) %>%
    left_join(placebo_assignment, by = "id") %>%
    mutate(
      # For placebo units, assign treatment year; for others keep at 0
      placebo_treat = ifelse(!is.na(placebo_treat_year), placebo_treat_year, 0)
    )
  
  # Run C&S on placebo data
  tryCatch({
    placebo_space_model <- att_gt(
      yname = "y", 
      tname = "time", 
      idname = "id", 
      gname = "placebo_treat",
      data = placebo_space_data, 
      control_group = "notyettreated",
      anticipation = 0,
      est_method = "dr", 
      allow_unbalanced_panel = TRUE
    )
    
    # Aggregate placebo effects
    placebo_space_att <- aggte(placebo_space_model, type = "simple")
    placebo_space_dyn <- aggte(placebo_space_model, type = "dynamic", 
                               min_e = -5, max_e = 10)
    
    # Print results
    cat("Placebo-in-space simple ATT result:\n")
    print(placebo_space_att)
    
    # Format for plotting
    placebo_space_df <- data.frame(
      time = placebo_space_dyn$egt,
      estimate = placebo_space_dyn$att.egt,
      se = placebo_space_dyn$se.egt,
      ci_lower = placebo_space_dyn$att.egt - 1.96 * placebo_space_dyn$se.egt,
      ci_upper = placebo_space_dyn$att.egt + 1.96 * placebo_space_dyn$se.egt
    )
    
    # Create plot
    space_plot <- ggplot(placebo_space_df, aes(x = time, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkblue") +
      geom_point(size = 2, color = "darkblue") +
      geom_line(color = "darkblue", group = 1) +
      theme_minimal() +
      labs(
        title = "Placebo-in-Space Test: C&S Estimator",
        subtitle = "Effect of Artificial Treatment on Never-Treated Units",
        x = "Time Relative to Placebo Treatment",
        y = "Placebo Effect on Divorce Rate"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, color = "gray")
      ) +
      coord_cartesian(xlim = c(-5, 10))
    
    print(space_plot)
    ggsave("modern_output/placebo/cs_placebo_space.png", space_plot, width = 10, height = 8)
    
    # Calculate metrics
    placebo_avg_effect <- mean(placebo_space_df$estimate[placebo_space_df$time >= 0], na.rm = TRUE)
    placebo_max_effect <- max(abs(placebo_space_df$estimate[placebo_space_df$time >= 0]), na.rm = TRUE)
    placebo_sig_rate <- mean(
      (placebo_space_df$ci_lower > 0 | placebo_space_df$ci_upper < 0)[placebo_space_df$time >= 0], 
      na.rm = TRUE
    )
    
    cat("Placebo-in-space results:\n")
    cat("Average effect:", placebo_avg_effect, "\n")
    cat("Maximum absolute effect:", placebo_max_effect, "\n")
    cat("Significance rate:", placebo_sig_rate*100, "%\n")
    cat("Interpretation:", ifelse(abs(placebo_avg_effect) < 0.1, 
                                  "PASSED - No significant effects from placebo treatment", 
                                  "FAILED - Significant effects detected from placebo treatment"), "\n\n")
    
    # Store results for summary
    placebo_space_result <- list(
      avg_effect = placebo_avg_effect,
      max_effect = placebo_max_effect,
      sig_rate = placebo_sig_rate,
      passed = abs(placebo_avg_effect) < 0.1
    )
    
  }, error = function(e) {
    cat("Error in placebo-in-space test:", e$message, "\n")
    cat("Skipping placebo-in-space test.\n\n")
    
    # Create empty result
    placebo_space_result <<- list(
      avg_effect = NA,
      max_effect = NA,
      sig_rate = NA,
      passed = NA
    )
  })
} else {
  cat("Not enough never-treated units for placebo-in-space test.\n\n")
  
  # Create empty result
  placebo_space_result <- list(
    avg_effect = NA,
    max_effect = NA,
    sig_rate = NA,
    passed = NA
  )
}

#############################################
### 2. RANDOMIZATION INFERENCE TEST       ###
#############################################
cat("2. Running randomization inference test...\n")

# Number of randomization iterations
n_iter <- 50  # Reduced for computational efficiency

# Get distribution of treatment timing (preserve structure)
treat_years <- cs_data %>%
  filter(first_treat > 0) %>%
  pull(first_treat) %>%
  table() %>%
  as.data.frame()

names(treat_years) <- c("year", "count")
treat_years$year <- as.numeric(as.character(treat_years$year))

# Get all unit IDs
all_ids <- unique(cs_data$id)
n_treated <- length(cs_data$id[cs_data$first_treat > 0] %>% unique())

# Storage for results
ri_results <- data.frame(
  iteration = 1:n_iter,
  att_estimate = numeric(n_iter)
)

# Run iterations
set.seed(456)  # For reproducibility

for(i in 1:n_iter) {
  if(i %% 10 == 0) cat("  - Running iteration", i, "of", n_iter, "\n")
  
  # Randomly select units to be "treated"
  random_treated <- sample(all_ids, n_treated)
  
  # Assign treatment years based on original distribution
  random_years <- numeric(n_treated)
  for(j in 1:nrow(treat_years)) {
    # For each treatment year in original data
    if(j == 1) {
      idx_start <- 1
    } else {
      idx_start <- sum(treat_years$count[1:(j-1)]) + 1
    }
    idx_end <- sum(treat_years$count[1:j])
    
    if(idx_start <= n_treated) {
      random_years[idx_start:min(idx_end, n_treated)] <- treat_years$year[j]
    }
  }
  
  # Create randomized treatment assignment
  random_assignment <- data.frame(
    id = random_treated,
    random_treat = random_years[1:length(random_treated)]
  )
  
  # Create dataset with randomized treatment
  random_data <- cs_data %>%
    select(-first_treat) %>%
    left_join(random_assignment, by = "id") %>%
    mutate(
      random_treat = ifelse(is.na(random_treat), 0, random_treat)
    )
  
  tryCatch({
    # Run C&S on randomized data
    ri_model <- att_gt(
      yname = "y", 
      tname = "time", 
      idname = "id", 
      gname = "random_treat",
      data = random_data, 
      control_group = "notyettreated",
      anticipation = 0,
      est_method = "dr", 
      allow_unbalanced_panel = TRUE
    )
    
    # Get simple ATT
    ri_att <- aggte(ri_model, type = "simple")
    ri_results$att_estimate[i] <- ri_att$overall.att
    
  }, error = function(e) {
    cat("  - Error in iteration", i, ":", e$message, "\n")
    ri_results$att_estimate[i] <- NA
  })
}

# Calculate the actual effect from original model
actual_att <- cs_simple$notyet$overall.att

# Remove NAs
ri_results <- ri_results %>%
  filter(!is.na(att_estimate))

if(nrow(ri_results) > 0) {
  # Calculate p-value (proportion of random effects >= actual effect in absolute value)
  p_value <- mean(abs(ri_results$att_estimate) >= abs(actual_att))
  
  # Plot histogram of randomization results
  ri_plot <- ggplot(ri_results, aes(x = att_estimate)) +
    geom_histogram(binwidth = 0.05, fill = "lightgreen", color = "darkgreen") +
    geom_vline(xintercept = actual_att, linetype = "dashed", color = "red", size = 1) +
    annotate("text", x = actual_att, y = max(table(cut(ri_results$att_estimate, breaks = 20)))/2, 
             label = paste("Actual Effect =", round(actual_att, 3)), 
             hjust = ifelse(actual_att > 0, -0.1, 1.1)) +
    annotate("text", x = min(ri_results$att_estimate), y = max(table(cut(ri_results$att_estimate, breaks = 20)))/1.5, 
             label = paste("p-value =", round(p_value, 3)), 
             hjust = 0) +
    theme_minimal() +
    labs(
      title = "Randomization Inference Test: C&S Estimator",
      subtitle = "Distribution of Effects Under Random Treatment Assignment",
      x = "Average Treatment Effect",
      y = "Frequency"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 12)
    )
  
  print(ri_plot)
  ggsave("modern_output/placebo/cs_randomization_inference.png", ri_plot, width = 10, height = 8)
  
  cat("Randomization inference results:\n")
  cat("Actual effect:", actual_att, "\n")
  cat("p-value:", p_value, "\n")
  cat("Interpretation:", ifelse(p_value < 0.05, 
                                "PASSED - Effect is unlikely to occur by chance", 
                                "FAILED - Effect could occur by random chance"), "\n\n")
  
  # Store results
  ri_result <- list(
    actual_effect = actual_att,
    p_value = p_value,
    passed = p_value < 0.05
  )
} else {
  cat("No valid results from randomization inference test.\n\n")
  
  # Create empty result
  ri_result <- list(
    actual_effect = actual_att,
    p_value = NA,
    passed = NA
  )
}

#############################################
### 3. OUTCOME PLACEBO TEST (Marriage)    ###
#############################################
cat("3. Running placebo outcome test with marriage rates...\n")

# First check if we have marriage data
has_marriage <- "married_annual" %in% names(raw_data)

if(has_marriage) {
  # Create dataset with marriage rates as outcome
  marriage_data <- cs_data %>%
    left_join(
      raw_data %>% 
        select(state_id, year, married_annual) %>%
        rename(id = state_id, time = year),
      by = c("id", "time")
    )
  
  # Check how many non-NA observations we have
  n_marriage_obs <- sum(!is.na(marriage_data$married_annual))
  
  if(n_marriage_obs > 100) {  # Only proceed if we have enough data
    tryCatch({
      # Run C&S with marriage rate as outcome
      marriage_model <- att_gt(
        yname = "married_annual", 
        tname = "time", 
        idname = "id", 
        gname = "first_treat",
        data = marriage_data %>% filter(!is.na(married_annual)), 
        control_group = "notyettreated",
        anticipation = 0,
        est_method = "dr", 
        allow_unbalanced_panel = TRUE
      )
      
      # Get dynamic effects
      marriage_dyn <- aggte(marriage_model, type = "dynamic", 
                            min_e = -5, max_e = 10)
      
      # Format for plotting
      marriage_df <- data.frame(
        time = marriage_dyn$egt,
        estimate = marriage_dyn$att.egt,
        se = marriage_dyn$se.egt,
        ci_lower = marriage_dyn$att.egt - 1.96 * marriage_dyn$se.egt,
        ci_upper = marriage_dyn$att.egt + 1.96 * marriage_dyn$se.egt
      )
      
      # Create plot
      marriage_plot <- ggplot(marriage_df, aes(x = time, y = estimate)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
        geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkred") +
        geom_point(size = 2, color = "darkred") +
        geom_line(color = "darkred", group = 1) +
        theme_minimal() +
        labs(
          title = "Placebo Outcome Test: C&S Estimator",
          subtitle = "Effect of Divorce Laws on Marriage Rates",
          x = "Time Relative to Divorce Law Change",
          y = "Effect on Marriage Rate"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.title = element_text(size = 12),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = NA, color = "gray")
        ) +
        coord_cartesian(xlim = c(-5, 10))
      
      print(marriage_plot)
      ggsave("modern_output/placebo/cs_placebo_marriage.png", marriage_plot, width = 10, height = 8)
      
      # Calculate metrics
      marriage_avg_effect <- mean(marriage_df$estimate[marriage_df$time >= 0 & marriage_df$time <= 5], na.rm = TRUE)
      marriage_max_effect <- max(abs(marriage_df$estimate[marriage_df$time >= 0 & marriage_df$time <= 5]), na.rm = TRUE)
      marriage_sig_rate <- mean(
        (marriage_df$ci_lower > 0 | marriage_df$ci_upper < 0)[marriage_df$time >= 0 & marriage_df$time <= 5], 
        na.rm = TRUE
      )
      
      cat("Placebo outcome (marriage) results:\n")
      cat("Average effect (0-5 years):", marriage_avg_effect, "\n")
      cat("Maximum absolute effect (0-5 years):", marriage_max_effect, "\n")
      cat("Significance rate (0-5 years):", marriage_sig_rate*100, "%\n")
      cat("Interpretation:", ifelse(abs(marriage_avg_effect) < 0.1, 
                                    "PASSED - No significant effects on marriage rates", 
                                    "FAILED - Significant effects detected on marriage rates"), "\n\n")
      
      # Store results
      marriage_result <- list(
        avg_effect = marriage_avg_effect,
        max_effect = marriage_max_effect,
        sig_rate = marriage_sig_rate,
        passed = abs(marriage_avg_effect) < 0.1
      )
      
    }, error = function(e) {
      cat("Error in placebo outcome test:", e$message, "\n")
      cat("Skipping placebo outcome test.\n\n")
      
      # Create empty result
      marriage_result <<- list(
        avg_effect = NA,
        max_effect = NA,
        sig_rate = NA,
        passed = NA
      )
    })
  } else {
    cat("Not enough marriage rate observations for placebo outcome test.\n\n")
    
    # Create empty result
    marriage_result <- list(
      avg_effect = NA,
      max_effect = NA,
      sig_rate = NA,
      passed = NA
    )
  }
} else {
  cat("Marriage rate data not available for placebo outcome test.\n\n")
  
  # Create empty result
  marriage_result <- list(
    avg_effect = NA,
    max_effect = NA,
    sig_rate = NA,
    passed = NA
  )
}

#############################################
### SUMMARY OF ALL PLACEBO TESTS          ###
#############################################
cat("Creating summary of all placebo tests...\n")

# Combine all test results
placebo_summary <- data.frame(
  Test = c("Placebo-in-Space", "Randomization Inference", "Placebo Outcome (Marriage)"),
  Effect = c(
    if(!is.na(placebo_space_result$avg_effect)) round(placebo_space_result$avg_effect, 3) else NA,
    round(ri_result$actual_effect, 3),
    if(!is.na(marriage_result$avg_effect)) round(marriage_result$avg_effect, 3) else NA
  ),
  Metric = c(
    if(!is.na(placebo_space_result$sig_rate)) paste0(round(placebo_space_result$sig_rate*100, 1), "% significant") else NA,
    paste0("p-value = ", round(ri_result$p_value, 3)),
    if(!is.na(marriage_result$sig_rate)) paste0(round(marriage_result$sig_rate*100, 1), "% significant") else NA
  ),
  Result = c(
    if(!is.na(placebo_space_result$passed)) ifelse(placebo_space_result$passed, "Passed", "Failed") else "Not run",
    if(!is.na(ri_result$passed)) ifelse(ri_result$passed, "Passed", "Failed") else "Not run",
    if(!is.na(marriage_result$passed)) ifelse(marriage_result$passed, "Passed", "Failed") else "Not run"
  )
)

# Print and save summary
print(placebo_summary)
write.csv(placebo_summary, "modern_output/placebo/cs_placebo_summary.csv", row.names = FALSE)

# Create a pretty summary plot
passed_colors <- c("Passed" = "darkgreen", "Failed" = "darkred", "Not run" = "gray50")

summary_plot <- ggplot(placebo_summary, aes(x = Test, y = 1, fill = Result)) +
  geom_tile(color = "white", size = 1.5) +
  geom_text(aes(label = Effect), vjust = -1, fontface = "bold") +
  geom_text(aes(label = Metric), vjust = 1, size = 3) +
  scale_fill_manual(values = passed_colors) +
  theme_minimal() +
  labs(
    title = "Summary of C&S Placebo Tests",
    subtitle = "Effect sizes and test results",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

print(summary_plot)
ggsave("modern_output/placebo/cs_placebo_summary.png", summary_plot, width = 10, height = 6)

# Save detailed summary to text file
sink("modern_output/placebo/cs_placebo_results.txt")
cat("===============================================\n")
cat("PLACEBO TEST RESULTS FOR CALLAWAY & SANT'ANNA\n")
cat("===============================================\n\n")

cat("1. PLACEBO-IN-SPACE TEST\n")
cat("   Description: Tests whether effects appear for never-treated units\n")
if(!is.na(placebo_space_result$avg_effect)) {
  cat("   Average effect:", round(placebo_space_result$avg_effect, 4), "\n")
  cat("   Maximum absolute effect:", round(placebo_space_result$max_effect, 4), "\n")
  cat("   Significance rate:", round(placebo_space_result$sig_rate*100, 1), "%\n")
  cat("   Result:", ifelse(placebo_space_result$passed, 
                           "PASSED - No significant effects detected in placebo units", 
                           "FAILED - Significant effects detected in placebo units"), "\n\n")
} else {
  cat("   Could not be calculated.\n\n")
}

cat("2. RANDOMIZATION INFERENCE TEST\n")
cat("   Description: Tests whether observed effect exceeds effects from random treatment assignment\n")
cat("   Actual effect:", round(ri_result$actual_effect, 4), "\n")
if(!is.na(ri_result$p_value)) {
  cat("   p-value:", round(ri_result$p_value, 4), "\n")
  cat("   Result:", ifelse(ri_result$passed, 
                           "PASSED - Effect is statistically significant compared to random assignment", 
                           "FAILED - Effect is not distinguishable from random chance"), "\n\n")
} else {
  cat("   p-value could not be calculated.\n\n")
}

cat("3. PLACEBO OUTCOME TEST (MARRIAGE RATES)\n")
cat("   Description: Tests whether effects appear for unrelated outcome (marriage rates)\n")
if(!is.na(marriage_result$avg_effect)) {
  cat("   Average effect (0-5 years):", round(marriage_result$avg_effect, 4), "\n")
  cat("   Maximum absolute effect (0-5 years):", round(marriage_result$max_effect, 4), "\n")
  cat("   Significance rate (0-5 years):", round(marriage_result$sig_rate*100, 1), "%\n")
  cat("   Result:", ifelse(marriage_result$passed, 
                           "PASSED - No significant effects detected on marriage rates", 
                           "FAILED - Significant effects detected on marriage rates"), "\n\n")
} else {
  cat("   Could not be calculated.\n\n")
}

cat("===============================================\n")
cat("OVERALL ASSESSMENT\n")
cat("===============================================\n")

# Count passed and total tests
tests_run <- sum(
  !is.na(placebo_space_result$passed),
  !is.na(ri_result$passed),
  !is.na(marriage_result$passed)
)

tests_passed <- sum(
  if(!is.na(placebo_space_result$passed)) placebo_space_result$passed else 0,
  if(!is.na(ri_result$passed)) ri_result$passed else 0,
  if(!is.na(marriage_result$passed)) marriage_result$passed else 0
)

if(tests_run > 0) {
  cat("Tests passed:", tests_passed, "out of", tests_run, "\n")
  
  if(tests_passed == tests_run) {
    cat("CONCLUSION: Results are robust - all placebo tests passed\n")
    cat("The C&S estimator appears to be reliable for this application.\n")
  } else if(tests_passed >= tests_run/2) {
    cat("CONCLUSION: Results are moderately robust - majority of placebo tests passed\n")
    cat("The C&S estimator appears generally reliable, though some caution is warranted.\n")
  } else {
    cat("CONCLUSION: Results require caution - multiple placebo tests failed\n")
    cat("The C&S estimator may not be ideal for this application. Consider additional\n")
    cat("validation or alternative estimation strategies.\n")
  }
} else {
  cat("No placebo tests could be successfully completed.\n")
  cat("Unable to assess the robustness of the C&S estimator.\n")
}
sink()

cat("\nAll C&S placebo test results saved to modern_output/placebo directory.\n")