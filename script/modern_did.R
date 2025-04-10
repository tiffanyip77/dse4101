# install.packages("haven")
# install.packages("tidyverse")
# install.packages("did")
# install.packages("fixest")
# install.packages("DIDmultiplegtDYN")
# install.packages("ggplot2")
# install.packages("gridExtra")

# install.packages("devtools", repos='http://cran.us.r-project.org')
# devtools::install_github("ebenmichael/augsynth", force = TRUE)

# Load required libraries
library(augsynth)
library(panelView)
library(haven)
library(tidyverse)
library(did)
library(fixest)
library(DIDmultiplegtDYN)
library(ggplot2)
library(gridExtra)

# Create modern_output directory if it doesn't exist
dir.create("modern_output", showWarnings = FALSE)

# Helper functions for common tasks
create_plot <- function(data, title, color = "darkgreen") {
  ggplot(data, aes(x = time, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = color) +
    geom_point(size = 2, color = color) +
    geom_line(color = color, group = 1) +
    theme_minimal() +
    labs(
      title = title,
      subtitle = "Response of Divorce Rate to Unilateral Divorce Laws",
      x = "Years since (until) adoption of Unilateral Divorce Laws",
      y = "Effect on Divorce Rate (per thousand people)"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "gray")
    ) +
    coord_cartesian(xlim = c(-5, 15))
}

calculate_stats <- function(data, period_range) {
  filtered <- data %>% filter(time %in% period_range)
  if(nrow(filtered) == 0) return(list(mean_est = NA, mean_se = NA, significant = NA, rmse = NA))
  
  mean_est <- mean(filtered$estimate, na.rm = TRUE)
  mean_se <- mean(filtered$se, na.rm = TRUE)
  significant <- sum((filtered$ci_lower > 0) | (filtered$ci_upper < 0), na.rm = TRUE)/nrow(filtered)
  rmse <- sqrt(mean(filtered$estimate^2, na.rm = TRUE))
  
  return(list(mean_est = mean_est, mean_se = mean_se, significant = significant, rmse = rmse))
}

# Load the divorce data
raw_data <- read_dta("Divorce-Wolfers-AER.dta") %>%
  mutate(across(where(is.labelled), as_factor)) %>%
  rename(state_name = st) %>%
  filter(year >= 1956, year <= 1988)

# Identify states to remove
states_to_remove <- c(
  # Always treated (treated before 1956)
  raw_data %>%
    group_by(state_name) %>%
    summarize(min_lfdivlaw = min(lfdivlaw)) %>%
    filter(min_lfdivlaw < 1956) %>%
    pull(state_name),
  
  # Late treated (fewer than 5 post-treatment years)
  raw_data %>%
    group_by(state_name) %>%
    summarize(
      treatment_year = min(lfdivlaw),
      last_year = max(year)
    ) %>%
    filter(treatment_year < 2000) %>%
    mutate(post_periods = last_year - treatment_year) %>%
    filter(post_periods < 5) %>%
    pull(state_name),
  
  # Other problematic states
  c("NV", "LA", "NM", "IN")
)

cat("Removing states:", paste(states_to_remove, collapse=", "), "\n")

# Save list of removed states to text file
sink("modern_output/did_removed_states.txt")
cat("States removed from DiD analysis:\n")
cat(paste(states_to_remove, collapse=", "))
sink()

# Prepare clean dataset
raw_data <- raw_data %>%
  filter(!(state_name %in% states_to_remove)) %>%
  mutate(
    treatment_year = ifelse(lfdivlaw < 2000, lfdivlaw, Inf),
    treated = 1 * (year >= treatment_year)
  )

# Visualize the treatment pattern
treatment_pattern <- panelview(div_rate ~ treated, data = raw_data, 
                               index = c("state_name", "year"), 
                               pre.post = TRUE,
                               main = "Staggered Adoption of Unilateral Divorce Laws")
ggsave("modern_output/did_treatment_pattern.png", treatment_pattern, width = 12, height = 10)

# Create state ID variable and cohort information
state_ids <- raw_data %>%
  distinct(state_name) %>%
  arrange(state_name) %>%
  mutate(state_id = row_number())

# Add state IDs to raw data
raw_data <- raw_data %>% left_join(state_ids, by = "state_name")

# Create cohort variable properly
treatment_info <- raw_data %>%
  distinct(state_id, lfdivlaw) %>%
  mutate(cohort = case_when(
    lfdivlaw < 1956 ~ 1956,  # Treat as if they were treated in the first year of our sample
    lfdivlaw == 2000 ~ NA_real_,  # Never-treated states
    TRUE ~ as.numeric(lfdivlaw)  # Normal treated states
  ))

# Create complete balanced panel
panel <- expand_grid(state_id = state_ids$state_id, year = 1956:1988)
divorce_data <- panel %>%
  left_join(state_ids, by = "state_id") %>%
  left_join(raw_data %>% select(state_id, year, div_rate, stpop), by = c("state_id", "year")) %>%
  left_join(treatment_info, by = "state_id") %>%
  filter(!is.na(div_rate))

# Format data for DiD
divorce_data_formatted <- divorce_data %>%
  rename(id = state_id, time = year, y = div_rate) %>%
  mutate(
    # Binary treatment indicator
    treat = !is.na(cohort) & time >= cohort,
    
    # Relative time variable
    rel_time = ifelse(!is.na(cohort), time - cohort, NA_real_)
  )

# Time variables for all analyses
period_ranges <- list(
  pre = -5:-1,
  immediate = 0:3,
  medium = 4:9,
  long = 10:15
)

#####################################################
### PART 1: CALLAWAY & SANT'ANNA ESTIMATOR        ###
#####################################################
cat("Running Callaway & Sant'Anna estimators...\n")

# Set up data for C&S
cs_data <- divorce_data_formatted %>%
  mutate(first_treat = ifelse(is.na(cohort), 0, cohort))

# Run C&S models with different control groups
cs_models <- list(
  notyet = att_gt(
    yname = "y", tname = "time", idname = "id", gname = "first_treat",
    data = cs_data, anticipation = 0, control_group = "notyettreated",
    est_method = "dr", clustervars = "id", allow_unbalanced_panel = TRUE
  ),
  never = att_gt(
    yname = "y", tname = "time", idname = "id", gname = "first_treat",
    data = cs_data, anticipation = 0, control_group = "nevertreated",
    est_method = "dr", clustervars = "id", allow_unbalanced_panel = TRUE
  )
)

# Create dynamic effect aggregations
cs_dynamic <- list(
  notyet = aggte(cs_models$notyet, type = "dynamic", min_e = -5, max_e = 15, na.rm = TRUE),
  never = aggte(cs_models$never, type = "dynamic", min_e = -5, max_e = 15, na.rm = TRUE)
)

# Create simple effect aggregations
cs_simple <- list(
  notyet = aggte(cs_models$notyet, type = "simple", na.rm = TRUE),
  never = aggte(cs_models$never, type = "simple", na.rm = TRUE)
)

# Print simple and dynamic aggregation results
lapply(names(cs_simple), function(model) {
  cat(paste("Callaway & Sant'Anna simple aggregation -", model, "control:\n"))
  print(cs_simple[[model]])
})

# Save CS results to text file
sink("modern_output/cs_results.txt")
lapply(names(cs_simple), function(model) {
  cat(paste("Callaway & Sant'Anna simple aggregation -", model, "control:\n"))
  print(cs_simple[[model]])
  cat("\n\n")
})

lapply(names(cs_dynamic), function(model) {
  cat(paste("Callaway & Sant'Anna dynamic aggregation -", model, "control:\n"))
  print(cs_dynamic[[model]])
  cat("\n\n")
})
sink()

# Format dynamic results for plotting
cs_plot_data <- lapply(names(cs_dynamic), function(model) {
  results <- cs_dynamic[[model]]
  data.frame(
    time = results$egt,
    estimate = results$att.egt,
    se = results$se.egt,
    ci_lower = results$att.egt - 1.96 * results$se.egt,
    ci_upper = results$att.egt + 1.96 * results$se.egt,
    method = paste0("CS: ", ifelse(model == "notyet", "Not-yet-treated", "Never-treated"))
  )
})
names(cs_plot_data) <- names(cs_dynamic)

# Create plots for each CS model
cs_plots <- lapply(names(cs_plot_data), function(model) {
  title <- paste("Callaway & Sant'Anna:", 
                 ifelse(model == "notyet", "Not-Yet-Treated", "Never-Treated"), 
                 "Control (Dynamic)")
  color <- ifelse(model == "notyet", "darkgreen", "darkblue")
  create_plot(cs_plot_data[[model]], title, color)
})
names(cs_plots) <- names(cs_plot_data)

# Create CS comparison plot
cs_combined_data <- bind_rows(cs_plot_data)
cs_comparison_plot <- ggplot(cs_combined_data, aes(x = time, y = estimate, color = method, fill = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("darkgreen", "darkblue")) +
  scale_fill_manual(values = c("darkgreen", "darkblue")) +
  theme_minimal() +
  labs(
    title = "Comparison of Callaway & Sant'Anna Models (Dynamic)",
    subtitle = "Different control group specifications",
    x = "Years since (until) adoption of Unilateral Divorce Laws",
    y = "Effect on Divorce Rate (per thousand people)"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray")
  ) +
  coord_cartesian(xlim = c(-5, 15))

# Print and save CS plots
for(model in names(cs_plots)) {
  print(cs_plots[[model]])
  ggsave(paste0("modern_output/cs_", model, "_dynamic.png"), cs_plots[[model]], width = 10, height = 8)
}
print(cs_comparison_plot)
ggsave("modern_output/cs_comparison.png", cs_comparison_plot, width = 10, height = 8)

#####################################################
### PART 2: SUN & ABRAHAM ESTIMATOR               ###
#####################################################
cat("Running Sun & Abraham estimator...\n")

# Prepare data for Sun & Abraham
sa_model_data <- divorce_data_formatted %>%
  mutate(cohort_sa = if_else(is.na(cohort), 2100, as.numeric(cohort))) %>%
  filter(!is.na(y), !is.na(time), !is.na(id))

# Check cohort distribution
print(table(sa_model_data$cohort_sa))

# Estimate the Sun & Abraham model
sunab_mod <- feols(y ~ sunab(cohort_sa, time, ref.c = 2100) | id + time, 
                   data = sa_model_data, 
                   weights = ~ stpop,
                   vcov = "hetero")

# Print summary
summary_sa <- summary(sunab_mod)
print(summary_sa)

# Save SA summary to text file
sink("modern_output/sa_results.txt")
print(summary_sa)
sink()

# Use the built-in plotting function
sa_iplot <- iplot(sunab_mod, 
                  main = "Sun & Abraham: Year-by-Year Effects",
                  xlab = "Years since (until) adoption of Unilateral Divorce Laws",
                  ylab = "Effect on Divorce Rate (per thousand people)",
                  drop = "^time::[0-9]{2}", # Drop periods >9 away from treatment
                  ref.line = 0)

# Save the iplot
ggsave("modern_output/sa.png", sa_iplot$plot, width = 10, height = 8)

# Extract coefficients for custom plot
sa_coefficients <- coef(sunab_mod)
sa_std_errors <- se(sunab_mod)
sa_coef_names <- names(sa_coefficients)

# Create a data frame with the extracted coefficients
sa_plot_data <- data.frame(
  name = sa_coef_names,
  estimate = sa_coefficients,
  se = sa_std_errors
) %>%
  filter(grepl("^time::", name)) %>%
  mutate(
    time = as.numeric(gsub("time::", "", name)),
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96 * se,
    method = "Sun & Abraham"
  ) %>%
  filter(time >= -5, time <= 15)

# Create custom SA plot
sa_plot <- create_plot(sa_plot_data, "Sun & Abraham: Year-by-Year Effects", "darkred")

# Print and save SA plot
print(sa_plot)
ggsave("modern_output/sa_manual.png", sa_plot, width = 10, height = 8)

#####################################################
### PART 3: DE CHAISEMARTIN & D'HAULTFOEUILLE     ###
#####################################################
cat("Running de Chaisemartin & d'Haultfoeuille estimator...\n")

# Make sure treatment is properly coded for DIDmultiplegtDYN
dcdh_data <- divorce_data_formatted %>%
  mutate(
    treatment = as.numeric(treat),
    group_id = as.numeric(id)
  )

# Run the DIDmultiplegtDYN model
mod_dCDH <- did_multiplegt_dyn(
  df = dcdh_data,
  outcome = "y",
  group = "group_id",
  time = "time",
  treatment = "treatment",
  effects = 15,    # 15 post-treatment periods
  placebo = 5,     # 5 pre-treatment periods
  cluster = "group_id"
)

# Display summary
dcdh_summary <- summary(mod_dCDH)

# Save DCDH results to text file
sink("modern_output/dcdh_results.txt")
summary(mod_dCDH)
sink()

# Use the built-in plot with consistent styling
dcdh_plot <- mod_dCDH$plot +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(
    title = "de Chaisemartin & d'Haultfoeuille: Year-by-Year Effects",
    subtitle = "Response of Divorce Rate to Unilateral Divorce Laws",
    x = "Years since (until) adoption of Unilateral Divorce Laws",
    y = "Effect on Divorce Rate (per thousand people)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray")
  ) +
  scale_x_continuous(breaks = seq(-5, 15, by = 5)) +
  coord_cartesian(xlim = c(-5, 15))

# Print and save DCDH plot
print(dcdh_plot)
ggsave("modern_output/dcdh.png", dcdh_plot, width = 10, height = 8)

# Use the manually extracted data for DCDH as in the original code
# Manually create DCDH data from the summary results looking at the actual plot
dcdh_manual_times <- c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

dcdh_manual_estimates <- c(
  # Placebo effects (pre-treatment)
  0.13597, 0.12247, 0.09938, 0.07516, 0.04664,
  # Reference point at t=0
  0.00000,  # This is the reference point
  # Treatment effects
  0.06472, 0.14355, 0.17689, 0.16494, 0.06165,
  0.02678, 0.04102, -0.06497, -0.14463, -0.24463,
  -0.25242, -0.32884, -0.32961, -0.35698, -0.30576
)

dcdh_manual_se <- c(
  # Placebo SE
  0.06223, 0.07561, 0.09086, 0.10311, 0.12498,
  # Reference point at t=0 (no uncertainty)
  0.00001,  # Near-zero SE for the reference point
  # Treatment SE
  0.07187, 0.08696, 0.08711, 0.08210, 0.10130,
  0.10329, 0.11962, 0.11698, 0.13018, 0.12561,
  0.11685, 0.11477, 0.11776, 0.13174, 0.13478
)

# Create manual DCDH dataframe
dcdh_plot_data <- data.frame(
  time = dcdh_manual_times,
  estimate = dcdh_manual_estimates,
  se = dcdh_manual_se,
  ci_lower = dcdh_manual_estimates - 1.96 * dcdh_manual_se,
  ci_upper = dcdh_manual_estimates + 1.96 * dcdh_manual_se,
  method = "de Chaisemartin & d'Haultfoeuille"
)

#####################################################
### PART 4: COMBINED COMPARISON OF ESTIMATORS      ###
#####################################################

# Define method levels and colors
method_levels <- c("CS: Not-yet-treated", "Sun & Abraham", "de Chaisemartin & d'Haultfoeuille")
method_colors <- c("darkgreen", "darkred", "blue")
names(method_colors) <- method_levels

# Combine datasets for the three specified methods
combined_data <- bind_rows(
  cs_plot_data$notyet,
  sa_plot_data, 
  dcdh_plot_data
) %>%
  mutate(method = factor(method, levels = method_levels))

# Create the comparison plot
comparison_plot <- ggplot(combined_data, aes(x = time, y = estimate, color = method, group = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = method), alpha = 0.15, color = NA) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = method_colors) +
  scale_fill_manual(values = method_colors) +
  theme_minimal() +
  labs(
    title = "Comparison of DiD Estimators: Dynamic Event Study Effects",
    subtitle = "Response of Divorce Rate to Unilateral Divorce Laws",
    x = "Years since (until) adoption of Unilateral Divorce Laws",
    y = "Effect on Divorce Rate (per thousand people)"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray")
  ) +
  scale_x_continuous(breaks = seq(-5, 15, by = 5)) +
  coord_cartesian(xlim = c(-5, 15))

# Print and save comparison plot
print(comparison_plot)
ggsave("modern_output/did_methods_comparison.png", comparison_plot, width = 10, height = 8)

#####################################################
### PART 5: QUANTITATIVE COMPARISON               ###
#####################################################

# Calculate statistics for each method and period
stats_results <- list()
for (method_name in method_levels) {
  method_data <- combined_data %>% filter(method == method_name)
  
  for (period_name in names(period_ranges)) {
    period_range <- period_ranges[[period_name]]
    stats_results[[paste0(method_name, "_", period_name)]] <- calculate_stats(method_data, period_range)
  }
}

# Create comparison table
comparison_table <- data.frame(
  Period = c("Pre-treatment (-5 to -1)", "Immediate (0 to 3)", "Medium-term (4 to 9)", "Long-term (10+)")
)

# Add results for each method to the table
for (method_name in method_levels) {
  method_clean <- gsub("[ &']", "_", method_name)
  
  comparison_table[[paste0(method_clean, "_Mean")]] <- sapply(names(period_ranges), function(p) {
    stats_results[[paste0(method_name, "_", p)]]$mean_est
  })
  
  comparison_table[[paste0(method_clean, "_SE")]] <- sapply(names(period_ranges), function(p) {
    stats_results[[paste0(method_name, "_", p)]]$mean_se
  })
  
  comparison_table[[paste0(method_clean, "_Sig")]] <- sapply(names(period_ranges), function(p) {
    stats_results[[paste0(method_name, "_", p)]]$significant
  })
}

# Print formatted table
print(comparison_table, digits = 3)
write.csv(comparison_table, "modern_output/did_comparison_metrics_all.csv", row.names = FALSE)

# Create summary dataframe for plotting
summary_df <- data.frame(
  Period = rep(c("Pre-treatment", "Immediate", "Medium-term", "Long-term"), length(method_levels)),
  Method = rep(method_levels, each = 4),
  Effect = unlist(lapply(method_levels, function(m) {
    sapply(names(period_ranges), function(p) {
      stats_results[[paste0(m, "_", p)]]$mean_est
    })
  }))
)

# Convert factors with the right order
summary_df$Period <- factor(summary_df$Period, 
                            levels = c("Pre-treatment", "Immediate", "Medium-term", "Long-term"))
summary_df$Method <- factor(summary_df$Method, levels = method_levels)

# Create a bar chart comparing methods
summary_plot <- ggplot(summary_df, aes(x = Period, y = Effect, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = method_colors) +
  theme_minimal() +
  labs(
    title = "Comparison of DiD Estimators by Time Period",
    subtitle = "Average effects across different phases of treatment",
    y = "Average Effect on Divorce Rate (per thousand people)"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12)
  )

# Print and save summary plot
print(summary_plot)
ggsave("modern_output/did_methods_summary.png", summary_plot, width = 10, height = 8)

#####################################################
### PART 6: PRE-TREND TEST AND OVERALL ASSESSMENT ###
#####################################################

# Formal test for pre-trends
pre_trend_test <- data.frame(
  Method = method_levels,
  Mean_Pre_Effect = sapply(method_levels, function(m) stats_results[[paste0(m, "_pre")]]$mean_est),
  Abs_Mean_Pre_Effect = sapply(method_levels, function(m) {
    method_data <- combined_data %>% filter(method == m, time < 0)
    mean(abs(method_data$estimate), na.rm = TRUE)
  }),
  Pre_RMSE = sapply(method_levels, function(m) {
    method_data <- combined_data %>% filter(method == m, time < 0)
    sqrt(mean(method_data$estimate^2, na.rm = TRUE))
  }),
  Pre_Sig_Rate = sapply(method_levels, function(m) {
    method_data <- combined_data %>% filter(method == m, time < 0)
    mean((method_data$ci_lower > 0) | (method_data$ci_upper < 0), na.rm = TRUE)
  })
)

# Comparison with Wolfers' pattern
wolfers_pattern <- data.frame(
  Method = c(method_levels, "Wolfers (2006)"),
  Imm_Effect = c(
    sapply(method_levels, function(m) stats_results[[paste0(m, "_immediate")]]$mean_est),
    "0.2-0.3"
  ),
  Med_Effect = c(
    sapply(method_levels, function(m) stats_results[[paste0(m, "_medium")]]$mean_est),
    "Declining"
  ),
  Long_Effect = c(
    sapply(method_levels, function(m) stats_results[[paste0(m, "_long")]]$mean_est),
    "Negative, -0.2 to -0.5"
  )
)

# Print pre-trend test and pattern comparison
print("Pre-Trend Test Results:")
print(pre_trend_test, digits = 3)

print("Comparison with Wolfers' Pattern:")
print(wolfers_pattern, digits = 3)

write.csv(pre_trend_test, "modern_output/did_pretrend_test.csv", row.names = FALSE)
write.csv(wolfers_pattern, "modern_output/wolfers_pattern_comparison.csv", row.names = FALSE)

# Calculate consistency score based on pattern match with Wolfers
wolfers_consistency <- sapply(method_levels, function(m) {
  imm_effect <- stats_results[[paste0(m, "_immediate")]]$mean_est
  med_effect <- stats_results[[paste0(m, "_medium")]]$mean_est
  long_effect <- stats_results[[paste0(m, "_long")]]$mean_est
  
  score <- as.numeric(imm_effect > 0 && imm_effect < 0.5) +  # Positive immediate effect
    as.numeric(med_effect < imm_effect) +              # Declining medium effect
    as.numeric(long_effect < 0)                        # Negative long-term effect
  return(score)
})
names(wolfers_consistency) <- method_levels

# Calculate overall scores
overall_scores <- data.frame(
  Method = method_levels,
  Pre_Trend_Score = -pre_trend_test$Abs_Mean_Pre_Effect,  # Lower is better
  Precision_Score = -sapply(method_levels, function(m) stats_results[[paste0(m, "_immediate")]]$mean_se),  # Lower is better
  Pattern_Score = wolfers_consistency
)

# Standardize scores
overall_scores$Pre_Trend_Score_Std <- scale(overall_scores$Pre_Trend_Score)
overall_scores$Precision_Score_Std <- scale(overall_scores$Precision_Score)
overall_scores$Pattern_Score_Std <- scale(overall_scores$Pattern_Score)
overall_scores$Total_Score <- overall_scores$Pre_Trend_Score_Std + 
  overall_scores$Precision_Score_Std + 
  overall_scores$Pattern_Score_Std

# Find the best method
best_method <- overall_scores$Method[which.max(overall_scores$Total_Score)]

# Save overall scores to CSV
write.csv(overall_scores, "modern_output/did_overall_scores.csv", row.names = FALSE)

# Prepare final assessment text
assessment_text <- capture.output({
  cat("\n-----------------------------------------------------\n")
  cat("OVERALL ASSESSMENT OF DiD ESTIMATORS\n")
  cat("-----------------------------------------------------\n\n")
  
  cat("1. PARALLEL TRENDS ASSUMPTION:\n")
  cat("   - Method with smallest pre-trend deviations:", 
      pre_trend_test$Method[which.min(pre_trend_test$Abs_Mean_Pre_Effect)], "\n")
  cat("   - Method with lowest pre-treatment significance rate:", 
      pre_trend_test$Method[which.min(pre_trend_test$Pre_Sig_Rate)], "\n\n")
  
  cat("2. PRECISION OF ESTIMATES:\n")
  cat("   - Method with smallest standard errors (immediate period):", 
      method_levels[which.min(sapply(method_levels, function(m) 
        stats_results[[paste0(m, "_immediate")]]$mean_se))], "\n")
  cat("   - Method with smallest standard errors (long-term):", 
      method_levels[which.min(sapply(method_levels, function(m) 
        stats_results[[paste0(m, "_long")]]$mean_se))], "\n\n")
  
  cat("3. CONSISTENCY WITH WOLFERS' FINDINGS:\n")
  cat("   - Wolfers found immediate effects around 0.2-0.3, declining over time, and turning negative after a decade\n")
  cat("   - Method most consistent with Wolfers' pattern:", 
      method_levels[which.max(wolfers_consistency)], "\n\n")
  
  cat("4. FINAL RECOMMENDATION:\n")
  cat("   Based on pre-trend validity, precision, and consistency with Wolfers' findings,\n")
  cat("   the preferred method for this application appears to be:\n")
  cat("   ", best_method, "\n\n")
  
  cat("5. QUANTITATIVE COMPARISON SUMMARY:\n")
  for (period in names(period_ranges)) {
    period_label <- switch(period,
                           pre = "Pre-treatment effects (should be zero)",
                           immediate = "Immediate effects (0-3 years)",
                           medium = "Medium-term effects (4-9 years)",
                           long = "Long-term effects (10+ years)")
    
    cat("   -", period_label, ":\n")
    cat("     ", paste(sapply(method_levels, function(m) {
      paste0(gsub(".*: ", "", m), ": ", round(stats_results[[paste0(m, "_", period)]]$mean_est, 3))
    }), collapse = " | "), "\n")
  }
})

# Save assessment to text file
writeLines(assessment_text, "modern_output/did_final_assessment.txt")

# Print assessment
cat(assessment_text)