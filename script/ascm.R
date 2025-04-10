# install.packages("devtools", repos='http://cran.us.r-project.org')
# devtools::install_github("ebenmichael/augsynth", force = TRUE)

# Load necessary libraries
library(augsynth)
library(haven)
library(tidyverse)
library(panelView)
library(ggplot2)

# Create an ascm_output directory if it doesn't exist
dir.create("ascm_output", showWarnings = FALSE)

# Load the divorce data
raw_data <- read_dta("Divorce-Wolfers-AER.dta") %>%
  mutate(across(where(is.labelled), as_factor)) %>%
  rename(state_name = st) %>%
  filter(year >= 1956, year <= 1988)

# Identify states that were always treated (treated before 1956)
always_treated_states <- raw_data %>%
  group_by(state_name) %>%
  summarize(min_lfdivlaw = min(lfdivlaw)) %>%
  filter(min_lfdivlaw < 1956) %>%
  pull(state_name)

# Identify states with very few post-treatment periods
late_treated_states <- raw_data %>%
  group_by(state_name) %>%
  summarize(
    treatment_year = min(lfdivlaw),
    last_year = max(year)
  ) %>%
  filter(treatment_year < 2000) %>%  # Only look at treated states
  mutate(post_periods = last_year - treatment_year) %>%
  filter(post_periods < 5) %>%  # States with fewer than 5 post-treatment years
  pull(state_name)

# BEFORE STATE REMOVAL - Create a dataset with only basic exclusions
# (always treated and late treated)
basic_exclusions <- c(always_treated_states, late_treated_states)
divorce_basic <- raw_data %>%
  filter(!(state_name %in% basic_exclusions)) %>%
  mutate(
    treatment_year = ifelse(lfdivlaw < 2000, lfdivlaw, Inf),
    treated = 1 * (year >= treatment_year)
  )

# Visualize treatment pattern of basic dataset
treatment_pattern_basic <- panelview(div_rate ~ treated, data = divorce_basic, 
                                     index = c("state_name", "year"), 
                                     pre.post = TRUE,
                                     main = "Staggered Adoption of Unilateral Divorce Laws (Before Additional Removals)")
ggsave("ascm_output/treatment_pattern_before_removal.png", treatment_pattern_basic, width = 12, height = 10)

# Run ascm on the basic dataset
div_ascm_basic <- multisynth(
  div_rate ~ treated,
  state_name,
  year,
  divorce_basic,
  n.leads = 15,
  n.lags = 5
)

# Examine results
div_results_basic <- summary(div_ascm_basic)
print(div_results_basic)
cat("Chosen nu value (before additional removals):", div_ascm_basic$nu, "\n")

# Plot the average effect before state removal
avg_plot_basic <- plot(div_ascm_basic, levels = "Average")
print(avg_plot_basic)
ggsave("ascm_output/ascm_average_effect_before_removal.png", avg_plot_basic, width = 10, height = 8)

# Now plot a few individual states from the basic dataset
# Choose some states with different patterns - excluding LA which has missing data
problematic_states <- c("NV", "NM", "IN")
for (state in problematic_states) {
  if (state %in% unique(divorce_basic$state_name)) {
    state_plot <- plot(div_ascm_basic, levels = state)
    print(state_plot)
    ggsave(paste0("ascm_output/ascm_state_", state, "_before_removal.png"), state_plot, width = 10, height = 8)
  }
}

# AFTER STATE REMOVAL - Add problematic states to removal list
# Also adding IN due to missing data
states_to_remove <- c(basic_exclusions, "NV", "LA", "NM", "IN")
cat("Removing states:", paste(states_to_remove, collapse=", "), "\n")

# Format for ascm, excluding all problematic states
divorce_asc <- raw_data %>%
  filter(!(state_name %in% states_to_remove)) %>%
  mutate(
    treatment_year = ifelse(lfdivlaw < 2000, lfdivlaw, Inf),
    treated = 1 * (year >= treatment_year)
  )

# Visualize the treatment pattern after removal
treatment_pattern_after <- panelview(div_rate ~ treated, data = divorce_asc, 
                                     index = c("state_name", "year"), 
                                     pre.post = TRUE,
                                     main = "Staggered Adoption of Unilateral Divorce Laws (After Removals)")
ggsave("ascm_output/treatment_pattern_after_removal.png", treatment_pattern_after, width = 12, height = 10)

# Run augmented synthetic control with default settings (auto-selected nu)
div_ascm <- multisynth(
  div_rate ~ treated,
  state_name,
  year,
  divorce_asc,
  n.leads = 15,
  n.lags = 5
)

# Examine results
div_results <- summary(div_ascm)
print(div_results)
cat("Chosen nu value (after removals):", div_ascm$nu, "\n")

# Save summary results to a text file
sink("ascm_output/ascm_results_summary.txt")
print(div_results)
cat("\nChosen nu value (after removals):", div_ascm$nu, "\n")
sink()

# Plot the average effect after state removal
avg_plot <- plot(div_ascm, levels = "Average")
print(avg_plot)
ggsave("ascm_output/ascm_average_effect_after_removal.png", avg_plot, width = 10, height = 8)

# Try different nu values to see sensitivity
# Separate SCM (nu = 0)
div_ascm_sep <- multisynth(
  div_rate ~ treated, 
  state_name, 
  year,
  nu = 0, 
  divorce_asc,
  n.leads = 15,
  n.lags = 5
)

# Plot separate SCM (nu = 0)
sep_plot <- plot(div_ascm_sep, levels = "Average")
print(sep_plot)
ggsave("ascm_output/ascm_separate_nu0.png", sep_plot, width = 10, height = 8)

# Mid-way (nu = 0.5)
div_ascm_mid <- multisynth(
  div_rate ~ treated, 
  state_name, 
  year,
  nu = 0.5, 
  divorce_asc,
  n.leads = 15,
  n.lags = 5
)

# Plot mid-way SCM (nu = 0.5)
mid_plot <- plot(div_ascm_mid, levels = "Average")
print(mid_plot)
ggsave("ascm_output/ascm_mid_nu05.png", mid_plot, width = 10, height = 8)

# Almost pooled SCM (nu = 0.99) - Added as requested
div_ascm_99 <- multisynth(
  div_rate ~ treated, 
  state_name, 
  year,
  nu = 0.99, 
  divorce_asc,
  n.leads = 15,
  n.lags = 5
)

# Plot nu = 0.99 SCM
nu99_plot <- plot(div_ascm_99, levels = "Average")
print(nu99_plot)
ggsave("ascm_output/ascm_nu099.png", nu99_plot, width = 10, height = 8)

# Pooled SCM (nu = 1)
div_ascm_pool <- multisynth(
  div_rate ~ treated, 
  state_name, 
  year,
  nu = 1, 
  divorce_asc,
  n.leads = 15,
  n.lags = 5
)

# Plot pooled SCM (nu = 1)
pool_plot <- plot(div_ascm_pool, levels = "Average")
print(pool_plot)
ggsave("ascm_output/ascm_pooled_nu1.png", pool_plot, width = 10, height = 8)

# Get a list of states and plot a selection of individual states from the final dataset
# Choose a few states with different treatment timings
remaining_states <- unique(divorce_asc$state_name)
treatment_years <- divorce_asc %>%
  filter(treatment_year < Inf) %>%
  group_by(state_name) %>%
  summarize(treatment_year = first(treatment_year)) %>%
  arrange(treatment_year)

# Get states from early, middle and late adoption periods
early_treated <- head(treatment_years$state_name, 3)
mid_treated <- treatment_years$state_name[round(length(treatment_years$state_name)/2-1):round(length(treatment_years$state_name)/2+1)]
late_treated <- tail(treatment_years$state_name, 3)

# Select states to plot
states_to_plot <- c(early_treated, mid_treated, late_treated)

# Plot individual states
for (state in states_to_plot) {
  if (state %in% remaining_states) {
    state_plot <- plot(div_ascm, levels = state)
    print(state_plot)
    ggsave(paste0("ascm_output/ascm_state_", state, "_after_removal.png"), state_plot, width = 10, height = 8)
  }
}

# Save a list of the removed states and the sensitivity results to a text file
sink("ascm_output/analysis_summary.txt")
cat("States removed from analysis:\n")
cat(paste(states_to_remove, collapse=", "), "\n\n")
cat("Generated individual plots for the following states:\n")
cat("Early treated states:", paste(early_treated, collapse=", "), "\n")
cat("Mid-period treated states:", paste(mid_treated, collapse=", "), "\n")
cat("Late treated states:", paste(late_treated, collapse=", "), "\n\n")
cat("Nu parameter sensitivity analysis:\n")
cat("Default (auto-selected) nu:", div_ascm$nu, "\n")
cat("Also tested nu values of: 0, 0.5, 0.99, and 1\n")
sink()

# Show a list of the individual state plots generated
cat("Generated individual plots for the following states:\n")
cat("Early treated states:", paste(early_treated, collapse=", "), "\n")
cat("Mid-period treated states:", paste(mid_treated, collapse=", "), "\n")
cat("Late treated states:", paste(late_treated, collapse=", "), "\n")

# Print the path where figures are saved
cat("\nAll figures have been saved to the 'ascm_output' directory\n")
