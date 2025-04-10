# Unilateral Divorce Laws and Divorce Rates: A Modern Causal Inference Analysis

This repository contains the replication code and analysis for my Capstone Project in Data Science and Economics (DSE 4101) at the National University of Singapore. The project reexamines the impact of unilateral divorce laws on divorce rates in the United States using modern difference-in-differences (DiD) methods.

## Overview

This project revisits Wolfers' (2006) seminal paper on unilateral divorce laws, applying contemporary DiD estimators to address potential biases in staggered adoption settings. The analysis includes:

1. Replication of the traditional two-way fixed effects (TWFE) approach
2. Application of four modern DiD methods:
   - Sun & Abraham (2021)
   - de Chaisemartin & D'Haultfœuille (2020)
   - Callaway & Sant'Anna (2021)
   - Augmented Synthetic Control Method (Ben-Michael et al., 2021)
3. Comprehensive robustness checks including placebo tests

## Repository Structure

```
script/
├── figures_output/           # Generated figures from the paper
├── divorce_output/           # Output from original Wolfers replication
├── modern_output/            # Output from modern DiD methods
│   └── placebo/              # Placebo test results for CS method
├── ascm_output/              # Output from ASCM analysis
│   └── placebo/              # Placebo test results for ASCM
├── tomode                    # Stata do-file from Wolfers
├── cs_placebo.R              # Placebo tests for Callaway & Sant'Anna
├── ascm_placebo.R            # Placebo tests for ASCM
├── ascm.R                    # Augmented Synthetic Control Method
├── modern_did.R              # Implementation of modern DiD methods
├── divorce.R                 # Replication of Wolfers (2006)
├── figures.R                 # Creates graphs and visualizations
├── RData                     # R workspace with all data objects
└── Divorce-Wolfers-AER       # Original Wolfers dataset
```

## Data

The analysis uses Wolfers' publicly available replication dataset from his website (https://users.nber.org/~jwolfers/data.php), which includes state-level divorce rates and unilateral divorce law adoption years. The original data and code files were obtained directly from this source. To enhance comparability across methods and mitigate data issues, seven problematic states are excluded due to high rates of missing data, outlier trends, or ambiguous treatment timing:
- Alaska
- Oklahoma
- South Dakota
- Nevada
- Louisiana
- New Mexico
- Indiana

This yields a balanced panel of 44 states from 1956 to 1988.

## Methods

### Two-Way Fixed Effects (TWFE)
The original approach from Wolfers (2006) using state and year fixed effects with dynamic treatment effects.

### Sun & Abraham (2021)
Interaction-weighted estimator that addresses heterogeneous treatment effects in event studies.

### de Chaisemartin & D'Haultfœuille (2020)
A flexible non-parametric framework that computes average treatment effects robust to treatment effect heterogeneity.

### Callaway & Sant'Anna (2021)
A doubly-robust estimator that computes group-time average treatment effects and then aggregates them.

### Augmented Synthetic Control Method (ASCM)
Extends the synthetic control approach to staggered adoption settings with multiple treated units.

## Placebo Tests

Several placebo tests are implemented to validate the causal interpretations:

1. **Placebo-in-time**: Artificially shifts treatment dates earlier to test for pre-trends
2. **Placebo-in-space**: Assigns placebo treatments to untreated states
3. **Placebo outcome**: Tests whether unrelated outcomes (marriage rates) are affected 
4. **Randomization inference**: Compares observed effects to distribution under random assignment

## Requirements

The analysis requires the following R packages:

```R
# Core packages
library(tidyverse)
library(haven)
library(fixest)

# Modern DiD packages
library(did)         # Callaway & Sant'Anna
library(DIDmultiplegt) # de Chaisemartin & D'Haultfœuille
library(augsynth)    # ASCM
```

## Usage

1. Run `divorce.R` to replicate the Wolfers (2006) analysis
2. Run `modern_did.R` to apply the modern DiD methods
3. Run `ascm.R` to implement the Augmented Synthetic Control Method
4. Run the placebo test scripts (`cs_placebo.R` and `ascm_placebo.R`) for robustness checks
5. Use `figures.R` to generate all the visualizations

## Main Findings

All four modern estimators reinforce Wolfers' core findings:
- None find a significant non-zero average effect in the long run
- All find a positive short-run effect
- Long-run effects trend negative across all methods
- The immediate effect peaks between years 2-3 post-reform, followed by a gradual decline

## References

- Ben-Michael, E., Feller, A., & Rothstein, J. (2021). The augmented synthetic control method. Journal of the American Statistical Association, 116(536), 1789–1803.
- Callaway, B., & Sant'Anna, P. H. (2021). Difference-in-differences with multiple time periods. Journal of Econometrics, 225(2), 200–230.
- de Chaisemartin, C., & D'Haultfœuille, X. (2020). Two-way fixed effects estimators with heterogeneous treatment effects. American Economic Review, 110(9), 2964–2996.
- Sun, L., & Abraham, S. (2021). Estimating dynamic treatment effects in event studies with heterogeneous treatment effects. Journal of Econometrics, 225(2), 175–199.
- Wolfers, J. (2006). Did unilateral divorce laws raise divorce rates? A reconciliation and new results. American Economic Review, 96(5), 1802–1820. Data and code available at: https://users.nber.org/~jwolfers/data.php

## Author

Tiffany Irene Prasetio  
National University of Singapore
