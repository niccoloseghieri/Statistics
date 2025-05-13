# Combining Estimates – Statistics for Data Science Project

**Academic Year**: 2023/2024  
**Course**: Statistics for Data Science  
**Based on**: *"Combining Estimates"* by Thomas Struppeck (FCAS, ASA, CERA, 2014)  

---

## Project Overview

This project aims to **reproduce and simulate the results** presented in Thomas Struppeck’s paper *“Combining Estimates”*, using **R programming language**. The paper addresses how to optimally combine different estimates (with potentially different variances or correlations) to improve the precision of predictions—primarily in insurance and actuarial contexts.

The project is divided into two main sections:

---

## Repository Structure

### `1Comb_Est_Same_Quantity`
Reproduces Section 2 of the paper: combining two or more estimates of the **same quantity** (e.g., unpaid claims).

**Contains:**
- `global_variables_1.r` – Variables and assumptions used in the examples.
- `functions_1.r` – Utility functions for statistical computations.
- `generate_samples_functions_1.r` – Functions to generate normally distributed samples.
- `plot_functions_1.r` – Plotting utilities used across the scripts.
- `comb_two_estimates.r` – Reproduces the main example with θₐ and θᵦ and finds the optimal weighted average.
- `comb_two_estimates_100%Corr.r` – Simulates the case where estimates are perfectly correlated (ρ = 1) and explores how correlation impacts precision.
- `comb_more_estimates.r` – Generalizes the case to multiple estimates (≥ 3), using inverse variance weighting.
- `plot/` – Contains all the generated plots for comparisons and CI visualization.

---

### `2Comb_Est_Mult_Components`
Reproduces Sections 3 and 4: combining estimates for **multiple components** (e.g., different lines of business).

**Contains:**
- `global_variables_2.r` – Setup for means, variances, and percentiles.
- `functions_2.r` – Statistical functions for simulations and computations.
- `generate_samples_functions_2.r` – Functions to simulate multiple correlated components.
- `multiple_components.r` – Reproduces page 11 of the paper (combined loss estimate).
- `simulation_multiple_comp.r` – Data generator to simulate multiple scenarios across different correlation settings.
- `user_application.r` – A **Shiny app** replicating the Excel tool from the paper in a fully dynamic R interface.

---

## Key Concepts Explored

### Combining Estimates (Same Quantity)
- Weighted average of two independent estimates.
- Optimization of weights to **maximize precision**.
- Simulation of 95% Confidence Intervals (CI).
- Exploration of **accuracy vs. precision**.
- Correlation effects on variance of combined estimators.

### Combining Multiple Estimates
- Generalization via **Inverse Variance Weighting (IVW)**.

### Combining Multiple Components
- Estimation of **total loss across multiple correlated components**.
- Comparison across:
  - Naïve total (ρ = 1),
  - No correlation (ρ = 0),
  - Covariance-adjusted method (using the variance-covariance matrix).

---

## Shiny App (Interactive Tool)

An interactive Shiny app is provided in `user_application.r`:

- Input percentiles and expectations for multiple components.
- Adjust correlations interactively.
- Observe the impact on combined estimate and confidence intervals.

Technologies used:
- `shiny` – for building the web app
- `DT` – for interactive tables

---

**Authors**:  
- Camilla Chiruzzi  
- Niccolò Seghieri  
- Stefano Toresan  
