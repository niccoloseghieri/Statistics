# This script emulate section 2. Combining Two Or More Estimates For The Same Quantity
# from the paper by: Thomas Struppeck, "Combining Estimates"

# Sets the R working directory to the directory where the currently active document in RStudio
# is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load the global variables and functions to make plots and computations   
source("global_variables_1.r")
source("plot_functions_1.r")
source("functions_1.r")
source("generate_samples_functions_1.r")

# Margin c(bottom, left, top, right)
par(mar=c(4,4,1,1)) 

# Check if the "Plot" directory exists, and if not, create it
if (!dir.exists("Plot")) {
  dir.create("Plot")
}

# Generate the two distributions for the specified parameters
estimators_A <- generate(n, mean_A, sd_A)
estimators_B <- generate(n, mean_B, sd_B)

# Find the optimal weight to combine the two estimators 
weight <- optimize(var_to_opt, interval = c(0, 1))
weight <- weight$minimum 
cat("Best weight -->", weight)

# Plot the curve for each possible weight
visualize_opt(weight)
# Save the plot generated by visualize_opt
png(file = "Plot/visualize_opt.png")
visualize_opt(weight)
dev.off()

# Weighted estimators C from the previous one
estimators_C <- (1 - weight) * estimators_A + weight * estimators_B

# Calculate the mean and the standard error of Estimators C
mean_C <- mean(estimators_C)
sd_C <- sd(estimators_C)

# Display the results
cat("Mean of estimators C -->", round(mean_C, 2))
cat("Sd of estimators C -->", round(sd_C, 2))

# Put the three estimators in a list
list_est <- list("A" = estimators_A, "B" = estimators_B, "C" = estimators_C)

# Find the 95% c.i. for all the three distributions
positions <- c(0.025, 0.975)

list_ci <- lapply(list_est, function(x) quantile(x, positions))

# Define names for the elements
names(list_ci) <- c("Estimator A", "Estimator B", "Estimator C")

# Plot the estimators' distributions
colors <- c("red", "blue", "black")
display_distributions(list_est, list_ci, colors)
# Save the plot generated by display_distributions
png(file = "Plot/display_distributions.png")
display_distributions(list_est, list_ci, colors)
dev.off()

# Define a list containing the means
mean_list <- list(mean_A, mean_B, mean_C)
names(mean_list) <- c("A", "B", "C")

# Defined a space of hypothetical parameters for the means within the c.i.
true_params <- c(min(unlist(list_ci)),max(unlist(list_ci)))

# Obtain a list of distances
distances <- get_distances(mean_list, true_params)

# Return the intervals for which estimators C is the best
best_C_intervals <- get_best_intervals(true_params, distances, "C")

# Visualize when C is the best estimator
visualize_best_intervals(true_params, distances, "C")
# Save the plot generated by visualize_best_C
png(file = "Plot/visualize_best_C.png")
visualize_best_intervals(true_params, distances, "C")
dev.off()

# Return the area
lower <- best_C_intervals[1]
upper <- best_C_intervals[2]

area <- area_between(lower, upper, mean_C, sd_C)

cat("Density between the interval --> ", round(area,2))

# Clear the workspace
rm(list=ls())