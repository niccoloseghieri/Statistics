# Lower and Upper percentiles
lower_percentile <- .25
upper_percentile <- .75

# Lower and Upper estimates for each line
lower_estimate_A <- 90
upper_estimate_A <- 110

lower_estimate_B <- 150
upper_estimate_B <- 300

lower_estimate_C <- 200
upper_estimate_C <- 500

# Vectors of lower and upper
lower_vect <- c(lower_estimate_A, lower_estimate_B, lower_estimate_C)
upper_vect <- c(upper_estimate_A, upper_estimate_B, upper_estimate_C)

# Correlation between the lines
corr_AB <- .5
corr_AC <- .5
corr_BC <- .6

# Vectors of correlations
corr_vect <- c(corr_AB, corr_AC, corr_BC)

# Number of samples
n_samples <- 10^6

# Set the seed
set.seed(42)