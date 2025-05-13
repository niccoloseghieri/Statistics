# Define the function to be minimized for combining two estimators
# t    --> weight to assign to estimator B (1 - t to estimator A)
# sd_a --> standard deviation of estimators A
# sd_b --> standard deviation of estimators B

var_to_opt <- function(t, sd_a = sd_A, sd_b = sd_B) {
  (1 - t)^2 * sd_a^2 + t^2 * sd_b^2
}

# Function to compute the distances from hypothetical value of the true parameter and the observed
# parameters of the data
# estimated_params --> is a vector containing the observed parameters
# range            --> the range of the values of the true hypothetical parameter

# return           --> a list of distances

get_distances <- function(estimated_params, range){
  
  lower <- min(range)
  upper <- max(range)
  
  # Generate the hypothetical value of the true parameter from lower to upper with steps of .5
  true_params <- seq(lower, upper, .5)
  
  # Define a function to calculate absolute distances
  calc_abs_distances <- function(mean, params) {
    abs(mean - params)
  }
  
  # Apply the function to each mean in mean_vector using lapply
  distances_list <- lapply(estimated_params, calc_abs_distances, params = true_params)
  
  # Assign names to the list elements using letters A, B, C, etc.
  names(distances_list) <- names(estimated_params)
  
  return(distances_list)
  
}

# Define the function to calculate the intervals where a specified estimator is the best
# range          --> hypothetical (lower and upper) values of the true hypothetical parameters
# distances      --> a list containing the distances for each estimators from the true parameters
# best_estimator --> the names of the estimators for which we want to find where it is the best

# return         --> the extremes where the estimators is the best

get_best_intervals <- function(range, distances, estimator) {
  
  # Ensure the specified estimator is in the distances list
  if (!(estimator %in% names(distances))) {
    stop("The specified estimator is not in the distances list.")
  }
  
  lower <- min(range)
  upper <- max(range)
  
  true_params <- seq(lower, upper, .5)
  
  # Combine the distance vectors into a matrix
  dist_matrix <- do.call(cbind, distances)
  
  # Find which estimator has the smallest distance for each true parameter value
  best_distances <- apply(dist_matrix, 1, which.min)
  
  # Determine the index of the specified estimator
  estimator_index <- which(names(distances) == estimator)
  
  # Identify values of true parameters where the specified estimator is the best
  best_values <- true_params[best_distances == estimator_index]
  
  # Return the extremes (minimum and maximum values) of these true parameters
  if (length(best_values) > 0) {
    extremes <- c(min(best_values), max(best_values))
  } else {
    extremes <- c(NA, NA)  # No interval found where the estimator is the best
  }
  
  return(extremes)
}

# Calculate the area between two values for a normal distribution
# lower  --> the lower value of the interval
# upper  --> the upper value of the interval
# mean.s --> the mean of the normal distribution
# se_s   --> the standard error of the normal distribution

# return --> the area between the two extremes

area_between <- function(lower, upper, mean_s, se_s) {
  area <- pnorm(upper, mean = mean_s, sd = se_s) - pnorm(lower, mean = mean_s, sd = se_s)
  return(area)
}

# Function that given a vector of variances calculate the inverse variance weighting
# variances --> a vector of variances

# return    --> a vector of normalized weights

get_weights <- function(variances){
  
  weights <- 1 / variances
  
  normalized_weights <- weights / sum(weights)
  
  # Print the weights in a nice format
  cat("Normalized Weights:\n")
  for (i in seq_along(normalized_weights)) {
    cat(sprintf("Weight %d: %.4f\n", i, normalized_weights[i]))
  }
    
  return(normalized_weights)
}

# Function to obtain the combined estimators
# df_estimators --> dataframe containing all the estimators to combine
# weights       --> a vector containing the weights for each column/estimator

#return         --> the combined estimator

get_combined <- function(df_estimators, weights){
  
  # Multiply each column by the corresponding element in the vector
  multiplied_df <- sweep(df_estimators, 2, weights, `*`)
  
  # Sum all the columns
  combined_estimator <- rowSums(multiplied_df)
  
  return(combined_estimator)
  
}