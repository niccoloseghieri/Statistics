# Function to generate dataset with any number of loss lines using constraints about correlation 
# lower_percentile --> the lower percentile of a distribution
# upper_percentile --> the upper percentile of a distribution
# lower            --> the value associated to the lower_percentile
# upper            --> the value associated to the upper_percentile
# corr_vector      --> correlations between the lossess
# n_sample         --> the number of samples to generate, default 10^6

# return           --> dataframe

generate_data <- function(lower_percentile, 
                          upper_percentile, 
                          lower, 
                          upper, 
                          corr_vect, 
                          n_sample = n_samples) {
  
  # Get the number of loss lines
  num_losses <- length(lower)
  
  # Calculate mean and standard deviation for each loss line
  means <- numeric(num_losses)
  sds <- numeric(num_losses)
  for (i in 1:num_losses) {
    line <- percentile_matching(lower_percentile, upper_percentile, lower[i], upper[i])
    means[i] <- line$mean
    sds[i] <- line$sd
  }
  
  # Build the correlation matrix
  corr_matrix <- build_corr_matrix(corr_vect)
  
  # Build the covariance matrix
  cov_matrix <- build_covariance_matrix(sds, corr_matrix)
  
  # Simulate the data
  data <- mvrnorm(n = n_samples, mu = means, Sigma = cov_matrix)
  
  # Convert to a data frame
  df <- as.data.frame(data)
  colnames(df) <- paste0("Loss_", LETTERS[1:num_losses])
  
  return(df)
}