# Function to generate random samples
# n         --> number of samples to generate
# mean_data --> mean of the data distribution
# sd_data   --> standard deviation of the data distribution

# return    --> n random samples normally distributed with the parameters above
generate = function(n, mean_data, sd_data) rnorm(n = n, mean = mean_data, sd = sd_data)

# Function to generate 2 multivariate distributions
# var_A     --> the variance of estimators A
# var_B     --> the variance of estimators B
# corr      --> the correlation between the two estimators
# means     --> a vector of means (must be 2)
# n_samples --> number of samples to generate

# return    --> a dataframe of the two estimators

generate_data <- function(var_A, var_B, corr, means, n_samples = n){
  
  # Check if the length is valid
  if(length(means) != 2){
    stop("The length of the vectors containing the means must be 2.")
  }
  
  # Build the covariance matrix
  cov_matrix <- matrix(c(var_A, sqrt(var_A * var_B) * corr,
                         sqrt(var_A * var_B) * corr, var_B),
                       nrow = 2,
                       ncol = 2,
                       byrow = TRUE)
  
  # Generate the data
  data <- mvrnorm(n = n_samples, mu = means, Sigma = cov_matrix)
  
  # Convert to dataframe and rename the columns
  data <- as.data.frame(data)
  colnames(data) <- c("Estimators_A", "Estimators_B")
  
  return (data)
}