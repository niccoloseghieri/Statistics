# Function to compute the mean and standard deviation using: Method-of-percentile-matching
# for a normal distribution
# lower_percentile --> the lower percentile of a distribution
# upper_percentile --> the upper percentile of a distribution
# lower            --> the value associated to the lower_percentile
# upper            --> the value associated to the upper_percentile

# return           --> a list with the mean and standard deviation found

percentile_matching = function(lower_percentile,
                               upper_percentile,
                               lower,
                               upper){
  
  lower_percentile <- qnorm(lower_percentile)
  upper_percentile <- qnorm(upper_percentile)
  
  sd_line <- (upper - lower) / (upper_percentile - lower_percentile)
  
  mean_line <- lower - sd_line * lower_percentile
  
  mean_and_sd = list(mean = mean_line, sd = sd_line)
  
  return (mean_and_sd)
  
}

# Function that extract the 1st quartile (25 percentile) and 3rd quartile 
# (75 percentile) of a normal distribution
# vect_mean --> vector of the mean
# vect_sd   --> vector of the standard deviation

# return    --> a list containing the lower and upper percentile

get_perc <- function(vect_mean, vect_sd) {
  # Calculate the 25th percentile (1st quartile)
  perc_25 <- qnorm(0.25, mean = vect_mean, sd = vect_sd)
  
  # Calculate the 75th percentile (3rd quartile)
  perc_75 <- qnorm(0.75, mean = vect_mean, sd = vect_sd)
  
  # Return a list with both percentiles
  return(list(perc_25 = perc_25, perc_75 = perc_75))
}


# Function to build a correlation matrix from a vector of correlations
# corr_vector --> a vector of correlations

# return      --> the correlation matrix

build_corr_matrix <- function(corr_vector) {
  # Determine the size of the matrix
  # The length of the vector should be n*(n-1)/2 for an n x n matrix (unique off-diagonal elements)
  # Solve n*(n-1)/2 = k (the length of corr_vector) lead to the solution of simple quadratic equation
  n <- (1 + sqrt(1 + 8 * length(corr_vector))) / 2
  
  if (n != floor(n)) {
    stop("The length of the correlation vector is not valid for a correlation matrix.")
  }
  
  n <- as.integer(n)
  
  # Initialize an n x n matrix with 1s on the diagonal
  corr_matrix <- diag(1, n, n)
  
  # Indices for the upper triangular part (excluding diagonal)
  upper_indices <- which(upper.tri(corr_matrix))
  
  # Fill the upper triangular part with the given correlations
  corr_matrix[upper_indices] <- corr_vector
  
  # Mirror the upper triangular part to the lower triangular part
  corr_matrix <- t(corr_matrix) + corr_matrix - diag(1, n, n)
  
  return(corr_matrix)
}

# Take the standard deviations of the three lines and as output give the covariance matrix
# sd_vect     --> a vector of standard deviation
# corr_matrix --> correlation matrix

# return  --> the covariance matrix

build_covariance_matrix <- function(sd_vect,
                                    corr_matrix){
  
  cov_matrix <- diag(sd_vect) %*% corr_matrix %*% diag(sd_vect)
  
  return(cov_matrix)
}

# Function that calculate the statistics for each of the three different methods used
# in Excel spreadsheet
# mean_lines   --> vector containing the mean for each line
# sd_lines     --> vector containing the sd for each line
# l_vector     --> vector containing the lower values (.25 percentile values) for each line
# u_vector     --> vector containing the upper values (.75 percentile values) for each line
# cov_matrix   --> the covariance matrix
# option       --> a number between 1-3 

#return        --> depending on the value of 'option' return:
#                  1: the naive totals
#                  2: the covariance-adjusted total
#                  3: the total not considering the correlation

# the function when called also print the results in a nice format

get_total <- function(mean_lines,
                      sd_lines,
                      l_vector,
                      u_vector,
                      cov_matrix,
                      option = 1){
  
  # Check if the option is correct, raise an error message otherwise
  if (!option %in% 1:3) {
    stop("Invalid option. Please choose a value between 1 and 3.")
  }
  
  # 1 --> naive totals a.k.a. comonotone
  if (option == 1){
    
    total_mean <- sum(mean_lines)
    
    lower_perc <- sum(l_vector) 
    upper_perc <- sum(u_vector)
    
    total_sd   <- percentile_matching(.25, .75, lower_perc, upper_perc) 
    total_sd   <- total_sd$sd
    
    cv         <- total_sd / total_mean
  
  }
  
  # 2 --> covariance adjustment total
  if (option == 2){
    
    sd_A <- sd_lines[1]
    sd_B <- sd_lines[2]
    sd_C <- sd_lines[3]
    
    total_mean <- sum(mean_lines)
    
    total_sd   <- sqrt(sum(cov_matrix))
    
    lower_perc <- qnorm(.25, mean = total_mean, sd = total_sd) 
    upper_perc <- qnorm(.75, mean = total_mean, sd = total_sd) 
    
    cv         <- total_sd / total_mean
    
  }
  
  # 3 --> no correlation
  if (option == 3){
    
    total_mean <- sum(mean_lines)
    
    # Calculate as the sum of the square root of the element-wise multiplication
    total_sd   <- sqrt(sum(sd_lines * sd_lines))
    
    lower_perc <- qnorm(.25, mean = total_mean, sd = total_sd) 
    upper_perc <- qnorm(.75, mean = total_mean, sd = total_sd) 
    
    cv         <- total_sd / total_mean
    
  }

  # Printing the results nicely
  cat("Results:\n")
  cat("---------\n")
  cat(sprintf("Total Mean: %.2f\n", total_mean))
  cat(sprintf("25th Percentile: %.2f\n", lower_perc))
  cat(sprintf("75th Percentile: %.2f\n", upper_perc))
  cat(sprintf("Total Standard Deviation: %.2f\n", total_sd))
  cat(sprintf("Coefficient of Variation: %.4f\n", cv))
  cat("---------\n")
  
  output     <- list(mean = total_mean,
                     sd = total_sd, 
                     lower_perc_loss = lower_perc,
                     upper_perc_loss = upper_perc,
                     cv = cv)
  return (output)
}

# Function to generate summary statistics
# data    --> take the dataframe with losses

# return  --> a dataframe the containing a summary of all the statistics for each loss
# the function when called also print the results in a nice format

generate_summary <- function(data) {
  
  # Identify the columns in the dataframe that are considered "loss" columns
  loss_columns <- names(data)
  
  # Create an empty dataframe to store summary statistics
  summary_stats <- data.frame(
    Statistic = c("Expected Loss",
                  "25th Percentile",
                  "75th Percentile",
                  "Standard Deviation",
                  "Estimated CV")
  )
  
  # Initialize the dataframe with NA values for each loss column
  for (loss in loss_columns) {
    summary_stats[[loss]] <- NA
  }
  
  # Calculate the summary statistics for each loss column
  for (loss in loss_columns) {
    expected_loss <- mean(data[[loss]], na.rm = TRUE)
    percentile_25 <- quantile(data[[loss]], 0.25, na.rm = TRUE)
    percentile_75 <- quantile(data[[loss]], 0.75, na.rm = TRUE)
    sd_est <- sd(data[[loss]], na.rm = TRUE)
    cv_est <- sd_est / expected_loss
    
    summary_stats[summary_stats$Statistic == "Expected Loss", loss] <- expected_loss
    summary_stats[summary_stats$Statistic == "25th Percentile", loss] <- percentile_25
    summary_stats[summary_stats$Statistic == "75th Percentile", loss] <- percentile_75
    summary_stats[summary_stats$Statistic == "Standard Deviation", loss] <- sd_est
    summary_stats[summary_stats$Statistic == "Estimated CV", loss] <- cv_est
  }
  
  print(summary_stats)
  return(summary_stats)
}