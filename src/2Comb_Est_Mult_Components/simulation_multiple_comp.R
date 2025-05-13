# This script emulate section 3. Combining Estimates for Multiple Components 
# from the paper by: Thomas Struppeck, "Combining Estimates".

# In particular it aims to reproduce the results obtained in the table pag. 11 and in addition
# also the total with no correlation in the Excel spreadsheet.
# Difference from the script "multiple_components": here we generate the data.

# Sets the R working directory to the directory where the currently active document in RStudio
# is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load the global variables and functions to make computations 
source("global_variables_2.r")
source("functions_2.r")
source("generate_samples_functions_2.r")

# Load the library MASS for mvrnorm
library(MASS)

# Simulation: return a dataframe  
dataset <- generate_data(lower_percentile, upper_percentile, lower_vect, upper_vect, corr_vect)

# To visualize statistics of the simulated data
custom_summary <- generate_summary(dataset)

# Visualize that the correlation matrix is approximately equal to the original one
cor_matrix <- cor(dataset)
cor_matrix

# Assign correlations
corr_AB <- cor_matrix["Loss_A", "Loss_B"]
corr_AC <- cor_matrix["Loss_A", "Loss_C"]
corr_BC <- cor_matrix["Loss_B", "Loss_C"]

attach(custom_summary)

# Assign the mean for each line
mean_a <- Loss_A[1]
mean_b <- Loss_B[1]
mean_c <- Loss_C[1]

mean_vect <- c(mean_a, mean_b, mean_c)

# Assign the standard deviation for each line
sd_a <- Loss_A[4]
sd_b <- Loss_B[4]
sd_c <- Loss_C[4]

sd_vect <- c(sd_a, sd_b, sd_c)

# Calculate 25 percentile and 75 percentile
perc_vector <- get_perc(mean_vect, sd_vect)

lower_vect <- perc_vector$perc_25
upper_vect <- perc_vector$perc_75

# Build the covariance matrix
matrix <- build_covariance_matrix(sd_vect, cor_matrix)

# Naive totals
naive_totals <- get_total(mean_vect, sd_vect, lower_vect, upper_vect, matrix, 1)

# With covariance adjustment
cov_adj_total <- get_total(mean_vect, sd_vect, lower_vect, upper_vect, matrix, 2)

# No correlation
no_corr_total <- get_total(mean_vect, sd_vect, lower_vect, upper_vect, matrix, 3)

detach(custom_summary)

# Clear the workspace
rm(list=ls())