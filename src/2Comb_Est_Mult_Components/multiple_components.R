# This script emulate section 3. Combining Estimates for Multiple Components 
# from the paper by: Thomas Struppeck, "Combining Estimates".

# In particular it aims to reproduce the results obtained in the table pag. 11 and in addition
# also the total with no correlation in the Excel spreadsheet.

# Sets the R working directory to the directory where the currently active document in RStudio
# is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load the global variables and functions to make computations 
source("global_variables_2.r")
source("functions_2.r")

# Get the mean and the sd for each line using percentile matching
linea_a <- percentile_matching(lower_percentile, upper_percentile, lower_estimate_A, upper_estimate_A)
linea_b <- percentile_matching(lower_percentile, upper_percentile, lower_estimate_B, upper_estimate_B)
linea_c <- percentile_matching(lower_percentile, upper_percentile, lower_estimate_C, upper_estimate_C)

# Extract the mean for each line
mean_a <- linea_a$mean
mean_b <- linea_b$mean
mean_c <- linea_c$mean

mean_vect <- c(mean_a, mean_b, mean_c)

# Extract the sd for each line
sd_a = linea_a$sd
sd_b = linea_b$sd
sd_c = linea_c$sd

sd_vect <- c(sd_a, sd_b, sd_c)

# Calculate 25 percentile and 75 percentile
perc_vector <- get_perc(mean_vect, sd_vect)

lower_vect <- perc_vector$perc_25
upper_vect <- perc_vector$perc_75

# Build the correlation matrix
corr_matrix <- build_corr_matrix(corr_vect)

# Build the covariance matrix
matrix <- build_covariance_matrix(sd_vect, corr_matrix)

# Naive totals
naive_totals <- get_total(mean_vect, sd_vect, lower_vect, upper_vect, matrix, 1)

# With covariance adjustment
cov_adj_total <- get_total(mean_vect, sd_vect, lower_vect, upper_vect, matrix, 2)

# No correlation
no_corr_total <- get_total(mean_vect, sd_vect, lower_vect, upper_vect, matrix, 3)

# Clear the workspace
rm(list=ls())