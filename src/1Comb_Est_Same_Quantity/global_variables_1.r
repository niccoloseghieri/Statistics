# Mean and standard deviation for estimator A
mean_A <- 250
sd_A <- 30

# Mean and standard deviation for estimator B
mean_B <- 275
sd_B <- 40

# Set the seed
set.seed(42)

# Mean and standard deviation for estimator D (not in the paper)
mean_D <- sample(220:300, 1)
sd_D <- sample(25:65, 1)

# Mean and standard deviation for estimator E (not in the paper)
mean_E <- sample(220:300, 1)
sd_E <- sample(25:65, 1)

# Number of samples to generate
n <- 10^6

# Constant
constant <- 100