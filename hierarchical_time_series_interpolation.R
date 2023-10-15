# Load required libraries
library(dplyr)
library(tidyr)
library(MASS)

# Set the number of time steps
n_steps <- 100
# Set the number of random walks
n_walks <- 100

# Generate random walks with the same mean and variance
set.seed(123)  # for reproducibility
cov_matrix <- diag(n_walks)
cov_matrix[cov_matrix == 0] <- 0.6
random_walks <- mvrnorm(n = n_steps, mu = rep(0, n_walks), Sigma = cov_matrix)
random_walks = apply(random_walks, 2, cumsum)

intercepts = rnorm(n_walks,0,5)
for (i in 1:ncol(random_walks)){
  random_walks[,i] = random_walks[,i] + intercepts[i]
}

# Create a data frame in wide format
wide_df <- data.frame(matrix(random_walks, ncol = n_walks))
colnames(wide_df) <- paste0("x", 1:n_walks)
wide_df$t <- 1:n_steps


# Plot the time series
matplot(as.matrix(wide_df[,-ncol(wide_df)]), type = "l", xlab = "Time", ylab = "Value", main = "Time Series Lines")
# Add a legend (optional)
legend("topright", legend = 1:n, col = 1:n, lty = 1, title = "Time Series")

# Convert to long format using tidyr
### Pivot
long_df <- wide_df %>%
  pivot_longer(cols = starts_with("x"),
               names_to = "level",
               values_to = "value")


# Print the long format data frame
print(long_df)

######
library(splines)

# Number of knots
n_knots <- 10

# Generate equally spaced knots
#knots <- seq(min(long_df$t), max(long_df$t), length = n_knots)

# Create the basis matrix in wide format
#basis_matrix_wide <- data.frame(
#  time = 1:100,
#  bs(c(1:100), knots = knots, degree = 1)
#)

# Define the piecewise linear function
piecewise_linear <- function(x, k) {
  y <- x - k
  y <- ifelse(x < 0, 0, y)
  y <- ifelse(x < k, 0, y)
  return(y)
}

# Define a function to create a linear spline basis
create_basis <- function(x, k) {
  num_x <- length(x)
  num_k <- length(k)
  b <- matrix(0, nrow = num_x, ncol = num_k)
  for (i in 1:num_k) {
    b_i <- piecewise_linear(x, k[i])
    b[, i] <- b_i
  }
  return(b)
}

# Example usage
x <- seq(1, n_steps, by = 1)  # Sample x values
k_values <- c(0:n_knots)*(n_steps/n_knots)   # Values of k
basis_matrix_wide <- create_basis(x, k_values)
basis_matrix_wide = basis_matrix_wide/max(basis_matrix_wide)
colnames(basis_matrix_wide) = paste0("basis_X", c(1:11))

#colnames(basis_matrix_wide) = paste0("basis_",colnames(basis_matrix_wide))
t = c(1:n_steps)
basis_matrix_wide = cbind(t, basis_matrix_wide)
basis_matrix_wide = as.data.frame(basis_matrix_wide)
#colnames(basis_matrix_wide)[1] = "time"

## Merge the dataframes
long_df = long_df %>% left_join(basis_matrix_wide, by='t')



library("lme4")

# Now try filtering out all but a handful of data points from specific series and then retrain the model, predict again.
# Keep only 3 data points at x10 and retrain
drop = which(long_df$level == "x10")
drop = drop[-c(1,5, 25, 91)]

sample_df = long_df[-drop,]

create_formula = function(num_basis, target = "value"){
  
  f = paste0(target, "  ~ (1 | level) + ")
  for (i in 1:num_basis){
    f = paste0(f, "(basis_X",i, " - 1 | level) + basis_X",i)
    if (i < num_basis){
      f = paste0(f, " + ")
    }
  }
  f = as.formula(f)
  return(f)
}
f = create_formula(10)
lm_fit = lmer(f, data=sample_df)

y_p_train = predict(lm_fit)
sample_df$y_p = y_p_train

for (x in unique(sample_df$level)){
  x1 = sample_df %>% filter(level == x)
  plot(x1$value, ylim=c(-15,15))
  lines(x1$y_p, col="red")
  Sys.sleep(1)
}


# out of sample
y_p_test = predict(lm_fit, newdata=long_df)
long_df$y_p = y_p_test

for (x in unique(long_df$level)){
  x1 = long_df %>% filter(level == x)
  print(x)
  plot(x1$value)
  lines(x1$y_p, col="red")
  Sys.sleep(1)
}


# Check the random effects and the fixed effects
rf = ranef(lm_fit)
fixef(lm_fit)




