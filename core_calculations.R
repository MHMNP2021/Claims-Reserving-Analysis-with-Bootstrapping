# Required packages
library(readxl)
library(moments) # For skewness/kurtosis

# Step 01 - Load Data ----
claim.triangle <- readxl::read_excel("C:\\Users\\Use\\Documents\\Campus\\MSM Week\\Low Volatility.xlsx")

# Prepare the cumulative triangle
cum_triangle <- as.matrix(claim.triangle[, -1])
rownames(cum_triangle) <- claim.triangle$`Origin Period`
n_orig <- nrow(cum_triangle)
n_dev <- ncol(cum_triangle)

# Step 02 - Calculate Development Factors ----
compute_factors <- function(triangle) {
  n_dev <- ncol(triangle)
  factors <- numeric(n_dev - 1)
  for (k in 1:(n_dev - 1)) {
    later <- triangle[, k + 1]
    prior <- triangle[, k]
    mask <- !is.na(later) & !is.na(prior)
    if (sum(mask) == 0 || sum(prior[mask]) == 0) {
      factors[k] <- 1
    } else {
      factors[k] <- sum(later[mask]) / sum(prior[mask])
    }
  }
  return(factors)
}
factors <- compute_factors(cum_triangle)
print(round(factors, 4))

# Convert to Incremental for Residual Calculation
get_incremental <- function(cum_tri) {
  inc_tri <- cum_tri
  n <- ncol(cum_tri)
  for(j in n:2) {
    inc_tri[, j] <- cum_tri[, j] - cum_tri[, j-1]
  }
  return(inc_tri)
}
inc_triangle <- get_incremental(cum_triangle)
inc_triangle

# Development Ratios (Age-to-Age Factors)

calculate_factors <- function(tri) {
  n <- ncol(tri)
  factors <- numeric(n - 1)
  for(j in 1:(n-1)) {
    # Sum of C(i, k+1) / Sum of C(i, k) for observed data
    idx <- which(!is.na(tri[, j+1]))
    sum_curr <- sum(tri[idx, j])
    sum_next <- sum(tri[idx, j+1])
    factors[j] <- sum_next / sum_curr
  }
  return(factors)
}
dev_factors <- calculate_factors(cum_triangle)
print(dev_factors)

# Produce Best Estimate (Deterministic Chain Ladder)

project_triangle <- function(tri, factors) {
  n_rows <- nrow(tri)
  n_cols <- ncol(tri)
  full_tri <- tri
  for(i in 1:n_rows) {
    for(j in 1:(n_cols-1)) {
      if(is.na(full_tri[i, j+1])) {
        
        full_tri[i, j+1] <- full_tri[i, j] * factors[j]
      }
    }
  }
  return(full_tri)
}
completed_tri_be <- project_triangle(cum_triangle, dev_factors)
# Calculate Best Estimate Reserve (Ultimate - Latest Cumulative)
latest_diagonal <- apply(cum_triangle, 1, function(x) tail(na.omit(x), 1))
ultimate_values <- completed_tri_be[, ncol(completed_tri_be)]
be_reserves <- ultimate_values - latest_diagonal
total_be <- sum(be_reserves)
print(paste("Deterministic Best Estimate Reserve:", comma(total_be)))

# Calculate Model Errors (Residuals)

# 1. Calculate Fitted Cumulative (Backward Calculation)
# fitted(i, j) = fitted(i, j-1) * f(j-1)
# But strictly for ODP Bootstrap, we usually re-fit the triangle using the
factors
# backwards from the latest diagonal to get 'm' (fitted incremental).
calculate_fitted_incremental <- function(cum_tri, factors) {
  n <- ncol(cum_tri)
  fitted_cum <- cum_tri
  # A simple approximation for Fitted Values:
  # Using the diagonals and running the factors backwards
  row_diags <- apply(cum_tri, 1, function(x) tail(na.omit(x), 1)) # Latest
  known
  col_len <- apply(cum_tri, 1, function(x) length(na.omit(x))) # How many
  cols
  # Fill backwards
  for(i in 1:nrow(cum_tri)) {
    curr_col <- col_len[i]
    if (curr_col > 1) {
      for(j in (curr_col-1):1) {
        fitted_cum[i, j] <- fitted_cum[i, j+1] / factors[j]
      }
    }
  }
  # Fill forwards (standard CL)
  fitted_cum <- project_triangle(fitted_cum, factors)
  
  # Convert to Incremental
  fitted_inc <- get_incremental(fitted_cum)
  return(fitted_inc)
}
fitted_inc <- calculate_fitted_incremental(cum_triangle, dev_factors)
fitted_inc

# 2. Calculate Residuals
# Pearson Residual = (Actual - Fitted) / sqrt(Fitted)
# We use Pearson to standardize variance (heteroscedasticity) as required for
bootstrapping.
residuals_tri <- (inc_triangle - fitted_inc) / sqrt(abs(fitted_inc))
# Store valid residuals in a list for resampling (exclude NAs and extreme
outliers)
resampling_pool <- na.omit(as.vector(residuals_tri))
# Bootstrapping Loop

N_SIMULATIONS <- 5000 # As per Step 6
simulated_reserves <- numeric(N_SIMULATIONS)
set.seed(123) # For reproducibility
for(s in 1:N_SIMULATIONS) {
  # 1. Create Pseudo-History
  # Sample residuals with replacement
  pseudo_resid <- matrix(sample(resampling_pool, length(inc_triangle),
                                replace = TRUE),
                         nrow=nrow(inc_triangle))
  # Calculate Pseudo-Incremental: Fit + Resid * sqrt(Fit)
  pseudo_inc <- fitted_inc + pseudo_resid * sqrt(abs(fitted_inc))
  # Convert to Pseudo-Cumulative
  pseudo_cum <- pseudo_inc
  for(j in 2:ncol(pseudo_cum)) {
    pseudo_cum[, j] <- pseudo_cum[, j-1] + pseudo_inc[, j]
  }
  # Mask the future (keep only the upper triangle observed structure)
  pseudo_cum[is.na(cum_triangle)] <- NA
  # 2. Recalculate Factors (Parameter Risk)
  # Handle cases where denominator might be 0 in simulation
  
  tryCatch({
    sim_factors <- calculate_factors(pseudo_cum)
    # 3. Re-project Future Claims
    sim_full_tri <- project_triangle(pseudo_cum, sim_factors)
    # 4. Sum Future Increments (Reserve)
    sim_latest <- apply(pseudo_cum, 1, function(x) tail(na.omit(x), 1))
    sim_ultimate <- sim_full_tri[, ncol(sim_full_tri)]
    sim_reserve <- sum(sim_ultimate - sim_latest)
    simulated_reserves[s] <- sim_reserve
  }, error = function(e) {
    simulated_reserves[s] <- NA # Skip failed simulations
  })
}
# Clean NAs if any
simulated_reserves <- na.omit(simulated_reserves)
# Build Distribution and Metrics

# Calculate Percentiles
percentiles <- quantile(simulated_reserves, probs = c(0.5, 0.75, 0.80, 0.90,
                                                      0.95, 0.995))
mean_reserve <- mean(simulated_reserves)
sd_reserve <- sd(simulated_reserves)
# Plotting the Distribution
hist(simulated_reserves, breaks=50, col="skyblue", border="white",
     main="Distribution of Simulated Reserves (Bootstrapped)",
     xlab="Reserve Amount", freq=FALSE)
lines(density(simulated_reserves), col="red", lwd=2)
abline(v = mean_reserve, col="blue", lwd=2, lty=2) # Mean
abline(v = percentiles["75%"], col="orange", lwd=2, lty=2) # 75th Percentile

# Tail Value at Risk (TVaR) / CTE (Conditional Tail Expectation)
threshold_80 <- percentiles["80%"]
tail_values <- simulated_reserves[simulated_reserves > threshold_80]
tvar_80 <- mean(tail_values)
risk_adjustment_tvar <- tvar_80 - mean_reserve





