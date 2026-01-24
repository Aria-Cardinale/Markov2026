set.seed(123)  # for reproducibility

# values of n
k_vals <- 2:10
n_vals <- 2^k_vals

Nsim <- 1e6  # number of Monte Carlo samples

# function to estimate skewness
estimate_skewness <- function(Y) {
  mean((Y - mean(Y))^3) / sd(Y)
}

# store results
skew_sim <- numeric(length(n_vals))
skew_theory <- n_vals^(-1/2)

# main loop
for (j in seq_along(n_vals)) {
  n <- n_vals[j]
  
  # generate Poisson samples: Nsim x n matrix
  X <- matrix(rpois(Nsim * n, lambda = 1), nrow = Nsim)
  
  # compute Yn
  Y_n <- (rowSums(X) - n) / sqrt(n)
  
  # estimate skewness
  skew_sim[j] <- estimate_skewness(Y_n)
  
  cat("Done n =", n, "\n")
}

# log-log plot
plot(n_vals, skew_sim,
     log = "xy",
     pch = 19,
     xlab = "n",
     ylab = "Skewness",
     main = "Skewness of Yn: Simulation vs Theory")

lines(n_vals, skew_theory, lwd = 2)

legend("upper right",
       legend = c("Simulation", "Theory n^{-1/2}"),
       pch = c(19, NA),
       lwd = c(NA, 2))
