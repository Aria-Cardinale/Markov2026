set.seed(123)

k_vals <- 2:10
n_vals <- 2^k_vals
Nsim <- 1e6

estimate_skewness <- function(Y) {
  mean((Y - mean(Y))^3) / sd(Y)
}

skew_sim <- numeric(length(n_vals))
skew_theory <- n_vals^(-1/2)

for (j in seq_along(n_vals)) {
  n <- n_vals[j]
  
  # Directly simulate the sum using Poisson additivity
  S_n <- rpois(Nsim, lambda = n)
  
  # Normalized variable
  Y_n <- (S_n - n) / sqrt(n)
  
  skew_sim[j] <- estimate_skewness(Y_n)
  
  cat("Finished n =", n, "\n")
}

# Log-log plot
plot(n_vals, skew_sim,
     log = "xy",
     pch = 19,
     xlab = "n",
     ylab = "Skewness",
     main = "Skewness of Yn")

lines(n_vals, skew_theory, lwd = 2)

legend("upper right",
       legend = c("Simulation", "Theory n^{-1/2}"),
       pch = c(19, NA),
       lwd = c(NA, 2))
