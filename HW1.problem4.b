N = 10^5
# Simulate 3 friends' arrival times (uniform between 0 and 1 hour from 6 PM)
arrivals = matrix(runif(3 * N), nrow = N, ncol = 3)
# T is the maximum arrival time for each simulation
T_values = apply(arrivals, 1, max)
# Theoretical PDF function: f(t) = 3t^2 for t in [0, 1]
theoretical_pdf = function(t) {
  ifelse(t >= 0 & t <= 1, 3 * t^2, 0)
}
# Plot histogram and superimpose PDF
hist(T_values, breaks = 50, freq = FALSE, main = "Simulated vs Theoretical PDF of T", xlab = "Time (hours from 6 PM)", ylab = "Density", col = "lightblue", border = "black")
curve(theoretical_pdf, from = 0, to = 1, add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Simulation Histogram", "Theoretical PDF"), col = c("lightblue", "red"), lwd = c(NA, 2), pch = c(22, NA))
