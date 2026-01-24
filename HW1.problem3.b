set.seed(123)
f = function(x) {
  return(x^4 / (x^6 +1))
}

x_values = seq(1, 5, by = 0.1)
N_values = floor(10^x_values)

# Initialize a vector to store the estimates E(N)
estimates = numeric(length(N_values))

# Run the simulation for each N
for (i in 1:length(N_values)) {
  N = N_values[i]
  # Sample N points uniformly in the square [0, 1] x [0, 1]
  x = runif(N, min = 0, max = 1)
  y = runif(N, min = 0, max = 1) # y values from 0 to 1

  # Check which points are below the curve y = f(x)
  below_curve = y < f(x)
  fraction_below = sum(below_curve) / N

  # The integral estimate E(N) is the fraction (area of square is 1*1=1)
  estimates[i] = fraction_below
}
fraction_below

plot(N_values, estimates,
     log = "x",
     xlab = "N (log scale)",
     ylab = "Estimate E(N)",
     main = "Monte Carlo Estimate of Integral vs. N")
abline(h = fraction_below, col = "red")
lines(N_values, estimates)
grid()
