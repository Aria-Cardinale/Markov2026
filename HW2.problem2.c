set.seed(1)
n <- 1e6
c <- 4/exp(1)

t2 <- proc.time()
x_ar <- numeric(0)

while (length(x_ar) < n) {
  y <- rexp(n, rate = 1/2)
  u <- runif(n)
  accept <- u <= (y * exp(-y)) / (c * (0.5 * exp(-y/2)))
  x_ar <- c(x_ar, y[accept])
}
x_ar <- x_ar[1:n]
runtime_ar <- proc.time() - t2
runtime_ar
