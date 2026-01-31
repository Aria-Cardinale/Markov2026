set.seed(1)
n <- 1e6

t3 <- proc.time()
x_sum <- rexp(n, rate = 1) + rexp(n, rate = 1)
runtime_sum <- proc.time() - t3
runtime_sum
