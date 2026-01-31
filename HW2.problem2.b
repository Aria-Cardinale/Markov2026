F_inv <- function(u) {
  f <- function(x) exp(-x) * (x + 1) - (1 - u)
  uniroot(f, lower = 0, upper = 50)$root
}

set.seed(1)
n <- 1e6
u <- runif(n)

t1 <- proc.time()
x_inv <- sapply(u, F_inv)
runtime_inv <- proc.time() - t1
runtime_inv
