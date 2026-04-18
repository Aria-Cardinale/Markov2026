Q <- matrix(c(-1,  1,  0,  0,
               0, -1,  1,  0,
               0,  0, -1,  1,
               1,  0,  0, -1), nrow=4, byrow=TRUE)

y0 <- c(1/3, 2/3, 0, 0)

# Eigendecomposition of Q transpose (for left eigenvectors)
eig      <- eigen(t(Q))
V        <- eig$vectors
lambdas  <- eig$values

# Solve for coefficients: V * c = y0  =>  c = V \ y0
c_coeffs <- solve(V, y0)

# Build analytical y1(t)
t_analytical <- seq(0, 5, length.out=1000)

y1_analytical <- sapply(t_analytical, function(t) {
  Re(sum(c_coeffs * V[1, ] * exp(lambdas * t)))
})
