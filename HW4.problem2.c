P = P <- matrix(c(
    0, 0, 0, 0, 0, 1,
    0.1, 0.9, 0, 0, 0, 0,
    0.01, 0.18, 0.81, 0, 0, 0, 
    0.001, 0.027, 0.243, 0.729, 0, 0,
    0.001, 0.0036, 0.0486, 0.2916, 0.6561, 0,
    0.00001, 0.00045, 0.0081, 0.0729, 0.3281, 0.5905
), nrow = 6, byrow = TRUE)

# Find eigenvalues and eigenvectors
ev <- eigen(t(P))

# Extract the eigenvector corresponding to eigenvalue 1
# (Usually the first one, but we use 'which.max' to be safe)
stationary <- Re(ev$vectors[, which.max(Re(ev$values))])

# Normalize so the sum of probabilities equals 1
stationary <- stationary / sum(stationary)

# Name the states for clarity
names(stationary) <- 0:5
print(stationary["1"])

