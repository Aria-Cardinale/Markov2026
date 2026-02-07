P <- matrix(c(
  9/10, 1/10, 0,
  0,    7/8,  1/8,
  2/5,  0,    3/5
), nrow = 3, byrow = TRUE)

P50 <- P
for (i in 1:49) {
  P50 <- P50 %*% P
}

P50
