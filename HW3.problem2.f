set.seed(123)

states <- c("G", "S", "D")
n <- 10000

# transition matrix
P <- matrix(c(
  9/10, 1/10, 0,
  0,    7/8,  1/8,
  2/5,  0,    3/5
), nrow = 3, byrow = TRUE)

# encode states as 1=G, 2=S, 3=D
current <- 1
chain <- numeric(n)

for (i in 1:n) {
  chain[i] <- current
  current <- sample(1:3, size = 1, prob = P[current, ])
}

# fraction of time in G
mean(chain == 1)
