
L <- 20
alpha <- 1
beta <- 1
N <- 1000  

simulate_transport <- function(L, alpha, beta) {
  pos <- 0
  total_time <- 0
  
  while(pos < L) {
   
    if (pos == 0) {
      rate <- alpha
    } else {
      rate <- alpha + beta
    }
    
    total_time <- total_time + rexp(1, rate)
    
    if (pos == 0) {
      pos <- 1
    } else {
   
      if (runif(1) < alpha / (alpha + beta)) {
        pos <- pos + 1
      } else {
        pos <- pos - 1
      }
    }
  }
  return(total_time)
}


set.seed(42) 

transport_times <- replicate(N, simulate_transport(L, alpha, beta))

# --- Analysis ---
sim_mean <- mean(transport_times)
sim_var  <- var(transport_times)
theoretical_mean <- (L * (L + 1)) / (2 * alpha)

cat("Theoretical Mean:", theoretical_mean, "\n")
cat("Simulated Mean:  ", round(sim_mean, 2), "\n")
cat("Simulated Var:   ", round(sim_var, 2), "\n")



hist(transport_times, breaks=50, col="skyblue", main="Distribution of Transport Times",
     xlab="Time (t)", border="white")
abline(v = theoretical_mean, col="red", lwd=2, lty=2)
