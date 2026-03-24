simulate_avalanche = function() {
  total <- 0
  current_gen <- 1  
  
  while (current_gen > 0) {
    total <- total + current_gen
    # each particle independently produces 0 or 2 offspring
    offspring <- sample(c(0, 2), size = current_gen, replace = TRUE, prob = c(0.5, 0.5))
    current_gen <- sum(offspring)
  }
  
  return(total)
}


N = 1000
avalanches = replicate(N, simulate_avalanche())

p3_estimate = mean(avalanches == 3)
cat(sprintf("Simulated P(S=3) ≈ %.4f\n", p3_estimate))
cat(sprintf("Theoretical p_3  = %.4f\n", 1/8))
