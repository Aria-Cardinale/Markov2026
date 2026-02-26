# Part 1: Theoretical Solution Function
solve_theoretical <- function(p, q, s, i) {
  # r is root of p*r^2 - r + q = 0
  # r = (1 - sqrt(1 - 4pq)) / (2p)
  r1 <- (1 - sqrt(1 - 4*p*q)) / (2*p)
  
  # Part (a): Prob retire before losing all
  # P_i = 1 - r1^i
  prob_retire <- 1 - r1^i
  
  # Part (b): Expected amount
  # E_i = i + (p-q)/s * (1 - r1^i)
  expected_val <- i + (p-q)/s * (1 - r1^i)
  
  return(list(prob_retire = prob_retire, 
              expected_val = expected_val, 
              r1 = r1))
}

# Part 2: Simulation Function
simulate_game <- function(p, q, s, start_val, trials = 100000) {
  results <- numeric(trials)
  
  for (t in 1:trials) {
    current <- start_val
    while (current > 0) {
      rng <- runif(1) # R equivalent to np.random.random()
      
      if (rng < s) { # Retire
        break
      } else if (rng < s + p) { # Win
        current <- current + 1
      } else { # Lose
        current <- current - 1
      }
    }
    results[t] <- current
  }
  return(mean(results))
}

# Parameters
p <- 0.35
q <- 0.4
s <- 0.25
i <- 10

# Execute
theoretical_res <- solve_theoretical(p, q, s, i)
simulated_E <- simulate_game(p, q, s, i, 100000)

# Print results
cat(sprintf("prob_retire = %f\n", theoretical_res$prob_retire))
cat(sprintf("theoretical_E = %f\n", theoretical_res$expected_val))
cat(sprintf("simulated_E = %f\n", simulated_E))
cat(sprintf("r1 = %f\n", theoretical_res$r1))
