simulate_avalanches = function(n_avalanches = 1000, a = 0.49, max_gen = 200) {
  
  extinct = logical(n_avalanches)  
  
  for (i in 1:n_avalanches) {
    X = 1  
    
    for (gen in 1:max_gen) {
      if (X == 0) break  
      
      Z = rbinom(1, size = X, prob = 1 - a)
      X = 2 * Z  
    }
    
    extinct[i] = (X == 0)
  }
  
  return(extinct)
}


set.seed(42)
a = 0.49
results = simulate_avalanches(n_avalanches = 1000, a = a, max_gen = 200)

p_sim = mean(results)

p_theory = a / (1 - a)

cat("Simulated extinction probability: ", round(p_sim, 4), "\n")
cat("Theoretical extinction probability:", round(p_theory, 4), "\n")
cat("Difference:                        ", round(abs(p_sim - p_theory), 4), "\n")
