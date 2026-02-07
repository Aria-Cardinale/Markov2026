nsims = 1000
steps = 5
start = 1
end = 4
results_x5 = replicate(nsims, {
  current_state = start
  for (t in 1:steps) {
    # Sample next state based on current state's transition probabilities
    current_state = sample(1:nrow(B), size = 1, prob = B[current_state, ])
  }
  current_state
})
fraction = sum(results_x5 == end) / nsims
print(fraction)
