library(ggplot2)

simulate_and_plot = function(N, a = 0.99, n_max = 300) {
  P = matrix(c(1-a, a, 0,
                 a,  0, 1-a,
                 0, 1-a, a), nrow=3, byrow=TRUE)
  
  
  states = matrix(1L, nrow=N, ncol=n_max+1)  
  for (n in 1:n_max) {
    for (i in 1:N) {
      s = states[i, n]
      states[i, n+1] = sample(1:3, 1, prob=P[s, ])
    }
  }
  
  fn = colMeans(states == 1)
  
  A = t(P)
  e = eigen(A)
  V = e$vectors
  lambdas = e$values
  coeffs = solve(V, c(1, 0, 0))
  
  qn1_theory = sapply(0:n_max, function(n) {
    Re(sum(V[1, ] * coeffs * lambdas^n))
  })
  
  # Plot
  df = data.frame(
    n = 0:n_max,
    simulated = fn,
    theoretical = qn1_theory
  )
  
  ggplot(df, aes(x=n)) +
    geom_line(aes(y=simulated, color="Simulated"), alpha=0.7) +
    geom_line(aes(y=theoretical, color="Theoretical"), linewidth=1) +
    geom_hline(yintercept=1/3, linetype="dashed", color="gray50") +
    scale_color_manual(values=c("Simulated"="steelblue", "Theoretical"="tomato")) +
    labs(title=paste0("Fraction in State 1 (N=", N, ", a=0.99)"),
         x="n", y="f_n", color="") +
    theme_minimal()
}

# Run for each N
p1 = simulate_and_plot(100)
p2 = simulate_and_plot(1000)
p3 = simulate_and_plot(10000)


library(patchwork)
p1 / p2 / p3
