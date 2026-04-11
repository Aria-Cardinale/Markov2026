set.seed(42)

lambda = function(t) 0.5 * (1 + (t / 30)^2)

T_max    = 120
lambda_max = lambda(T_max)   

cat("Step 1: lambda_MAX =", lambda_max, "/day\n")

arrivals = c()
t = 0
repeat {
  t = t + rexp(1, rate = lambda_max)
  if (t > T_max) break
  arrivals = c(arrivals, t)
}
cat("Step 2: HPPP candidates =", length(arrivals), "\n")

keep      = runif(length(arrivals)) < lambda(arrivals) / lambda_max
kept      = arrivals[keep]
cat("Step 3: Kept after thinning =", length(kept), "\n")
cat("Expected total (theory)    = 380\n")
cat("Difference                 =", length(kept) - 380, "\n\n")

hist(kept,
     breaks = 0:T_max,
     freq   = TRUE,
     col    = adjustcolor("#CFB87C", alpha.f = 0.55),
     border = "white",
     xlab   = "Day (t)",
     ylab   = "Reports per day",
     main   = "Simulated flu reports — NHPP thinning algorithm",
     xlim   = c(0, T_max))


t_seq <- seq(0.5, T_max - 0.5, by = 1)
lines(t_seq, lambda(t_seq), col = "#565A5C", lwd = 2)

legend("topleft",
       legend = c("Simulated reports/day", expression(lambda(t) ~ "theory")),
       fill   = c(adjustcolor("#CFB87C", alpha.f = 0.55), NA),
       lty    = c(NA, 1),
       lwd    = c(NA, 2),
       col    = c(NA, "#565A5C"),
       border = c("white", NA),
       bty    = "n")
