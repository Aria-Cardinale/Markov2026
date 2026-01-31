hist(x_sum, prob = TRUE, breaks = 100,
     main = "Gamma(2,1) Samples",
     xlab = "x")

curve(x * exp(-x), from = 0, to = max(x_sum),
      col = "red", lwd = 2, add = TRUE)
