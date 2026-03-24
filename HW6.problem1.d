P <- matrix(c(
  0, 1, 0, 0, 0,
  1/3, 0, 2/3, 0,  0,
  0, 1/2, 0, 1/2, 0,
  0, 0, 2/3, 0, 1/3,
  0, 0, 0, 1, 0
), nrow = 5, byrow = TRUE)

q = c(0, 0, 1, 0, 0)

q_sum = q  

for (n in 1:49) {
  q = q %*% P        
  q_sum = q_sum + q  
}

q_bar50 = q_sum / 50  


pi_stat = c(1, 3, 4, 3, 1) / 12

cat("q_bar_50: ", round(q_bar50, 6), "\n")
cat("pi:       ", round(pi_stat, 6), "\n")

states = 0:4

plot(states, pi_stat, type="b", col="blue", pch=16, lty=2,
     ylim=c(0, 0.4), xlab="State i", ylab="Probability",
     main=expression(paste("Comparison of ", bar(q)[50](i), " and ", pi(i))))

lines(states, q_bar50, type="b", col="red", pch=17, lty=1)

legend("topright",
       legend=c(expression(bar(q)[50](i)), expression(pi(i))),
       col=c("red", "blue"), pch=c(17, 16), lty=c(1, 2))
