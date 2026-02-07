f <- function(x) (1/3) * x * (1 + x) * exp(-x)
g <- function(x, a) a^2 * x * exp(-a * x)

a_star <- 0.5
c_star <- exp(-a_star) / (3 * a_star^2 * (1 - a_star))

# Grid
x <- seq(0, 15, length.out = 1000)

# Plot
plot(x, f(x), type = "l", lwd = 2,
     ylab = "Density", xlab = "x",
     main = "f(x) and c(a*) g_a*(x)")
lines(x, c_star * g(x, a_star), col = "red", lwd = 2)
legend("topright",
       legend = c("f(x)", "c(a*) g_a*(x)"),
       col = c("black", "red"),
       lwd = 2)
