gamma <- 4
x0 <- 10
c_val <- (1 - gamma)*(x0^gamma-1) 

inverse_cdf <- function(u, gamma, x0, c_val) {
  term <- (u * (1 - gamma)) / c_val + x0^(1 - gamma)
   x <- term^(1 / (1 - gamma))
  return(x)
}

c_val_pdf <- (gamma - 1) / x0^(1 - gamma) # Set c for a valid PDF

theoretical_pdf <- function(x, gamma, c_val_pdf) {
  if (x < x0) {
    return(0)
  } else {
    return(c_val_pdf * x^(-gamma))
  }
}

vec_theoretical_pdf <- Vectorize(theoretical_pdf, "x")


generate_and_plot <- function(N) {
  
  u_samples <- runif(N, min = 0, max = 1)
  
  x_samples <- inverse_cdf(u_samples, gamma, x0, c_val_pdf) 

 
  hist(x_samples, 
       freq = FALSE, 
       xlim = c(0, 60), 
       breaks = 50, 
       main = paste("Histogram of X samples (N =", N, ") with Theoretical PDF"), 
       xlab = "X values", 
       ylab = "Density",
       col = "lightblue", 
       border = "white")
   
  curve(vec_theoretical_pdf(x, gamma, c_val_pdf), 
        from = x0, to = 60, 
        col = "red", 
        lwd = 2, 
        add = TRUE)
  
  legend("topright", 
         legend = c("Sample Density", "Theoretical PDF"), 
         col = c("lightblue", "red"), 
         lwd = c(1, 2), 
         fill = c("lightblue", NA),
         border = c("white", NA))
}


par(mfrow = c(1, 3)) 

generate_and_plot(100)
generate_and_plot(1000)
generate_and_plot(10000)
