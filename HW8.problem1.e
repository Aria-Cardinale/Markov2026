N_values <- c(100, 1000, 10000, 100000)
t_eval   <- seq(0, 5, length.out=500)
next_state <- function(s) (s %% 4) + 1   # 1->2->3->4->1

par(mfrow=c(2,2), mar=c(4,4,3,1))

for (N in N_values) {
  
  # State 1 w.p. 1/3, state 2 w.p. 2/3
  states <- ifelse(runif(N) < 1/3, 1, 2)
  
  max_jumps  <- 50
  hold_times <- matrix(rexp(N * max_jumps, rate=1), nrow=N, ncol=max_jumps)
  jump_times <- t(apply(hold_times, 1, cumsum))   
  
  f_t <- sapply(t_eval, function(t) {
       
    n_jumps <- rowSums(jump_times <= t)
      
    current_states <- (states - 1 + n_jumps) %% 4 + 1
    
    mean(current_states == 1)
  })
  
  plot(t_eval, f_t,
       type='l', col='blue', lwd=1.2,
       xlim=c(0,5), ylim=c(0,0.5),
       xlab='t', ylab='f(t)',
       main=paste('N =', formatC(N, format='d', big.mark=',')))
  
  lines(t_analytical, y1_analytical, col='red',   lwd=2)
  abline(h=0.25,                                  col='black', lwd=1, lty=2)
  
  legend('topright',
         legend=c('Simulation f(t)', 'Analytical y1(t)', 'pi = 1/4'),
         col=c('blue','red','black'),
         lty=c(1,1,2), lwd=c(1.2,2,1),
         cex=0.8)
  
  grid()
}

mtext('Fraction of Chains in State 1 vs. Theoretical y1(t)',
      side=3, line=-1.5, outer=TRUE, cex=1.2, font=2)
