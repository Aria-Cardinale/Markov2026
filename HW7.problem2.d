n_sims = 10^5
lambda = 3
t = 48

N = rpois(n_sims, 2 * lambda * t)

n_a = rbinom(n_sims, N, 0.5)
n_b = N - n_a

D = 2 * (n_a - n_b)

est_mean = mean(D)
est_var  = var(D)
est_tie  = mean(D == 0)


theo_mean = 0
theo_var  = 8 * lambda * t
theo_tie  = exp(-2 * lambda * t) * besselI(2 * lambda * t, 0)

cat(sprintf("Mean: Sim=%.4f, Theo=%.4f\n", est_mean, theo_mean))
cat(sprintf("Var:  Sim=%.4f, Theo=%.4f\n", est_var, theo_var))
cat(sprintf("Tie:  Sim=%.4f, Theo=%.4f\n", est_tie, theo_tie))
