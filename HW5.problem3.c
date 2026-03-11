find_eigen = function(a) {
  P = matrix(c(1-a,  a,   0,
                 a,   0,  1-a,
                 0,  1-a,  a), nrow=3, byrow=TRUE)
  eigen(P)
}

find_eigen(0.5)

a_vals = seq(0.01, 0.99, by=0.01)
eigs = sapply(a_vals, function(a) sort(Re(find_eigen(a)$values), decreasing=TRUE))
eigs

a = 0.99
P = matrix(c(1-a, a, 0,
               a,  0, 1-a,
               0, 1-a, a), nrow=3, byrow=TRUE)

A = t(P)  

e = eigen(A)
lambdas = e$values
V = e$vectors  # columns are eigenvectors

q0 = c(1, 0, 0)


coeffs = solve(V, q0)

n_max = 300
qn1 = numeric(n_max + 1)  

for (n in 0:n_max) {
  qn = Re(V %*% (coeffs * lambdas^n))
  qn1[n + 1] = qn[1]
}
qn
