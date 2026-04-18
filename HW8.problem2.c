Q = matrix(c(-2,  1,  1,
               1, -1,  0,
               1,  0, -1), 
            nrow = 3, byrow = TRUE)

decomp = eigen(t(Q)) 

calc_Pf <- function(t) {
  (1/3) * (1 - exp(-3 * t))
}

t_val <- 2
numeric_sol <- solve_at_t(t_val, p0, Q)[1] # Get 'F' component
formula_sol <- calc_Pf(t_val)

cat("Numeric:", numeric_sol, "\nFormula:", formula_sol)
