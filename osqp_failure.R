library(CVXR)

w <- Variable(36, cols = 1) # number of weight coefficients should also be adjusted for a different dataset
b <- Variable(1)

df <- read.csv("df37.csv")
df$X37 <- as.factor(df$X37)
X <- as.matrix(dplyr::select(df, -X37))

X1 <- X[df$X37 == 3,]
X2 <- X[df$X37 == 7,]
    
objective <- Minimize(sum_squares(w))
constraints <- list(X1 %*% w + b <= -1, X2 %*% w + b >= 1)
problem <- Problem(objective, constraints = constraints)
for (s in c("ECOS", "OSQP", "CLARABEL")) {
  result <- solve(problem, solver = s)
  print(sprintf("Result (desired %s) (reported %s) was %s",
                    s,
                    result$solver,
                    result$status))
}
