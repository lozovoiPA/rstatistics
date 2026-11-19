generate_corr_norm <- function(N = 200, mean = 2, sd = 3) {
  X1 <- rnorm(N, mean, sd)
  r1 <- runif(1, 0.05, 0.15)
  r2 <- runif(1, 0.6, 0.9)
  
  cat("Коэф. корреляции:", r1, r2)
  X2 <- r1 * X1 + sqrt(1 - r1*r1) * rnorm(N, 5, 4)
  X3 <- r2 * X1 + sqrt(1 - r2*r2) * rnorm(N, -4, 6)
  
  return(list(X = list(X1 = X1, X2 = X2, X3 = X3), r1 = r1, r2 = r2))
}