get_rs <- function(samples) {
  X1 <- samples$X$X1
  X2 <- samples$X$X2
  X3 <- samples$X$X3
  
  rs1 <- cor(X1, X2)
  rs2 <- cor(X1, X3)
  
  cat("Выборочный коэффициент корреляции X1, X2:", rs1, "\n")
  cat("Выборочный коэффициент корреляции X1, X3:", rs2, "\n")
  
  samples$rs1 <<- rs1
  samples$rs2 <<- rs2
}

# t-тест Стьюдента
get_t_test <- function(samples, alpha = 0.05) {
  r1 <- samples$rs1
  r2 <- samples$rs2
  n = 200
  
  t_stat1 <- r1 * sqrt((n - 2) / (1 - r1^2))
  t_stat2 <- r2 * sqrt((n - 2) / (1 - r2^2))
  
  samples$t_stat1 <<- t_stat1
  samples$t_stat2 <<- t_stat2
  
  df <- n - 2
  p_val1 <- 2 * pt(-abs(t_stat1), df)
  p_val2 <- 2 * pt(-abs(t_stat2), df)
  
  cat("Значение статистики Стьюдента для X1, X2:", t_stat1, "p-value =", p_val1,
      ifelse(p_val1 > alpha, "Выборки не коррелируют", "Выборки коррелированы"), "(alpha =", alpha, ")\n")
  cat("Значение статистики Стьюдента для X1, X3:", t_stat2, "p-value =", p_val2,
      ifelse(p_val2 > alpha, "Выборки не коррелируют", "Выборки коррелированы"), "(alpha =", alpha, ")\n")
}

interval_calc <- function(r, n = 200, alpha = 0.05) {
  z <- atanh(r)
  se <- 1 / sqrt(n - 3)
  z_crit <- qnorm(1 - alpha/2)
  
  z_lower <- z - z_crit * se
  z_upper <- z + z_crit * se
  
  r_lower <- tanh(z_lower)
  r_upper <- tanh(z_upper)
  
  return(c(lower = r_lower, upper = r_upper))
}

interval_all_calc <- function(samples, alpha = 0.05) {
  r1 <- samples$rs1
  r2 <- samples$rs2
  n = 200
  
  samples$rs1_interval <- interval_calc(r1, n, alpha)
  samples$rs2_interval <- interval_calc(r2, n, alpha)
  
  cat("\n--- Доверительные интервалы (95%) ---\n")
  cat("Для X1 и X2 (r =", r1, "): [", round(samples$rs1_interval[1], 4), ";", round(samples$rs1_interval[2], 4), "]\n")
  cat("Для X1 и X3 (r =", r2, "): [", round(samples$rs2_interval[1], 4), ";", round(samples$rs2_interval[2], 4), "]\n")
}
