test_significance <- function(reg_model, alpha = 0.05) {
  t_a <- reg_model$a_lin_hat / reg_model$se_a_lin
  p_a <- 2 * pt(-abs(t_a), reg_model$df)
  t_b <- reg_model$b_hat / reg_model$se_b
  p_b <- 2 * pt(-abs(t_b), reg_model$df)
  
  reg_model$t_a <- t_a; reg_model$p_a <- p_a
  reg_model$t_b <- t_b; reg_model$p_b <- p_b
  
  cat("\n=== Проверка значимости коэффициентов модели (", reg_model$name, ") ===\n")
  cat("  a: t =", t_a, " p =", p_a, " ->",
      ifelse(p_a < alpha, "значим", "не значим"), "\n")
  cat("  b: t =", t_b, " p =", p_b, " ->",
      ifelse(p_b < alpha, "значим", "не значим"), "\n")
  
  invisible(reg_model)
}

calc_r2 <- function(reg_model) {
  R2     <- 1 - reg_model$RSS / reg_model$TSS
  reg_model$R2     <- R2
  
  cat("R^2 =", R2, "\n")
  
  invisible(reg_model)
}

test_adequacy <- function(reg_model, alpha = 0.05) {
  F_stat <- (reg_model$TSS - reg_model$RSS) / (reg_model$RSS / reg_model$df)
  p_F    <- pf(F_stat, 1, reg_model$df, lower.tail = FALSE)
  
  reg_model$F_stat <- F_stat
  reg_model$p_F    <- p_F
  
  cat("F =", F_stat, " p =", p_F, "->",
      ifelse(p_F < alpha, "модель значима", "модель не значима"), "\n")
  
  invisible(reg_model)
}

compare_with_lm <- function(reg_model) {
  fit <- lm(reg_model$y_lin ~ reg_model$x_lin)
  s   <- summary(fit)
  
  cat("\n=== Сравнение с lm() (для линеанизированной", reg_model$name, ") ===\n")
  compare <- data.frame(
    parameter = c("a_lin", "b"),
    manual    = c(reg_model$a_lin_hat, reg_model$b_hat),
    lm        = coef(fit),
    se_manual = c(reg_model$se_a_lin, reg_model$se_b),
    se_lm     = coef(s)[, "Std. Error"]
  )
  print(compare, digits = 6)
  cat("R^2 manual =", reg_model$R2, " | R^2 lm =", s$r.squared, "\n")
  cat("F  manual =", reg_model$F_stat, " | F  lm =", s$fstatistic[1], "\n")
  
  reg_model$lm_fit <<- fit
  invisible(reg_model)
}

