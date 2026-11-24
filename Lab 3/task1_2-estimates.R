estimate_mnk <- function(reg_model) {
  x <- reg_model$x_lin
  y <- reg_model$y_lin
  
  x_bar <- mean(x)
  y_bar <- mean(y)
  Sxx   <- sum((x - x_bar)^2)
  Sxy   <- sum((x - x_bar) * (y - y_bar))
  
  reg_model$b_hat   <- Sxy / Sxx
  reg_model$a_lin_hat <- y_bar - reg_model$b_hat * x_bar
  reg_model$x_bar   <- x_bar
  reg_model$y_bar   <- y_bar
  reg_model$Sxx     <- Sxx
  
  is_lin <- if (reg_model$name %in% c("pow", "exp")) FALSE else TRUE
  # обратное преобразование свободного члена для експоненциальной и степенной моделей
  reg_model$a_hat <- if (!is_lin) {
    exp(reg_model$a_lin_hat)
  } else {
    reg_model$a_lin_hat
  }
  
  cat("\n=== МНК-оценки (модель ", reg_model$name, ") ===\n", sep="")
  cat("Оценка a =", reg_model$a_hat, "\n")
  cat("Оценка b =", reg_model$b_hat, "\n")
  
  invisible(reg_model)
}

estimate_variances <- function(reg_model) {
  x <- reg_model$x_lin
  y <- reg_model$y_lin
  N <- reg_model$N
  df <- N - 2
  
  y_hat <- reg_model$a_lin_hat + reg_model$b_hat * x
  e     <- y - y_hat
  
  TSS <- sum((y - reg_model$y_bar)^2)
  RSS <- sum(e^2)
  
  sigma2_hat <- RSS / df
  sigma_hat  <- sqrt(sigma2_hat)
  
  var_y  <- TSS / N
  sigma_y <- sqrt(var_y)
  
  var_a_lin_hat <- sigma2_hat * (1/N + reg_model$x_bar^2 / reg_model$Sxx)
  var_b_hat     <- sigma2_hat / reg_model$Sxx
  
  if (reg_model$name %in% c("pow", "exp"))
    var_a_hat <- exp(2*reg_model$a_lin_hat) * var_a_lin_hat
  else
    var_a_hat <- var_a_lin_hat
  
  reg_model$y_hat     <- y_hat
  reg_model$e         <- e
  reg_model$TSS       <- TSS
  reg_model$RSS       <- RSS
  reg_model$df        <- df
  reg_model$sigma2_hat <- sigma2_hat
  reg_model$sigma_hat  <- sigma_hat
  reg_model$var_y  <- var_y
  reg_model$sigma_y <- sigma_y
  reg_model$var_a_lin_hat <- var_a_lin_hat
  reg_model$var_b_hat     <- var_b_hat
  reg_model$se_a_lin      <- sqrt(var_a_lin_hat)
  reg_model$se_b          <- sqrt(var_b_hat)
  reg_model$var_a_hat     <- var_a_hat
  reg_model$se_a          <- sqrt(var_a_hat)
  
  cat("\n=== Дисперсии и СКО (", reg_model$name, ") ===\n")
  cat("  sigma2_hat =", sigma2_hat, "  sigma_hat =", sigma_hat, "\n")
  cat("  var(y) =", var_y, "  sigma(y) =", sigma_y, "\n")
  cat("  var(a_hat) =", var_a_hat, "  se =", reg_model$se_a, "\n")
  cat("  var(b_hat) =", var_b_hat,     "  se =", reg_model$se_b, "\n")
  
  invisible(reg_model)
}

calc_confint <- function(reg_model, alpha = 0.05) {
  t_cr <- qt(1 - alpha / 2, reg_model$df)
  
  reg_model$t_cr <- t_cr
  reg_model$ci_a_lin <- c(low = reg_model$a_lin_hat - t_cr * reg_model$se_a_lin,
                           high = reg_model$a_lin_hat + t_cr * reg_model$se_a_lin)
  reg_model$ci_b <- c(low = reg_model$b_hat - t_cr * reg_model$se_b,
                       high = reg_model$b_hat + t_cr * reg_model$se_b)
  
  # для a_hat (исходная шкала) — экспонента от границ, если модель мультипликативная
  reg_model$ci_a <- if (reg_model$name %in% c("pow", "exp")) {
    exp(reg_model$ci_a_lin)
  } else {
    reg_model$ci_a_lin
  }
  
  cat("\n=== 95% Доверительный интервал (", reg_model$name, ") ===\n")
  cat("  a: [", reg_model$ci_a[1], ";", reg_model$ci_a[2], "]\n")
  cat("  b: [", reg_model$ci_b[1], ";", reg_model$ci_b[2], "]\n")
  
  invisible(reg_model)
}