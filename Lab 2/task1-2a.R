# Выборки независимые
test_shapiro <- function(X, name="X", alpha = 0.05) {
  shapiro_X <- shapiro.test(X)
  pvalue <- shapiro_X$p.value
  cat("Тест Шапиро-Уилка", name, ": p-value =", round(pvalue, 4), 
      ifelse(pvalue > alpha, "(Распределение нормальное", "(Не нормальное"), "при alpha =", alpha, ")\n")
  result <- pvalue > alpha
  return(result)
}
test_all_shapiro <- function(X) {
  for (condition_name in names(X)) {
    condition <- X[[condition_name]]
    print(condition_name)
    for (i in seq_along(condition$X)) {
      x <- (condition$X)[[i]]
      result <- test_shapiro(x$samples, paste("X", i))
      X[[condition_name]]$X[[i]]$shapiro <<- result
    }
  }
}

# F-тест Фишера равенства дисперсий (выборки нормальные)
test_dequality <- function(X1, X2, alpha = 0.05) {
  f_test <- var.test(X1, X2)
  pvalue <- f_test$p.value
  cat("F-тест (равенство дисперсий): p-value =", round(pvalue, 4), 
      ifelse(pvalue > alpha, "(Дисперсии равны", "(Дисперсии не равны"), "при alpha =", alpha, ")\n")
  result <- pvalue > alpha
  return(result)
}
test_all_dequality <- function(X) {
  for (condition in X) {
    print(condition$name)
    if(!(condition$X$X1$shapiro && condition$X$X2$shapiro)) {
      cat("Одна из выборок или обе не нормальны\n")
      X[[condition$name]]$dequality <<- "NONE"
      next
    }
    result <- test_dequality(condition$X$X1$samples, condition$X$X2$samples)
    X[[condition$name]]$dequality <<- result
  }
}

# Равенство мат. ожиданий (выборки нормальные)
test_eequality <- function(X1, X2, dequality, alpha = 0.05) {
  if (dequality) {
    t_test <- t.test(X1, X2, var.equal = TRUE)
    pvalue <- t_test$p.value
    result <- pvalue > alpha
    cat("t-тест Стьюдента: p-value =", round(pvalue, 4),
        ifelse(pvalue > alpha, "(Мат. ожидания равны", "(Мат. ожидания не равны"), "при alpha =", alpha, ")\n")
    return(result)
  } else {
    t_test <- t.test(X1, X2, var.equal = FALSE)
    pvalue <- t_test$p.value
    result <- pvalue > alpha
    cat("t-тест Уэлча: p-value =", round(pvalue, 4),
        ifelse(pvalue > alpha, "(Мат. ожидания равны", "(Мат. ожидания не равны"), "при alpha =", alpha, ")\n")
    return(result)
  }
}
test_all_eequality <- function(X) {
  for (condition in X) {
    print(condition$name)
    if(!(condition$X$X1$shapiro && condition$X$X2$shapiro)) {
      cat("Одна из выборок или обе не нормальны\n")
      X[[condition$name]]$eequality <<- "NONE"
      next
    }
    result <- test_eequality(condition$X$X1$samples, condition$X$X2$samples, condition$dequality)
    X[[condition$name]]$eequality <<- result
  }
}