test_categorical <- function(c_table = table(df$Type, df$F), title = "Type ~ F", alpha = 0.05) {
  print(addmargins(c_table))
  
  test_res <- chisq.test(c_table, simulate.p.value = TRUE)
  pvalue <- test_res$p.value
  
  cat("Значимость связи по критерию Хи-квадрат Пирсона для:", title, "\n")
  cat("\tp-value =", pvalue, "\n\t")
  if (pvalue > alpha) {
    cat("(связь не значима при alpha =", alpha, ")\n")
  } else {
    cat("(связь значима при alpha =", alpha, ")\n")
  }
  
  mosaicplot(c_table, main = title, color = TRUE)
  result <- pvalue > alpha
  return(result)
}

