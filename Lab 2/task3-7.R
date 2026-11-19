library("coin")

test_cor_spearman <- function(formula, df, alpha = 0.05) {
  vars <- all.vars(formula)
  name_y <- vars[1]
  name_x <- vars[2]
  
  test <- spearman_test(formula, df)
  pvalue <- pvalue(test)
  
  cat("Значимость выборочного коэф. корреляции по критерию Спирмена:", name_y, "~", name_x, ": p-value =", pvalue, "\n\t",
      ifelse(pvalue > alpha, "(не значим", "(значим"), "при alpha =", alpha, ")\n")
  
  result <- pvalue > alpha
  return(result)
}

test_all_cor_spearman <- function(df, features = c("Rent", "Floor", "Total", "M2")) {
  cor_matrix <- cor(df[, features], method = "spearman")
  print(cor_matrix)
  
  pairs <- combn(features, 2)
  print(pairs)
  
  for (i in 1:ncol(pairs)) {
    var1 <- pairs[1, i]
    var2 <- pairs[2, i]
    
    form <- as.formula(paste(var1, "~", var2))
    test_cor_spearman(form, df)
  }
}

