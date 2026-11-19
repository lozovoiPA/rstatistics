library("coin") # Для вычисления точного p-value в тесте Манна-Уитни 

plot_boxplots <- function(df) {
  # График: Цена в зависимости от Типа
  p1 <- ggplot(df, aes(x = Type, y = Rent, fill = Type)) +
    geom_boxplot() +
    labs(title = "Распределение цены аренды\nпо типу жилья", x = "Тип", y = "Цена") +
    theme_minimal()
  
  # График: Цена в зависимости от наличия мебели
  p2 <- ggplot(df, aes(x = F, y = Rent, fill = F)) +
    geom_boxplot() +
    labs(title = "Распределение цены аренды\nпо наличию мебели", x = "Мебель", y = "Цена") +
    theme_minimal()
  
  # График: M2 в зависимости от типа
  p3 <- ggplot(df, aes(x = Type, y = M2, fill = Type)) +
    geom_boxplot() +
    labs(title = "Распределение площади\nпо типу жилья", x = "Тип", y = "Площадь, кв.м.") +
    theme_minimal()
  
  grid.arrange(p1, p2, p3, ncol = 3)
}

test_mann_whitney <- function(formula, df, name_x = "x", name_y = "y", alpha = 0.05) {
  test <- wilcox_test(formula, data = df, conf.level = 1 - alpha, distribution = "exact")
  pvalue <- pvalue(test)
  cat("Тест Манна-Уитни", name_y, "~", name_x, ": p-value =", pvalue, "\n\t(Различия между группами статистически",
      ifelse(pvalue > alpha, "не значимы", "значимы"), "при alpha =", alpha, ")\n")
  result <- pvalue > alpha
  return(result)
}

test_t_student <- function(formula, df, name_x = "x", name_y = "y", alpha = 0.05) {
  test <- t.test(formula, data = df, conf.level = 1 - alpha)
  pvalue <- test$p.value
  cat("t-тест Стьюдента", name_y, "~", name_x, ": p-value =", pvalue, "\n\t(Различия между группами статистически",
      ifelse(pvalue > alpha, "не значимы", "значимы"), "при alpha =", alpha, ")\n")
  result <- pvalue > alpha
  return(result)
}
