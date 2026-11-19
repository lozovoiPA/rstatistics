library(ggplot2)
library(gridExtra)

plot_scatter <- function(samples) {
  X1 <- samples$X$X1
  X2 <- samples$X$X2
  X3 <- samples$X$X3
  r1 <- samples$r1
  r2 <- samples$r2
  
  df <- data.frame(X1 = X1, X2 = X2, X3 = X3)
  
  p1 <- ggplot(df, aes(x = X1, y = X2)) +
    geom_point(color = "blue", alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
    labs(title = "Диаграмма рассеяния X1 и X2",
         subtitle = paste0("r = ", r1),
         x = "X1", y = "X2") +
    theme_minimal()
  
  p2 <- ggplot(df, aes(x = X1, y = X3)) +
    geom_point(color = "darkgreen", alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
    labs(title = "Диаграмма рассеяния X1 и X3",
         subtitle = paste0("r = ", r2),
         x = "X1", y = "X3") +
    theme_minimal()
  
  grid.arrange(p1, p2, ncol = 2)
}