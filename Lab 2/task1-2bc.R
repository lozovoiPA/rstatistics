library("ggplot2")
library("gridExtra")

plot_samples <- function(X1, X2, N = 100, title) {
  df <- data.frame(
    value = c(X1, X2),
    group = factor(rep(c("X1", "X2"), each = N))
  )
  
  # b) Гистограммы и оценки плотностей
  p1 <- ggplot(df, aes(x = value, fill = group, color = group)) +
    geom_histogram(aes(y = ..density..), alpha = 0.3, position = "identity", bins = 15) +
    geom_density(alpha = 1, size = 1, fill = NA) +
    geom_vline(xintercept = mean(X1), linetype = "dashed", color = "red", size = 1) +
    geom_vline(xintercept = mean(X2), linetype = "dashed", color = "blue", size = 1) +
    labs(title = paste("Гистограммы и плотность:", title),
         subtitle = "Пунктирные линии - выборочные средние",
         x = "Значения", y = "Плотность") +
    theme_minimal()
  
  # c) Коробки с усами
  p2 <- ggplot(df, aes(x = group, y = value, fill = group)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Коробки с усами:", title),
         x = "Группа", y = "Значения") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Выводим графики на экран
  # grid.arrange(p1, p2, ncol = 2)
  
  return(list(p1 = p1, p2 = p2))
}

draw_all_plots <- function() {
  all_plots <- list()
  for(condition in X) {
    plots <- plot_samples(condition$X$X1$samples, condition$X$X2$samples, title = paste("Вариант", condition$name))
    
    all_plots[[paste0(condition$name, "_p1")]] <- plots$p1
    all_plots[[paste0(condition$name, "_p2")]] <- plots$p2
  }
  
  final_grid <- arrangeGrob(
    grobs = all_plots, 
    ncol = 2, 
    nrow = 4
  )
  
  ggsave(
    filename = "task1.png", 
    plot = final_grid, 
    width = 14, 
    height = 16, 
    dpi = 300
  )
  grid.arrange(final_grid)
}

