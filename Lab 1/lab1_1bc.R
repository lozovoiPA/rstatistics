plot_hist <- function(x, name = 'X', br = "Sturges") {
  switch(br, 
         "Sturges" = {br_hist <- nclass.Sturges(x)}, # Стерджес
         "Scott" = {br_hist <- nclass.scott(x)}, # Скотт
         "FD" = {br_hist <- nclass.FD(x)},
         {br_hist <- nclass.Sturges(x)}) # Фридман-Диаконис
  
  h <- hist(x, breaks = br_hist, plot = FALSE)
  
  df_hist <- data.frame(x = h$mids, count = h$counts,
                        density = h$density)
  
  p_hist <- ggplot(data.frame(x = x), aes(x)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = br_hist, fill = "steelblue",
                   color = "white", alpha = 0.7) +
    geom_density(color = "darkred", linewidth = 1, show.legend = ) +   # KDE
    labs(title = sprintf("%s: гистограмма + KDE (bins=%d по %s)", name, br_hist, br),
         x = "x", y = "Плотность") +
    theme_minimal()
}

plot_ecdf <- function(x, name = 'X') {
  p_ecdf <- ggplot(data.frame(x = x), aes(x)) +
    stat_ecdf(color = "darkgreen", linewidth = 1) +
    labs(title = sprintf("%s: ECDF", name), x = "x", y = "F(x)") +
    theme_minimal()
}
