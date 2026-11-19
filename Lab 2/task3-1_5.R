library("tidyverse")
library("ggplot2")
library("gridExtra")

import_df <- function(file = "flats.csv") {
  df <- read.csv2(file, stringsAsFactors = TRUE)
  
  print(head(df))
  str(df)
  
  return(df)
}

rm_outliers_by_IQR <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(y)
}

clean_df <- function(df) {
  df_clean <- df %>%
    mutate(
      Rent = rm_outliers_by_IQR(Rent),
      M2 = rm_outliers_by_IQR(M2)
      ) %>%
    drop_na()
  return(df_clean)
}

plot_hist <- function(df) {
  p1 <- ggplot(df, aes(x = Rent)) +
    geom_histogram(bins = 10, fill = "skyblue", color = "black") +
    labs(title = "Гистограмма распределения арендной платы (Rent)", x = "Цена", y = "Частота") +
    theme_minimal()
  
  p2 <- ggplot(df, aes(x = M2)) +
    geom_histogram(bins = 10, fill = "lightgreen", color = "black") +
    labs(title = "Гистограмма распределения площади (M2)", x = "Площадь кв.м", y = "Частота") +
    theme_minimal()
  
  grid.arrange(p1, p2, ncol = 2)
}

source("task1-2a.R") # Для функции test_shapiro