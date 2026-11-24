source("../Lab 2/task3-1_5.R")

# Загрузка данных
df <- import_df("../Lab 2/flats.csv")

df$Rent <- as.numeric(df$Rent)
str(df)

df_clean <- clean_df(df)
cat("Было строк:", nrow(df), "Стало строк:", nrow(df_clean), "\n")

df <- df_clean

# Составляем парную модель регресии
N <- length(df$Rent)
x <- df$M2
y <- df$Rent
reg_model <- list("name" = "lin", "N" = N, "x" = x, "y" = y)

visualize_pair_data(reg_model)

# Оценка параметров по lm
result <- lm(y ~ x)
a <- result$coefficients[["(Intercept)"]]
b <- result$coefficients[["x"]]
cat("Оценки коэффициентов:\n", "a = ", a, "\nb = ", b, sep="")
s <- summary(result)
print(s)

visualize_pair_reg_model(reg_model)

# Предсказание
my_flat <- data.frame(x = 35)
pred_pair <- predict(result, newdata = my_flat, interval = "prediction", level = 0.95)
print(pred_pair)

# Проверка остатков
check_hetsci(result)

# Гомоскедастичные остатки, но попробуем удалить выбросы (одна из точек например с платой 2000)
df[c(32, 34, 44), ]
df2 <- df[-c(32, 34, 44), ]
nrow(df2)

N <- length(df2$Rent)
x <- df2$M2
y <- df2$Rent
result <- lm(y ~ x)
check_hetsci(result)


# модель множественной регрессии
full_model <- lm(Rent ~ Type + Floor + Total + M2 + F, data = df)
summary(full_model)
# В полной модели оказываются не значимы Total и Type
multi_model <- lm(Rent ~ Floor + M2 + F, data = df)
summary(multi_model)
# В данном случае не значим Floor
multi_model <- lm(Rent ~ M2 + F, data = df)
summary(multi_model)

visualize_multi_model(df)

num_vars <- df[, c("Rent", "Floor", "Total", "M2")]
cor_mat <- cor(num_vars, use = "complete.obs", method = "pearson")
print(round(cor_mat, 3))

# Предсказание
my_flat <- data.frame(M2 = 35, F = "yes")
pred_pair <- predict(multi_model, newdata = my_flat, interval = "prediction", level = 0.95)
print(pred_pair)

check_hetsci(multi_model, TRUE, ~ M2 + F, df = df)
# То же самое - гетероскедастичности нет, но есть те же выбросы
multi_model <- lm(Rent ~ M2 + F, data = df2)
check_hetsci(multi_model, TRUE, ~ M2 + F, df = df2)

check_hetsci <- function(result, multi = FALSE, formula = NULL, df = NULL) {
  fit <- fitted(result)
  res <- residuals(result)
  par(mfrow = c(2, 2))
  plot(result)
  par(mfrow = c(1, 1))
  
  cat("Среднее остатков:", mean(res), "\n")
  cat("Ст. отклонение:", sd(res), "\n")
  
  # Тест на нормальность
  shapiro.test(res)
  # Тесты на гетероскедастичность
  library(lmtest)
  if (multi)
    bptest(result, formula, data=df)
  else
    bptest(result)
}

visualize_pair_data <- function(reg_model, title = "(Rent и M2)") {
  x_min <- min(reg_model$x) - 1
  x_max <- max(reg_model$x) + 1
  
  p_orig <- ggplot(data.frame(x = reg_model$x, y = reg_model$y), aes(x, y)) +
    geom_point(color = "steelblue", size = 1.5) +
    labs(title = paste("Исходные данные", title), x = "x", y = "y") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10)) +
    coord_cartesian(xlim = c(x_min, x_max))

  grid.arrange(p_orig, ncol=1, nrow=1)
}

visualize_pair_reg_model <- function(reg_model, title = "(Rent и M2)") {
  x <- reg_model$x
  x_grid <- seq(min(x), max(x), length.out = reg_model$N)
  y_grid <- (x_grid * b + a)
  
  p_reg <- ggplot() +
    geom_point(data = data.frame(x = x, y = reg_model$y), aes(x, y),
               color = "steelblue", size = 1.5) +
    geom_line(data = data.frame(x = x_grid, y = y_grid), aes(x, y),
              color = "blue", linewidth = 1) +
    labs(title = paste("Модель регрессии", title), x = "x", y = "y") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10))
  
  grid.arrange(p_reg, ncol=1, nrow=1)
}

visualize_multi_model <- function(df) {
  library(plotly)
  
  fig3d <- plot_ly(df, x = ~M2, y = ~Floor, z = ~Rent,
                   type = "scatter3d", mode = "markers",
                   marker = list(size = 3, color = ~Rent, colorscale = "Viridis")) %>%
    layout(scene = list(xaxis = list(title = "M2"),
                        yaxis = list(title = "Floor"),
                        zaxis = list(title = "Rent")))
  fig3d
}



