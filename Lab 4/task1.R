n <- 45
x <- runif(n, 30, 45)
eps <- rnorm(n, 0, 5)

t0 <- -20.1
t1 <- 0.5
expt <- exp(t0 + t1 * x + eps)
pi <- expt / (1 + expt)

y <- ifelse(pi < 0.5, 0, 1)

visualize_pair_data(x, y)

# Задание 2
model <- glm(y ~ x, family = binomial(link = "logit"))
summary(model)

# Предсказываем вероятности для всей выборки
probs <- predict(model, type = "response")
# Превращаем в классы
preds <- ifelse(probs > 0.5, 1, 0)

# Строим таблицу
conf_matrix <- table(Actual = y, Predicted = preds)
print(conf_matrix)

# Считаем метрики
tn <- conf_matrix[1, 1]
tp <- conf_matrix[2, 2]
fn <- conf_matrix[1, 2]
fp <- conf_matrix[2, 1]

accuracy <- (tp+tn) / sum(conf_matrix)
prec <- tp / (tp + fp)
recall <- tp / (tp + fn)

f1 <- 2 * prec * recall / (prec + recall)
cat("Accuracy :", round(accuracy, 4), "\n")
cat("Precision:", round(prec, 4), "\n")
cat("Recall   :", round(recall, 4), "\n")
cat("F1-score :", round(f1, 4), "\n")

library(pROC)
roc_obj <- roc(y, probs)
plot(roc_obj, col = "blue", main = "ROC Curve", print.auc = TRUE)

visualize_pair_data(x, y, preds, model)
# Одного признака в данной модели не хватает

# Предсказание
new_x <- data.frame(x = 34.5)
pred_prob <- predict(model, newdata = new_x, type = "response")
print(paste("Вероятность y=1 при x=34.5:", round(pred_prob, 4)))
pred_class <- ifelse(pred_prob >= 0.5, 1, 0)
print(paste("Предсказанный класс:", pred_class))

library(ggplot2)
library(gridExtra)

visualize_pair_data <- function(x, y, preds = NULL, model = NULL) {
  x_min <- min(x) - 1
  x_max <- max(x) + 1
  
  p_orig <- ggplot(data.frame(x = x, y = y), aes(x, y)) +
    geom_point(color = "steelblue", size = 1.5) +
    labs(title = "Исходные данные", x = "x", y = "y") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10)) +
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(-1, 2))
  
  if (!is.null(preds) && !is.null(model)) {
    df <- data.frame(x = x, y = y, preds = preds)
    df$correct <- ifelse(df$y == df$preds, "Верно", "Неверно")
    
    line_data <- data.frame(x = seq(x_min, x_max, length.out = 100))
    line_data$prob <- predict(model, newdata = line_data, type = "response")
    
    p_colored <- ggplot(df, aes(x = x, y = y, color = correct)) +
      geom_line(data = line_data, aes(x = x, y = prob), 
                color = "darkblue", linewidth = 1) +
      geom_point(size = 1.5) +
      scale_color_manual(values = c("Верно" = "#2ca02c", "Неверно" = "#d62728")) +
      labs(title = "Результат классификации", 
           x = "x", 
           y = "y",
           color = "Предсказание") +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        legend.position = "bottom"
      ) +
      coord_cartesian(xlim = c(x_min, x_max), ylim = c(-0.5, 1.5))
    grid.arrange(p_orig, p_colored, ncol = 2, nrow = 1)
  } else {
    grid.arrange(p_orig, ncol=1, nrow=1)
  }
}