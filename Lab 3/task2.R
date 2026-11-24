# ---------- 3. Оценка коэффициентов
x_bar <- mean(x)
y_bar <- mean(y)

Sxx <- sum((x - x_bar)^2)
Sxy <- sum((x - x_bar) * (y - y_bar))
b_hat <- Sxy / Sxx
a_hat <- y_bar - b_hat * x_bar

cat("МНК-оценки:\n")
cat("  a_hat =", a_hat, "\n")
cat("  b_hat =", b_hat, "\n")

# ---------- 4. Оценка дисперсии и СКО
# остатки
y_hat <- a_hat + b_hat * x
e     <- y - y_hat

# суммы квадратов и оценка дисперсии
TSS  <- sum((y - y_bar)^2)
var_y_hat <- TSS / N
sigma_y_hat <- sqrt(var_y_hat)

RSS  <- sum(e^2)
df   <- N - 2
sigma2_hat <- RSS / df
sigma_hat  <- sqrt(sigma2_hat)

# дисперсии оценок
var_a_hat <- sigma2_hat * (1/N + x_bar^2 / Sxx)
var_b_hat <- sigma2_hat / Sxx

se_a <- sqrt(var_a_hat)
se_b <- sqrt(var_b_hat)

cat("\nДисперсии и СКО:\n")
cat("  var(y_hat) =", var_y_hat, " sigma(y_hat) =", sigma_y_hat, ")\n")
cat("  sigma2_hat =", sigma2_hat, " sigma_hat =", sigma_hat, ")\n")
cat("  var(a_hat) =", var_a_hat, "  se(a_hat) =", se_a, "\n")
cat("  var(b_hat) =", var_b_hat, "  se(b_hat) =", se_b, "\n")

# ---------- 5. Доверительные интервалы
alpha <- 0.05
t_cr  <- qt(1 - alpha/2, df)

ci_a <- c(a_hat - t_cr * se_a, a_hat + t_cr * se_a)
ci_b <- c(b_hat - t_cr * se_b, b_hat + t_cr * se_b)

cat("\n95% Доверительный интервал для a:", ci_a, "\n")
cat("95% Доверительный интервал для b:", ci_b, "\n")

# ---------- 6. Проверка гипотез
t_a <- a_hat / se_a
p_a <- 2 * pt(-abs(t_a), df)

t_b <- b_hat / se_b
p_b <- 2 * pt(-abs(t_b), df)

cat("\nПроверка значимости коэффициентов:\n")
cat("  a: t =", t_a, " p-value =", p_a, "\n",
    ifelse(p_a > 0.05, "(не значим при alpha = 0.05)\n", "(значим при alpha = 0.05)\n"))
cat("  b: t =", t_b, " p-value =", p_b, "\n",
    ifelse(p_a > 0.05, "(не значим при alpha = 0.05)\n", "(значим при alpha = 0.05)\n"))

# ---------- 7. Коэффициент детерминации
R2 <- 1 - RSS / TSS
cat("\nR^2 =", R2, "\n")

# ---------- 8. Проверка адекватности модели (F-тест)
F_stat <- (TSS - RSS) / (RSS / df)
p_F    <- pf(F_stat, 1, df, lower.tail = FALSE)

cat("\nF-тест адекватности:\n")
cat("F =", F_stat, " p-value =", p_F, "\n",
    ifelse(p_F > 0.05, "(не значима при alpha = 0.05)\n", "(значима при alpha = 0.05)\n"))

# ---------- 9. Расчёты через lm()
fit <- lm(y ~ x)
s   <- summary(fit)
cat("\n===== Результаты lm() =====\n")
print(s)

cat("\n===== Сравнение =====\n")
compare <- data.frame(
  parameter = c("a", "b"),
  manual    = c(a_hat, b_hat),
  lm        = coef(fit),
  se_manual = c(se_a, se_b),
  se_lm     = coef(s)[, "Std. Error"],
  p_manual  = c(p_a, p_b),
  p_lm      = coef(s)[, "Pr(>|t|)"]
)
print(compare, digits = 6)
cat("R^2 manual =", R2, " | R^2 lm =", s$r.squared, "\n")
cat("F manual   =", F_stat, " | F lm   =", s$fstatistic[1], "\n")

# ---------- Визуализация
x_grid <- seq(min(x), max(x), length.out = 200)
y_grid <- a_hat + b_hat * x_grid

# Дов. интервал для средней линии регрессии
se_fit <- sqrt(sigma2_hat * (1/N + (x_grid - x_bar)^2 / Sxx))
ci_fit_low  <- y_grid - t_cr * se_fit
ci_fit_high <- y_grid + t_cr * se_fit

# Дов. интервал для прогноза
se_pred <- sqrt(sigma2_hat * (1 + 1/N + (x_grid - x_bar)^2 / Sxx))
ci_pred_low  <- y_grid - t_cr * se_pred
ci_pred_high <- y_grid + t_cr * se_pred

plot(x, y, pch = 19, col = "grey40",
     main = "Парная регрессия",
     xlab = "x", ylab = "y")
grid()
# Дов. интервал для прямой
polygon(c(x_grid, rev(x_grid)),
        c(ci_fit_low, rev(ci_fit_high)),
        col = rgb(0, 0, 1, 0.15), border = NA)
# Дов. интервал для прогноза
polygon(c(x_grid, rev(x_grid)),
        c(ci_pred_low, rev(ci_pred_high)),
        col = rgb(1, 0, 0, 0.10), border = NA)
lines(x_grid, y_grid, col = "blue", lwd = 2)              # линия регрессии
lines(x_grid, ci_fit_low,  col = "blue", lty = 2)         # ДИ прямой
lines(x_grid, ci_fit_high, col = "blue", lty = 2)
lines(x_grid, ci_pred_low,  col = "red",  lty = 3)        # ДИ прогноза
lines(x_grid, ci_pred_high, col = "red",  lty = 3)
points(x, y, pch = 19, col = "grey40")
legend("topleft",
       legend = c("Данные", "Линия регрессии",
                  "95% ДИ для прямой", "95% ДИ для прогноза"),
       col = c("grey40", "blue", "blue", "red"),
       pch = c(19, NA, NA, NA),
       lty = c(NA, 1, 2, 3), lwd = c(NA, 2, 1, 1), bty = "n")


