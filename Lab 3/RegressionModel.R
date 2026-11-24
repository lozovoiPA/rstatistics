RegressionModel <- setRefClass(
  "RegressionModel",
  fields = list(
    x        = "numeric",   # исходный предиктор
    y        = "numeric",   # исходные наблюдения
    a        = "numeric",   # коэф. a
    b        = "numeric",   # коэф. b
    eps_sigma  = "numeric", # параметр распределения ошибок
    eps      = "numeric",   # ошибки
    
    x_t      = "numeric"    # 
    
    
    x_lin    = "numeric",   # линеаризованный предиктор
    y_lin    = "numeric",   # линеаризованные наблюдения
    weights  = "numeric",   # веса (для гетероскедастичности; по умолчанию 1)
    a_hat    = "numeric",   # МНК-оценка a
    b_hat    = "numeric",   # МНК-оценка b
    sigma2   = "numeric",   # оценка дисперсии ошибок
    R2       = "numeric",   # коэффициент детерминации
    tss      = "numeric",   # TSS (взвешенная)
    rss      = "numeric",   # RSS (взвешенная)
    df       = "numeric"    # число степеней свободы
  ),
  methods = list(
    
    # ---------- Хуки, переопределяемые в подклассах ----------
    transform = function(x, y) {
      "Линеаризация. По умолчанию — тождественное преобразование."
      list(x = x, y = y)
    },
    inverse_predict = function(eta) {
      "Обратное преобразование предсказания в исходную шкалу."
      eta
    },
    default_weights = function(x) {
      "Веса по умолчанию — все единицы (гомоскедастичность)."
      rep(1, length(x))
    },
    
    # ---------- Инициализация ----------
    initialize = function(x, y, weights = NULL) {
      x <<- as.numeric(x)
      y <<- as.numeric(y)
      if (is.null(weights)) weights <<- default_weights(x)
      else                 weights <<- as.numeric(weights)
      
      tr  <- transform(x, y)
      x_t <<- tr$x
      y_t <<- tr$y
      df  <<- length(x_t) - 2
      
      .self$fit()
      invisible(.self)
    },
    
    visualize = function() {
      
    },
    
    # ---------- МНК-оценки ----------
    fit = function() {
      w  <- weights
      W  <- sum(w)
      xb <- sum(w * x_t) / W
      yb <- sum(w * y_t) / W
      
      Sxx <- sum(w * (x_t - xb)^2)
      Sxy <- sum(w * (x_t - xb) * (y_t - yb))
      
      b_hat <<- Sxy / Sxx
      a_hat <<- yb - b_hat * xb
      
      e    <- y_t - (a_hat + b_hat * x_t)
      tss  <<- sum(w * (y_t - yb)^2)
      rss  <<- sum(w * e^2)
      sigma2 <<- rss / df
      R2   <<- 1 - rss / tss
      
      # сохраняем взвешенные средние и Sxx для ДИ
      .self$store_fit_stats(xb, yb, Sxx, W)
      invisible(.self)
    },
    
    store_fit_stats = function(xb, yb, Sxx, W) {
      .self$xb <<- xb
      .self$yb <<- yb
      .self$Sxx <<- Sxx
      .self$W <<- W
    },
    
    # ---------- Дисперсии и стандартные ошибки оценок ----------
    se = function() {
      se_a <- sqrt(sigma2 * (1 / W + xb^2 / Sxx))
      se_b <- sqrt(sigma2 / Sxx)
      c(a = se_a, b = se_b)
    },
    
    # ---------- Доверительные интервалы для параметров ----------
    confint = function(alpha = 0.05) {
      t_cr <- qt(1 - alpha / 2, df)
      s    <- se()
      rbind(
        a = c(low = a_hat - t_cr * s["a"], high = a_hat + t_cr * s["a"]),
        b = c(low = b_hat - t_cr * s["b"], high = b_hat + t_cr * s["b"])
      )
    },
    
    # ---------- t-тесты значимости ----------
    t_tests = function() {
      s     <- se()
      t_val <- c(a = a_hat / s["a"], b = b_hat / s["b"])
      p_val <- 2 * pt(-abs(t_val), df)
      data.frame(estimate = c(a_hat, b_hat), se = s,
                 t = t_val, p_value = p_val, row.names = c("a", "b"))
    },
    
    # ---------- F-тест адекватности ----------
    f_test = function() {
      F_stat <- (tss - rss) / (rss / df)
      p_val  <- pf(F_stat, 1, df, lower.tail = FALSE)
      list(F = F_stat, df1 = 1, df2 = df, p_value = p_val)
    },
    
    # ---------- Предсказание (в исходной шкале) ----------
    predict = function(new_x, interval = c("none", "confidence", "prediction"),
                       alpha = 0.05) {
      interval <- match.arg(interval)
      tr <- transform(new_x, rep(NA_real_, length(new_x)))
      x_new_t <- tr$x
      eta <- a_hat + b_hat * x_new_t
      
      if (interval == "none") return(inverse_predict(eta))
      
      t_cr <- qt(1 - alpha / 2, df)
      se_fit <- sqrt(sigma2 * (1 / W + (x_new_t - xb)^2 / Sxx))
      if (interval == "prediction")
        se_fit <- sqrt(se_fit^2 + sigma2)
      
      list(
        fit  = inverse_predict(eta),
        lwr  = inverse_predict(eta - t_cr * se_fit),
        upr  = inverse_predict(eta + t_cr * se_fit)
      )
    },
    
    # ---------- График: данные + линия + ДИ ----------
    plot_fit = function(alpha = 0.05, n_grid = 200, main = NULL, ...) {
      x_grid <- seq(min(x), max(x), length.out = n_grid)
      pred_fit <- predict(x_grid, "confidence", alpha)
      pred_pr  <- predict(x_grid, "prediction", alpha)
      
      plot(x, y, pch = 19, col = "grey40",
           main = if (is.null(main)) class(.self)[1] else main,
           xlab = "x", ylab = "y", ...)
      grid()
      polygon(c(x_grid, rev(x_grid)), c(pred_fit$lwr, rev(pred_fit$upr)),
              col = rgb(0, 0, 1, 0.15), border = NA)
      polygon(c(x_grid, rev(x_grid)), c(pred_pr$lwr, rev(pred_pr$upr)),
              col = rgb(1, 0, 0, 0.10), border = NA)
      lines(x_grid, pred_fit$fit, col = "blue", lwd = 2)
      lines(x_grid, pred_fit$lwr, col = "blue", lty = 2)
      lines(x_grid, pred_fit$upr, col = "blue", lty = 2)
      lines(x_grid, pred_pr$lwr,  col = "red",  lty = 3)
      lines(x_grid, pred_pr$upr,  col = "red",  lty = 3)
      legend("topleft",
             legend = c("Данные", "Линия регрессии",
                        "95% ДИ для прямой", "95% ДИ для прогноза"),
             col = c("grey40", "blue", "blue", "red"),
             pch = c(19, NA, NA, NA),
             lty = c(NA, 1, 2, 3), lwd = c(NA, 2, 1, 1), bty = "n")
    },
    
    # ---------- Краткий отчёт ----------
    summary = function() {
      cat("Модель:", class(.self)[1], "\n")
      cat("  a_hat =", a_hat, "  b_hat =", b_hat, "\n")
      cat("  sigma2 =", sigma2, "  R^2 =", R2, "\n")
      cat("  F =", f_test()$F, " p =", f_test()$p_value, "\n")
      invisible(.self)
    }
  )
)

# ---------- y = a + b*x + eps ----------
LinearModel <- setRefClass(
  "LinearModel",
  contains = "RegressionModel",
  methods = list(
    transform = function(x, y) list(x = x, y = y),
    inverse_predict = function(eta) eta
  )
)

# ---------- y = a * x^b * eps  ->  ln y = ln a + b ln x ----------
PowerModel <- setRefClass(
  "PowerModel",
  contains = "RegressionModel",
  methods = list(
    transform = function(x, y) list(x = log(x), y = log(y)),
    inverse_predict = function(eta) exp(eta)
  )
)

# ---------- y = a * exp(b*x) * eps  ->  ln y = ln a + b x ----------
ExponentialModel <- setRefClass(
  "ExponentialModel",
  contains = "RegressionModel",
  methods = list(
    transform = function(x, y) list(x = x, y = log(y)),
    inverse_predict = function(eta) exp(eta),
    # гетероскедастичность: sigma_i^2 ~ x^2  ->  w_i = 1/x^2
    default_weights = function(x) 1 / x^2
  )
)

# ---------- y = a + b*ln(x) + eps ----------
LogModel <- setRefClass(
  "LogModel",
  contains = "RegressionModel",
  methods = list(
    transform = function(x, y) list(x = log(x), y = y),
    inverse_predict = function(eta) eta
  )
)

# ---------- y = a + b*sqrt(x) + eps ----------
SqrtModel <- setRefClass(
  "SqrtModel",
  contains = "RegressionModel",
  methods = list(
    transform = function(x, y) list(x = sqrt(x), y = y),
    inverse_predict = function(eta) eta,
    # гетероскедастичность: sigma_i^2 ~ x  ->  w_i = 1/x
    default_weights = function(x) 1 / x
  )
)