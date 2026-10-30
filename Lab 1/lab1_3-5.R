library(MASS)

# Функция для оценки параметров методом моментов (MM)
estimate_params_mm <- function(x, dist_name) {
  n <- length(x)
  m1 <- mean(x)
  m2 <- mean(x^2)
  v  <- var(x) * (n - 1) / n # выборочная дисперсия со знаменателем n
  
  if (dist_name == "normal") {
    return(list(mean = m1, sd = sqrt(v)))
  } 
  else if (dist_name == "gamma") {
    # E(X) = shape/rate, Var(X) = shape/rate^2
    shape_mm <- m1^2 / v
    rate_mm  <- m1 / v
    return(list(shape = shape_mm, rate = rate_mm))
  } 
  else if (dist_name == "exp") {
    # E(X) = 1/rate
    return(list(rate = 1 / m1))
  }
  else if (dist_name == "chisq") {
    return(list(df = m1))
  }
  else if (dist_name == "lognormal") {
    # m1 = exp(mu + sigma^2/2), m2 = exp(2mu + 2sigma^2)
    mu_mm <- 2 * log(m1) - 0.5 * log(m2)
    sigma2_mm <- log(m2) - 2 * log(m1)
    return(list(meanlog = mu_mm, sdlog = sqrt(sigma2_mm)))
  } 
  else if (dist_name == "weibull") {
    # Решается численно: Var = scale^2 * (Gamma(1+2/k) - Gamma(1+1/k)^2)
    # Используем uniroot для поиска shape
    cv2 <- v / m1^2
    f_shape <- function(k) {
      gamma(1 + 2/k) / (gamma(1 + 1/k)^2) - 1 - cv2
    }
    shape_mm <- tryCatch(uniroot(f_shape, c(0.1, 50))$root, error = function(e) NA)
    if (is.na(shape_mm)) return(list(shape = NA, scale = NA))
    scale_mm <- m1 / gamma(1 + 1/shape_mm)
    return(list(shape = shape_mm, scale = scale_mm))
  } 
  else if (dist_name == "uniform") {
    # a + b = 2*mean, (b-a)^2/12 = var => b-a = sqrt(12*var)
    a_mm <- m1 - sqrt(3 * v)
    b_mm <- m1 + sqrt(3 * v)
    return(list(min = a_mm, max = b_mm))
  } 
  else if (dist_name == "pois") {
    return(list(lambda = m1))
  } 
  else if (dist_name == "binom") {
    # MM не позволяет надежно оценить n и p одновременно без доп. информации.
    # Обычно n=max(x), p = mean/n (как и в MLE)
    n_est <- max(x)
    return(list(size = n_est, prob = m1 / n_est))
  } 
  else if (dist_name == "geom") {
    # E(X) = 1/p (если считаем число испытаний)
    return(list(prob = 1 / m1))
  }
  print("Неизвестное распределение для оценки MM: %s", dist_name)
  return(NULL)
}

# Функция для оценки параметров (MLE) в зависимости от типа распределения
estimate_params <- function(x, dist_name) {
  # Для дискретных обрабатываем отдельно
  if (dist_name == "pois") {
    return(list(lambda = mean(x)))
  } else if (dist_name == "binom") {
    # n оцениваем как max(x), p = mean/n
    n_est <- max(x)
    p_est <- mean(x) / n_est
    return(list(size = n_est, prob = p_est))
  } else if (dist_name == "geom") {
    # В R геометрическое - это число неудач до первого успеха
    # Если данные от 1, то это число испытаний, p = 1/mean
    return(list(prob = 1 / mean(x)))
  }
  
  # Для непрерывных используем MASS::fitdistr
  fit <- tryCatch({
    switch(dist_name,
           normal = fitdistr(x, "normal"),
           gamma = fitdistr(x, "gamma"),
           weibull = fitdistr(x, "weibull"),
           lognormal = fitdistr(x, "lognormal"),
           exp = fitdistr(x, "exponential"),
           chisq = fitdistr(x, "chi-squared", list(df = mean(x))),
           uniform = list(estimate = list(min=min(x), max=max(x)))
    )
  }, error = function(e) {
    message("Ошибка оценки MLE для ", dist_name, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(fit)) return(NULL)
  
  # Извлекаем оцененные параметры
  est <- as.list(fit$estimate)
  
  # Приводим имена параметров к тем, что используются в plot_overlay_cdf
  if (dist_name == "normal") return(list(mean = est$mean, sd = est$sd))
  if (dist_name == "gamma") return(list(shape = est$shape, rate = est$rate))
  if (dist_name == "weibull") return(list(shape = est$shape, scale = est$scale))
  if (dist_name == "lognormal") return(list(meanlog = est$meanlog, sdlog = est$sdlog))
  if (dist_name == "exp") return(list(rate = est$rate))
  if (dist_name == "chisq") return(list(df = est$df))
  if (dist_name == "uniform") return(list(min = est$min, max = est$max))
}

# Функция для проведения хи-квадрат теста
# Возвращает: статистику, df, p-value, текст вывода, ожидаемые и наблюдаемые частоты (для графика)
chi_square_test <- function(x, dist_name, params, is_discrete = FALSE, alpha = 0.05) {
  n <- length(x)
  
  if (is_discrete) {
    # Для дискретных: группируем по уникальным значениям
    obs_table <- table(x)
    vals <- as.numeric(names(obs_table))
    
    # Считаем ожидаемые вероятности
    probs <- switch(dist_name,
                    pois = dpois(vals, lambda = params$lambda),
                    binom = dbinom(vals, size = params$size, prob = params$prob),
                    geom = dgeom(vals - 1, prob = params$prob) # Внимание: сдвиг для dgeom, если данные от 1!
    )
    
    # Частоты
    exp_freqs <- probs * n
    obs_freqs <- as.numeric(obs_table)
    
    # Объединяем интервалы, где ожидаемая частота < 5
    while(any(exp_freqs < 5) && length(exp_freqs) > 1) {
      idx <- which(exp_freqs < 5)[1]
      if (idx == length(exp_freqs)) {
        # Если последний, объединяем с предыдущим
        exp_freqs[idx-1] <- exp_freqs[idx-1] + exp_freqs[idx]
        obs_freqs[idx-1] <- obs_freqs[idx-1] + obs_freqs[idx]
        exp_freqs <- exp_freqs[-(idx)]
        obs_freqs <- obs_freqs[-(idx)]
      } else {
        # Объединяем со следующим
        exp_freqs[idx+1] <- exp_freqs[idx+1] + exp_freqs[idx]
        obs_freqs[idx+1] <- obs_freqs[idx+1] + obs_freqs[idx]
        exp_freqs <- exp_freqs[-(idx)]
        obs_freqs <- obs_freqs[-(idx)]
      }
    }
    
    m <- length(exp_freqs) # число интервалов после объединения
    chi_sq <- sum((obs_freqs - exp_freqs)^2 / exp_freqs)
    s <- length(params) # число оцененных параметров
    df <- m - 1 - s
    
  } else {
    # Для непрерывных: используем hist для получения границ
    # Строим интервалы
    h <- hist(x, breaks = "Sturges", plot = FALSE)
    breaks <- h$breaks
    obs_freqs <- h$counts
    m <- length(obs_freqs)
    
    # Функция CDF в зависимости от распределения
    cdf_func <- function(q) {
      switch(dist_name,
             normal = pnorm(q, mean = params$mean, sd = params$sd),
             gamma = pgamma(q, shape = params$shape, rate = params$rate),
             weibull = pweibull(q, shape = params$shape, scale = params$scale),
             lognormal = plnorm(q, meanlog = params$meanlog, sdlog = params$sdlog),
             exp = pexp(q, rate = params$rate),
             chisq = pchisq(q, df = params$df),
             uniform = punif(q, min = params$min, max = params$max)
      )
    }
    
    # Вероятности попадания в интервалы
    p_theory <- diff(cdf_func(breaks))
    
    # Ожидаемые частоты
    exp_freqs <- p_theory * n
    
    # Объединяем с конца, если ожидаемая частота < 5
    while(any(exp_freqs < 5) && length(exp_freqs) > 1) {
      idx <- which(exp_freqs < 5)[1]
      if (idx == length(exp_freqs)) {
        exp_freqs[idx-1] <- exp_freqs[idx-1] + exp_freqs[idx]
        obs_freqs[idx-1] <- obs_freqs[idx-1] + obs_freqs[idx]
        exp_freqs <- exp_freqs[-(idx)]
        obs_freqs <- obs_freqs[-(idx)]
      } else {
        exp_freqs[idx+1] <- exp_freqs[idx+1] + exp_freqs[idx]
        obs_freqs[idx+1] <- obs_freqs[idx+1] + obs_freqs[idx]
        exp_freqs <- exp_freqs[-(idx)]
        obs_freqs <- obs_freqs[-(idx)]
      }
    }
    
    m <- length(exp_freqs)
    chi_sq <- sum((obs_freqs - exp_freqs)^2 / exp_freqs)
    s <- length(params)
    df <- m - 1 - s
  }
  
  # p-value
  p_val <- pchisq(chi_sq, df = df, lower.tail = FALSE)
  
  # Вывод
  conclusion <- ifelse(p_val > alpha, "Не отвергается", "Отвергается")
  
  return(list(
    chi_sq = chi_sq,
    df = df,
    p_value = p_val,
    conclusion = conclusion
  ))
}