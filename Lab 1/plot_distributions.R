library(ggplot2)
library(gridExtra)
library(patchwork)

plot_overlay_pdf <- function(x, 
                             dists = c("normal", "gamma", "lognormal"), 
                             params = list(),
                             name = "X") {
  
  # 1. Базовый датафрейм
  df_obs <- data.frame(x = x)
  
  # Создаем плотную последовательность для теоретических линий
  x_seq <- seq(min(x), max(x), length.out = 500)
  
  df_theory_list <- list()
  labels_and_colors <- list()
  
  base_colors <- c(
    uniform   = "steelblue",
    normal    = "brown",
    exp       = "firebrick",
    chisq     = "purple",
    lognormal = "darkorange",
    gamma     = "forestgreen",
    weibull   = "darkcyan",
    pois      = "orchid",
    binom     = "darkblue",
    geom      = "pink"
  )
  
  for (d in dists) {
    p <- params[[d]]
    if (is.null(p)) next
    
    # Вычисляем плотность или вероятность для каждой функции
    res <- switch(d,
                  normal = {
                    y <- dnorm(x_seq, mean = p$mean, sd = p$sd)
                    list(y = y, label = sprintf("Normal(μ=%.2f, σ=%.2f)", p$mean, p$sd), type = "line")
                  },
                  gamma = {
                    y <- dgamma(x_seq, shape = p$shape, rate = p$rate)
                    list(y = y, label = sprintf("Gamma(shape=%.2f, rate=%.2f)", p$shape, p$rate), type = "line")
                  },
                  lognormal = {
                    y <- dlnorm(x_seq, meanlog = p$meanlog, sdlog = p$sdlog)
                    list(y = y, label = sprintf("Log-normal(μ=%.2f, σ=%.2f)", p$meanlog, p$sdlog), type = "line")
                  },
                  exp = {
                    y <- dexp(x_seq, rate = p$rate)
                    list(y = y, label = sprintf("Exp(λ=%.2f)", p$rate), type = "line")
                  },
                  weibull = {
                    y <- dweibull(x_seq, shape = p$shape, scale = p$scale)
                    list(y = y, label = sprintf("Weibull(k=%.2f, λ=%.2f)", p$shape, p$scale), type = "line")
                  },
                  uniform = {
                    y <- dunif(x_seq, min = p$min, max = p$max)
                    list(y = y, label = sprintf("Uniform[%.2f, %.2f]", p$min, p$max), type = "line")
                  },
                  chisq = {
                    y <- dchisq(x_seq, df = p$df)
                    list(y = y, label = sprintf("Chi-sq(df=%.2f)", p$df), type = "line")
                  },
                  pois = {
                    # Для дискретных используем вероятность. Так как x_seq непрерывен, 
                    # округляем для получения "ступенек"
                    y <- dpois(round(x_seq), lambda = p$lambda)
                    list(y = y, label = sprintf("Poisson(λ=%.2f)", p$lambda), type = "step")
                  },
                  binom = {
                    y <- dbinom(round(x_seq), size = p$size, prob = p$prob)
                    list(y = y, label = sprintf("Binom(n=%g, p=%.2f)", p$size, p$prob), type = "step")
                  },
                  geom = {
                    y <- dgeom(round(x_seq), prob = p$prob)
                    list(y = y, label = sprintf("Geom(p=%.2f)", p$prob), type = "step")
                  },
                  {
                    warning(sprintf("Распределение '%s' не поддерживается", d))
                    list(y = NULL, label = "", type = "none")
                  }
    )
    
    if (!is.null(res$y)) {
      df_theory_list[[d]] <- data.frame(
        x = x_seq, 
        y = res$y, 
        Distribution = res$label,
        Type = res$type
      )
      labels_and_colors[[res$label]] <- unname(base_colors[d])
    }
  }
  
  # Разделяем данные для непрерывных (линии) и дискретных (ступеньки)
  df_theory <- do.call(rbind, df_theory_list)
  df_lines <- subset(df_theory, Type == "line")
  df_steps <- subset(df_theory, Type == "step")
  
  # Добавляем легенду для гистограммы и KDE
  labels_and_colors[["Гистограмма"]] <- "steelblue"
  labels_and_colors[["KDE"]] <- "darkred"
  
  color_vector <- unlist(labels_and_colors)
  
  # 3. Строим итоговый график
  p_final <- ggplot() +
    # Гистограмма
    geom_histogram(data = df_obs, aes(x = x, y = after_stat(density), fill = "Гистограмма"), 
                   bins = nclass.Sturges(x), color = "white", alpha = 0.5) +
    
    # KDE
    geom_density(data = df_obs, aes(x = x, color = "KDE"), 
                 linewidth = 1.2, alpha = 1) +
    
    # Непрерывные теоретические плотности (полупрозрачные линии)
    {if(nrow(df_lines) > 0) geom_line(data = df_lines, 
                                      aes(x = x, y = y, color = Distribution), 
                                      linewidth = 1, alpha = 0.7)} +
    
    # Дискретные теоретические вероятности (ступеньки)
    {if(nrow(df_steps) > 0) geom_step(data = df_steps, 
                                      aes(x = x, y = y, color = Distribution), 
                                      linewidth = 1, alpha = 0.7, direction = "hv")} +
    
    # Настройка цветов для линий
    scale_color_manual(name = "Распределения", values = color_vector) +
    
    # Настройка цвета заливки гистограммы (чтобы не было дубля в легенде)
    scale_fill_manual(name = "Распределения", values = c("Гистограмма" = "steelblue"), guide = "none") +
    
    labs(title = sprintf("%s: Гистограмма, KDE и теоретические плотности", name),
         x = "x", y = "Плотность / Вероятность") +
    theme_minimal() +
    theme(legend.position = "right",
          legend.direction = "vertical") +
    guides(color = guide_legend(ncol = 1))
  
  return(p_final)
}

plot_overlay_cdf <- function(x, 
                             dists = c("normal", "gamma", "lognormal"), 
                             params = list(),
                             name = "X") {
  
  # 1. Готовим базовый датафрейм с эмпирической функцией (ECDF)
  df_ecdf <- data.frame(x = x)
  
  # Создаем последовательность для теоретических линий
  x_seq <- seq(min(x), max(x), length.out = 500)
  
  # 2. Собираем данные для выбранных теоретических CDF
  df_theory_list <- list()
  labels_and_colors <- list()
  
  base_colors <- c(
    uniform   = "steelblue",
    normal    = "brown",
    exp       = "firebrick",
    chisq     = "purple",
    lognormal = "darkorange",
    gamma     = "forestgreen",
    weibull   = "darkcyan",
    pois      = "orchid",
    binom     = "darkblue",
    geom      = "pink"
  )
  
  for (d in dists) {
    p <- params[[d]]
    
    if (is.null(p)) next
    
    # Вычисляем CDF для каждой выбранной функции
    res <- switch(d,
                  normal = {
                    y <- pnorm(x_seq, mean = p$mean, sd = p$sd)
                    list(y = y, label = sprintf("Normal(μ=%.2f, σ=%.2f)", p$mean, p$sd))
                  },
                  gamma = {
                    y <- pgamma(x_seq, shape = p$shape, rate = p$rate)
                    list(y = y, label = sprintf("Gamma(shape=%.2f, rate=%.2f)", p$shape, p$rate))
                  },
                  lognormal = {
                    y <- plnorm(x_seq, meanlog = p$meanlog, sdlog = p$sdlog)
                    list(y = y, label = sprintf("Log-normal(μ=%.2f, σ=%.2f)", p$meanlog, p$sdlog))
                  },
                  exp = {
                    y <- pexp(x_seq, rate = p$rate)
                    list(y = y, label = sprintf("Exp(λ=%.2f)", p$rate))
                  },
                  weibull = {
                    y <- pweibull(x_seq, shape = p$shape, scale = p$scale)
                    list(y = y, label = sprintf("Weibull(k=%.2f, λ=%.2f)", p$shape, p$scale))
                  },
                  uniform = {
                    y <- punif(x_seq, min = p$min, max = p$max)
                    list(y = y, label = sprintf("Uniform[%.2f, %.2f]", p$min, p$max))
                  },
                  chisq = {
                    y <- pchisq(x_seq, df = p$df)
                    list(y = y, label = sprintf("Chi-sq(df=%.2f)", p$df))
                  },
                  pois = {
                    y <- ppois(floor(x_seq), lambda = p$lambda) # floor для дискретных
                    list(y = y, label = sprintf("Poisson(λ=%.2f)", p$lambda))
                  },
                  binom = {
                    y <- pbinom(round(x_seq), size = p$size, prob = p$prob)
                    list(y = y, label = sprintf("Binom(n=%g, p=%.2f)", p$size, p$prob))
                  },
                  geom = {
                    y <- pgeom(floor(x_seq), prob = p$prob)
                    list(y = y, label = sprintf("Geom(p=%.2f)", p$prob))
                  },
                  {
                    warning(sprintf("Распределение '%s' не поддерживается", d))
                    list(y = NULL, label = "")
                  }
    )
    
    if (!is.null(res$y)) {
      # Добавляем данные для линии
      df_theory_list[[d]] <- data.frame(
        x = x_seq, 
        y = res$y, 
        Distribution = res$label
      )
      
      # Собираем цвет для этой метки
      labels_and_colors[[res$label]] <- unname(base_colors[d])
    }
  }
  labels_and_colors[["Эмпирическая (ECDF)"]] <- "darkgreen"
  
  
  # Объединяем все теоретические линии в один датафрейм
  df_theory <- do.call(rbind, df_theory_list)
  color_vector <- unlist(labels_and_colors)
  
  # 3. Строим итоговый график
  p_final <- ggplot() +
    stat_ecdf(data = df_ecdf, aes(x = x, color = "Эмпирическая (ECDF)"), 
              linewidth = 1.2, alpha = 0.8) +
    geom_line(data = df_theory, 
              aes(x = x, y = y, color = Distribution), 
              linewidth = 1, alpha = 0.6) +
    
    # Используем динамический вектор цветов
    scale_color_manual(name = "Распределения", values = color_vector) +
    
    labs(title = sprintf("%s: ECDF vs Теоретические CDF", name),
         x = "x", y = "F(x)") +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.direction = "vertical"
    ) +
    guides(
      color = guide_legend(ncol = 1)
    )
  
  return(p_final)
}

plot_duniform <- function(a = 0, b = 1, n = 1000) {
  x <- seq(a - 0.2 * (b - a), b + 0.2 * (b - a), length.out = n)
  y <- dunif(x, a, b)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_area(data = subset(df, x >= a & x <= b), fill = "steelblue", alpha = 0.3) +
    labs(title = sprintf("Uniform U[%g, %g] (PDF)", a, b), x = "x", y = "f(x)") + 
    theme_minimal()
}

plot_puniform <- function(a = 0, b = 1, n = 1000) {
  x <- seq(a - 0.2 * (b - a), b + 0.2 * (b - a), length.out = n)
  y <- punif(x, a, b)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "steelblue", linewidth = 1) +
    labs(title = sprintf("Uniform U[%g, %g] (CDF)", a, b), x = "x", y = "F(x)") + 
    theme_minimal()
}

plot_dnormal <- function(mu = 0, sigma2 = 1, n = 1000) {
  sigma <- sqrt(sigma2)
  x <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = n)
  y <- dnorm(x, mu, sigma)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "darkgreen", linewidth = 1) +
    geom_area(fill = "darkgreen", alpha = 0.3) +
    labs(title = sprintf("Normal N(%g, %g) (PDF)", mu, sigma2), x = "x", y = "f(x)") + 
    theme_minimal()
}

plot_pnormal <- function(mu = 0, sigma2 = 1, n = 1000) {
  sigma <- sqrt(sigma2)
  x <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = n)
  y <- pnorm(x, mu, sigma)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "darkgreen", linewidth = 1) +
    labs(title = sprintf("Normal N(%g, %g) (CDF)", mu, sigma2), x = "x", y = "F(x)") + 
    theme_minimal()
}

plot_dexp <- function(lambda = 1, n = 1000) {
  x <- seq(0, qexp(0.999, lambda), length.out = n)
  y <- dexp(x, lambda)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "firebrick", linewidth = 1) +
    geom_area(fill = "firebrick", alpha = 0.3) +
    labs(title = sprintf("Exp(λ = %g) (PDF)", lambda), x = "x", y = "f(x)") + 
    theme_minimal()
}

plot_pexp <- function(lambda = 1, n = 1000) {
  x <- seq(0, qexp(0.999, lambda), length.out = n)
  y <- pexp(x, lambda)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "firebrick", linewidth = 1) +
    labs(title = sprintf("Exp(λ = %g) (CDF)", lambda), x = "x", y = "F(x)") + 
    theme_minimal()
}

plot_dchisq <- function(k = 3, n = 1000) {
  x <- seq(0, qchisq(0.999, k), length.out = n)
  y <- dchisq(x, k)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "purple", linewidth = 1) +
    geom_area(fill = "purple", alpha = 0.3) +
    labs(title = sprintf("χ²(k = %g) (PDF)", k), x = "x", y = "f(x)") + 
    theme_minimal()
}

plot_pchisq <- function(k = 3, n = 1000) {
  x <- seq(0, qchisq(0.999, k), length.out = n)
  y <- pchisq(x, k)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "purple", linewidth = 1) +
    labs(title = sprintf("χ²(k = %g) (CDF)", k), x = "x", y = "F(x)") + 
    theme_minimal()
}

plot_dlognormal <- function(mu = 0, sigma = 1, n = 1000) {
  x <- seq(0, qlnorm(0.999, mu, sigma), length.out = n)
  y <- dlnorm(x, mu, sigma)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "darkorange", linewidth = 1) +
    geom_area(fill = "darkorange", alpha = 0.3) +
    labs(title = sprintf("Log-normal (μlog = %g, σlog = %g) (PDF)", mu, sigma), x = "x", y = "f(x)") + 
    theme_minimal()
}

plot_plognormal <- function(mu = 0, sigma = 1, n = 1000) {
  x <- seq(0, qlnorm(0.999, mu, sigma), length.out = n)
  y <- plnorm(x, mu, sigma)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "darkorange", linewidth = 1) +
    labs(title = sprintf("Log-normal (μlog = %g, σlog = %g) (CDF)", mu, sigma), x = "x", y = "F(x)") + 
    theme_minimal()
}

plot_dgamma <- function(shape = 2, rate = 1, n = 1000) {
  x <- seq(0, qgamma(0.999, shape, rate), length.out = n)
  y <- dgamma(x, shape, rate)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "brown", linewidth = 1) +
    geom_area(fill = "brown", alpha = 0.3) +
    labs(title = sprintf("Gamma(shape = %g, rate = %g) (PDF)", shape, rate), x = "x", y = "f(x)") + 
    theme_minimal()
}

plot_pgamma <- function(shape = 2, rate = 1, n = 1000) {
  x <- seq(0, qgamma(0.999, shape, rate), length.out = n)
  y <- pgamma(x, shape, rate)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "brown", linewidth = 1) +
    labs(title = sprintf("Gamma(shape = %g, rate = %g) (CDF)", shape, rate), x = "x", y = "F(x)") + 
    theme_minimal()
}

plot_dweibull <- function(shape = 1.5, scale = 1, n = 1000) {
  x <- seq(0, qweibull(0.999, shape, scale), length.out = n)
  y <- dweibull(x, shape, scale)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "darkcyan", linewidth = 1) +
    geom_area(fill = "darkcyan", alpha = 0.3) +
    labs(title = sprintf("Weibull(shape = %g, scale = %g) (PDF)", shape, scale), x = "x", y = "f(x)") + 
    theme_minimal()
}

plot_pweibull <- function(shape = 1.5, scale = 1, n = 1000) {
  x <- seq(0, qweibull(0.999, shape, scale), length.out = n)
  y <- pweibull(x, shape, scale)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_line(color = "darkcyan", linewidth = 1) +
    labs(title = sprintf("Weibull(shape = %g, scale = %g) (CDF)", shape, scale), x = "x", y = "F(x)") + 
    theme_minimal()
}

plot_dpois <- function(lambda = 3) {
  x <- 0:qpois(0.999, lambda)
  y <- dpois(x, lambda)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    labs(title = sprintf("Poisson(λ = %g) (PMF)", lambda), x = "k", y = "P(X = k)") + 
    theme_minimal()
}

plot_ppois <- function(lambda = 3) {
  x <- 0:qpois(0.999, lambda)
  y <- ppois(x, lambda)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_step(color = "steelblue", linewidth = 1, direction = "hv") +
    geom_point(color = "steelblue", size = 2) +
    labs(title = sprintf("Poisson(λ = %g) (CDF)", lambda), x = "k", y = "F(k)") + 
    theme_minimal()
}

plot_dbinom <- function(n = 10, p = 0.5) {
  x <- 0:n
  y <- dbinom(x, n, p)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_col(fill = "darkgreen", alpha = 0.7) +
    labs(title = sprintf("Binomial(n = %g, p = %g) (PMF)", n, p), x = "k", y = "P(X = k)") + 
    theme_minimal()
}

plot_pbinom <- function(n = 10, p = 0.5) {
  x <- 0:n
  y <- pbinom(x, n, p)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_step(color = "darkgreen", linewidth = 1, direction = "hv") +
    geom_point(color = "darkgreen", size = 2) +
    labs(title = sprintf("Binomial(n = %g, p = %g) (CDF)", n, p), x = "k", y = "F(k)") + 
    theme_minimal()
}

plot_dgeom <- function(p = 0.3) {
  x <- 0:qgeom(0.999, p)
  y <- dgeom(x, p)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_col(fill = "purple", alpha = 0.7) +
    labs(title = sprintf("Geometric(p = %g) (PMF)", p), x = "k", y = "P(X = k)") + 
    theme_minimal()
}

plot_pgeom <- function(p = 0.3) {
  x <- 0:qgeom(0.999, p)
  y <- pgeom(x, p)
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_step(color = "purple", linewidth = 1, direction = "hv") +
    geom_point(color = "purple", size = 2) +
    labs(title = sprintf("Geometric(p = %g) (CDF)", p), x = "k", y = "F(k)") + 
    theme_minimal()
}


dplots <- list(
  plot_duniform(0, 1), plot_dnormal(0, 1), plot_dexp(1),
  plot_dchisq(3), plot_dlognormal(0, 0.5), plot_dgamma(2, 1),
  plot_dweibull(1.5, 1), plot_dpois(3), plot_dbinom(10, 0.5),
  plot_dgeom(0.3)
)

pplots <- list(
  plot_puniform(0, 1), plot_pnormal(0, 1), plot_pexp(1),
  plot_pchisq(3), plot_plognormal(0, 0.5), plot_pgamma(2, 1),
  plot_pweibull(1.5, 1), plot_ppois(3), plot_pbinom(10, 0.5),
  plot_pgeom(0.3)
)

dp_grid <- wrap_plots(dplots, ncol = 3)
p_grid  <- wrap_plots(pplots, ncol = 3)

grids <- list(dgrid = dp_grid, pgrid = p_grid)