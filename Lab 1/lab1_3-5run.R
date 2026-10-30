source("lab1_1a.R") # Загрузка выборок
source("plot_distributions.R") # Графики
source("lab1_3-5.R") # Оценка и тесты

# Определяем, какие распределения мы проверяем для каждой выборки
chosen_dists <- list(
  X1 = c("normal", "lognormal", "uniform"),
  X2 = c("lognormal", "gamma", "exp", "weibull", "normal", "chisq"),
  X3 = c("normal", "lognormal", "weibull"),
  X4 = c("gamma", "lognormal", "weibull"),
  X5 = c("pois", "binom", "geom")
)

# Дискретные ли данные
discrete_flags <- list(
  X1 = FALSE, X2 = FALSE, X3 = FALSE, X4 = FALSE, X5 = TRUE
)

# Список для хранения результатов по всем выборкам
all_results <- list()

for (nm in names(samples)) {
  x <- samples[[nm]]
  message("\n=======================================================")
  message("Анализ выборки: ", nm, " (N = ", length(x), ")")
  
  dists <- chosen_dists[[nm]]
  is_disc <- discrete_flags[[nm]]
  
  # Список для сохранения результатов по текущей выборке
  results_tbl <- data.frame(
    Распределение = character(),
    Параметры_MM = character(),
    Параметры_MLE = character(),
    Chi_sq = numeric(),
    df = numeric(),
    p_value = numeric(),
    Вывод = character(),
    stringsAsFactors = FALSE
  )
  
  best_params <- list()
  best_dist <- NULL
  best_pval <- -1
  
  best_params_all <- list()
  
  for (d in dists) {
    # 1. Оцениваем параметры
    params_mm <- estimate_params_mm(x, d)
    params_mle <- estimate_params(x, d)
    
    if (is.null(params_mle)) next
    
    # Форматируем параметры в строку для таблицы
    format_params <- function(p) {
      if (is.null(p) || any(is.na(unlist(p)))) return("NA")
      paste(names(p), round(unlist(p), 3), sep = "=", collapse = ", ")
    }
    
    str_mle <- format_params(params_mle)
    str_mm  <- format_params(params_mm)
    
    # 2. Проводим тест хи-квадрат
    test_res <- chi_square_test(x, d, params_mle, is_discrete = is_disc)
    
    # Сохраняем результат
    results_tbl <- rbind(results_tbl, data.frame(
      Распределение = d,
      Параметры_MM = str_mm,
      Параметры_MLE = str_mle,
      Chi_sq = round(test_res$chi_sq, 3),
      df = test_res$df,
      p_value = round(test_res$p_value, 4),
      Вывод = test_res$conclusion
    ))
    
    # Запоминаем лучшую модель (с наибольшим p-value)
    if(test_res$p_value > 0.05) {
      best_params_all[[d]] <- params_mle
      
      if (test_res$p_value > best_pval) {
        best_pval <- test_res$p_value
        best_dist <- d
      }
    }
  }
  
  # Вывод таблицы в консоль
  print(results_tbl)
  all_results[[nm]] <- results_tbl
  
  table_grob <- tableGrob(results_tbl, rows = NULL, theme = ttheme_minimal())
  
  # 3. Визуализация
  if (!is.null(best_dist)) {
    message("Наилучшее распределение: ", best_dist, " (p-value = ", round(best_pval, 4), ")")
    
    # Строим график ECDF и теоретических CDF для всех неотвергнутых (или лучших)
    p_overlay <- plot_overlay_cdf(
      x = x, 
      dists = dists, 
      params = best_params_all,
      name = nm
    )
    # Гистограмма с наложением KDE и теоретических функций плотности
    dp_overlay <- plot_overlay_pdf(
      x = x, 
      dists = dists, 
      params = best_params_all,
      name = nm
    )
    final_plot <- wrap_elements(table_grob) / (p_overlay | dp_overlay)
    
    # Сохраняем график
    # ggsave(sprintf("Overlay_%s.png", nm), p_overlay, width = 8, height = 6, dpi = 150)
    # ggsave(sprintf("Overlay_%s_hist.png", nm), dp_overlay, width = 8, height = 6, dpi = 150)
    
  } else {
    message("Ни одно из распределений не прошло тест (p-value < 0.05).")
    final_plot <- wrap_elements(table_grob) / (plot_spacer() | plot_spacer())
  }
  
  final_plot <- final_plot + plot_layout(heights = c(1, 2))
  
  # Сохраняем итоговую картинку
  file_name <- sprintf("Results_%s.png", nm)
  ggsave(file_name, final_plot, width = 12, height = 6, dpi = 150)
}