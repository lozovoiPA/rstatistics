predict_original_scale <- function(reg_model, x_new) {
  a <- reg_model$a_hat
  b <- reg_model$b_hat
  
  switch(reg_model$name,
         "lin"  = a + b * x_new,
         "pow"  = a * x_new^b,
         "exp"  = a * exp(b * x_new),
         "log"  = a + b * log(x_new),
         "sqrt" = a + b * sqrt(x_new),
         stop("Неизвестная модель")
  )
}

library(ggplot2)
library(gridExtra)

# Возвращает список list(p_orig, p_lin) для исходной и линеаризованной модели
build_visualize_plots <- function(reg_model) {
  x_label_lin <- switch(reg_model$name,
                        "pow" = "ln(x)", "log" = "ln(x)", "sqrt" = "sqrt(x)",
                        "exp" = "x", "lin" = "x")
  y_label_lin <- switch(reg_model$name,
                        "pow" = "ln(y)", "exp" = "ln(y)", "log" = "y",
                        "sqrt" = "y", "lin" = "y")
  
  x_min <- min(reg_model$x) - 1
  x_max <- max(reg_model$x) + 1
  
  p_orig <- ggplot(data.frame(x = reg_model$x, y = reg_model$y), aes(x, y)) +
    geom_point(color = "steelblue", size = 1.5) +
    labs(title = paste0("Исходная (", reg_model$name, ")"), x = "x", y = "y") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10)) +
    coord_cartesian(xlim = c(x_min, x_max))
  
  p_lin <- ggplot(data.frame(x = reg_model$x_lin, y = reg_model$y_lin), aes(x, y)) +
    geom_point(color = "darkgreen", size = 1.5) +
    labs(title = paste0("Линеаризованная (", reg_model$name, ")"),
         x = x_label_lin, y = y_label_lin) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10)) +
    coord_cartesian(xlim = c(x_min, x_max))
  
  return(list(p_orig = p_orig, p_lin = p_lin))
}

build_plot_with_ci <- function(reg_model, alpha = 0.05, n_grid = 200) {
  # --- Левый график: исходная модель + нелинейная линия ---
  x <- reg_model$x
  x_grid <- seq(min(x), max(x), length.out = n_grid)
  y_grid <- predict_original_scale(reg_model, x_grid)
  
  p_orig <- ggplot() +
    geom_point(data = data.frame(x = x, y = reg_model$y), aes(x, y),
               color = "steelblue", size = 1.5) +
    geom_line(data = data.frame(x = x_grid, y = y_grid), aes(x, y),
              color = "blue", linewidth = 1) +
    labs(title = paste0("Исходная с линией регрессии (", reg_model$name, ")"), x = "x", y = "y") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10))
  
  # --- Правый график: линеаризованная модель + ДИ ---
  x_lin <- reg_model$x_lin
  x_grid_lin <- seq(min(x_lin), max(x_lin), length.out = n_grid)
  y_grid_lin <- reg_model$a_lin_hat + reg_model$b_hat * x_grid_lin
  
  t_cr <- reg_model$t_cr
  se_fit  <- sqrt(reg_model$sigma2_hat * (1/reg_model$N +
                                            (x_grid_lin - reg_model$x_bar)^2 / reg_model$Sxx))
  se_pred <- sqrt(reg_model$sigma2_hat * (1 + 1/reg_model$N +
                                            (x_grid_lin - reg_model$x_bar)^2 / reg_model$Sxx))
  
  df_grid_lin <- data.frame(
    x = x_grid_lin, y = y_grid_lin,
    fit_low  = y_grid_lin - t_cr * se_fit,
    fit_high = y_grid_lin + t_cr * se_fit,
    pred_low = y_grid_lin - t_cr * se_pred,
    pred_high= y_grid_lin + t_cr * se_pred
  )
  
  x_label_lin <- switch(reg_model$name,
                        "pow" = "ln(x)", "log" = "ln(x)", "sqrt" = "sqrt(x)",
                        "exp" = "x", "lin" = "x")
  y_label_lin <- switch(reg_model$name,
                        "pow" = "ln(y)", "exp" = "ln(y)", "log" = "y",
                        "sqrt" = "y", "lin" = "y")
  
  p_lin <- ggplot() +
    geom_ribbon(data = df_grid_lin, aes(x = x, ymin = fit_low, ymax = fit_high),
                fill = "blue", alpha = 0.15) +
    geom_ribbon(data = df_grid_lin, aes(x = x, ymin = pred_low, ymax = pred_high),
                fill = "red", alpha = 0.10) +
    geom_line(data = df_grid_lin, aes(x = x, y = y), color = "blue", linewidth = 1) +
    geom_line(data = df_grid_lin, aes(x = x, y = fit_low),  color = "blue", linetype = "dashed") +
    geom_line(data = df_grid_lin, aes(x = x, y = fit_high), color = "blue", linetype = "dashed") +
    geom_line(data = df_grid_lin, aes(x = x, y = pred_low),  color = "red",  linetype = "dotted") +
    geom_line(data = df_grid_lin, aes(x = x, y = pred_high), color = "red",  linetype = "dotted") +
    geom_point(data = data.frame(x = x_lin, y = reg_model$y_lin), aes(x, y),
               color = "grey40", size = 1.5) +
    labs(title = paste0("Линеаризованная с ДИ (", reg_model$name, ")"),
         x = x_label_lin, y = y_label_lin) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10))
  
  return(list(p_orig = p_orig, p_lin = p_lin))
}

plot_all_visualize <- function(reg_models, ncol = 2, title = "Исходные и линеаризованные модели") {
  grobs <- list()
  for (name in names(reg_models)) {
    ps <- build_visualize_plots(reg_models[[name]])
    grobs[[length(grobs) + 1]] <- ps$p_orig
    grobs[[length(grobs) + 1]] <- ps$p_lin
  }
  
  final_grid <- arrangeGrob(
    grobs = grobs, 
    ncol = 2, 
    nrow = 5
  )
  ggsave(
    filename = "vis_data.png", 
    plot = final_grid, 
    width = 14, 
    height = 16, 
    dpi = 300
  )
  #do.call(gridExtra::grid.arrange, c(grobs, ncol = ncol,
  #                                   top = title))
}

plot_all_with_ci <- function(reg_models, alpha = 0.05, ncol = 2,
                             title = "Линии регрессии и доверительные интервалы") {
  grobs <- list()
  for (name in names(reg_models)) {
    ps <- build_plot_with_ci(reg_models[[name]], alpha = alpha)
    grobs[[length(grobs) + 1]] <- ps$p_orig
    grobs[[length(grobs) + 1]] <- ps$p_lin
  }
  
  final_grid <- arrangeGrob(
    grobs = grobs, 
    ncol = 2, 
    nrow = 5
  )
  ggsave(
    filename = "vis_models.png", 
    plot = final_grid, 
    width = 14, 
    height = 16, 
    dpi = 300
  )
  #do.call(gridExtra::grid.arrange, c(grobs, ncol = ncol,
  #                                   top = title))
}