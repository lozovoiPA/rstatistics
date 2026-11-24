source("task1_2-model-def.R")
source("task1_2-estimates.R")
source("task1_2-model-quality.R")
source("task1_2-visualizations.R")

reg_models <- list()
for (name in list("lin", "pow", "exp", "log", "sqrt")) {
  reg_models[[name]] <- get_reg_model(name)
}

for (i in seq_along(reg_models)) {
  cat("\n\n=============== МОДЕЛЬ", reg_models[[i]]$name, "=================\n")
  reg_models[[i]] <- estimate_mnk(reg_models[[i]])
  reg_models[[i]] <- estimate_variances(reg_models[[i]])
  reg_models[[i]] <- calc_confint(reg_models[[i]])
}

for (i in seq_along(reg_models)) {
  cat("\n======== Оценка адекватности и качества модели", "========\n")
  reg_models[[i]] <- test_significance(reg_models[[i]])
  reg_models[[i]] <- calc_r2(reg_models[[i]])
  reg_models[[i]] <- test_adequacy(reg_models[[i]])
  reg_models[[i]] <- compare_with_lm(reg_models[[i]])
}

plot_all_visualize(reg_models)
plot_all_with_ci(reg_models)