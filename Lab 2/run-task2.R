source("task2-1_3.R")
source("task2-4.R")
source("task2-5_7.R")

samples <- generate_corr_norm()
plot_scatter(samples)

get_rs(samples)
get_t_test(samples)
interval_all_calc(samples)