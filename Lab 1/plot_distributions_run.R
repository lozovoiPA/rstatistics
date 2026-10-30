source("plot_distributions.R")

# g <- arrangeGrob(grobs = plots, ncol = 3)
p_grid
ggsave("distributions_grid.png", plot = p_grid, width = 12, height = 10, dpi = 150)

dp_grid
ggsave("densities_grid.png", plot = dp_grid, width = 12, height = 10, dpi = 150)
