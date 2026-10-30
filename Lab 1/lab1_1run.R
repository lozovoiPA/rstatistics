source("lab1_1a.R")
source("lab1_1bc.R")
source('lab1_1d.R')

sample_analysis <- function(x, name = "X", file_prefix = "Analysis"){
  x_properties <- sample_properties(x)
  
  p_hist <- plot_hist(x, name)
  
  p_ecdf <- plot_ecdf(x, name)
  
  table_grob <- tableGrob(x_properties, rows = NULL, theme = ttheme_minimal())
  final_plot <- wrap_elements(table_grob) | (p_hist / p_ecdf)
  final_plot <- final_plot + plot_layout(widths = c(1, 2))
  
  file_name <- sprintf("%s_%s.png", file_prefix, name)
  ggsave(file_name, final_plot, width = 7, height = 5, dpi = 150)
}

lapply(names(samples), function(nm) {
  sample_analysis(samples[[nm]], name = nm)
})




