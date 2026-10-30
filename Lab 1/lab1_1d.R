sample_properties <- function(x) {
  vals <- c(
    length(x),
    mean(x),
    median(x),
    var(x),
    sd(x),
    e1071::skewness(x, type = 2),      # выборочная (несмещённая) асимметрия
    e1071::kurtosis(x, type = 2),      # избыточный эксцесс (норма = 0)
    min(x), quantile(x, 0.25), median(x), quantile(x, 0.75), max(x)
  )
    
  vals[2:length(vals)] <- round(vals[2:length(vals)], 4)
  vals_char <- format(vals, nsmall = 0, drop0trailing = TRUE, trim = TRUE)
  
  stats_tbl <- data.frame(
    Показатель = c("Объём N", "Среднее", "Медиана", "Дисперсия s²",
                   "Ст. откл. s", "Асимметрия", "Эксцесс",
                   "Min", "Q1", "Q2 (медиана)", "Q3", "Max"),
    Значение = vals_char)
  return(stats_tbl)
}