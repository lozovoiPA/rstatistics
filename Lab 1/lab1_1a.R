library(e1071)
library(moments)
library(ggplot2)
library(gridExtra)
library(readr)

# ---- 1a. Импорт данных ----

# Чтение .csv
read_sample <- function(path) {
  df <- read_csv(path, show_col_types = FALSE)
  x <- as.numeric(df[[1]])
  x <- x[!is.na(x)]
  x
}

sample_files <- paste0("sample", 1:5, ".csv")

# Если нет файлов, то запускаем генерацию выборок
if (any(!file.exists(sample_files))) {
  source("generate_samples.R")
  sample_files <- paste0("sample", 1:5, ".csv")
  if (any(!file.exists(sample_files))) {
    message("Файлы не найдены.")
    quit("no")
  }
}

samples <- lapply(sample_files, read_sample)
names(samples) <- paste0("X", 1:5)