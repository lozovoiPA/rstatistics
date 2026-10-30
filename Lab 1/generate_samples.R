# Запуск: source("generate_samples.R")  (или Run в RStudio)
# ============================================================

# ---- 1. Индивидуальный seed (выдаётся преподавателем) ----
my_seed <- 2024L          # <-- ЗАМЕНИТЕ на свой индивидуальный seed
set.seed(my_seed)

# ---- 2. Список распределений с интервалами параметров ----
# Каждый элемент списка:
#   name  - строка-идентификатор (для преподавателя),
#   dist  - имя генератора (r-функция),
#   ranges - список интервалов для параметров (min, max),
#   is_discrete - TRUE/FALSE
distributions <- list(
  list(name = "Uniform",    dist = "runif",
       ranges = list(min = c(0, 5), max = c(6, 15)), is_discrete = FALSE),
  list(name = "Normal",     dist = "rnorm",
       ranges = list(mean = c(-5, 15), sd = c(0.5, 5)), is_discrete = FALSE),
  list(name = "Exponential",dist = "rexp",
       ranges = list(rate = c(0.2, 2.0)), is_discrete = FALSE),
  list(name = "ChiSquare",  dist = "rchisq",
       ranges = list(df = c(2, 10)), is_discrete = FALSE),
  list(name = "Poisson",    dist = "rpois",
       ranges = list(lambda = c(1, 8)), is_discrete = TRUE),
  list(name = "Binomial",   dist = "rbinom",
       ranges = list(size = c(5, 20), prob = c(0.2, 0.8)), is_discrete = TRUE),
  list(name = "Geometric",  dist = "rgeom",
       ranges = list(prob = c(0.1, 0.7)), is_discrete = TRUE),
  list(name = "Lognormal",  dist = "rlnorm",
       ranges = list(meanlog = c(0, 2), sdlog = c(0.3, 1.0)), is_discrete = FALSE),
  list(name = "Gamma",      dist = "rgamma",
       ranges = list(shape = c(1, 5), rate = c(0.5, 2.0)), is_discrete = FALSE),
  list(name = "Weibull",    dist = "rweibull",
       ranges = list(shape = c(0.8, 2.5), scale = c(1, 10)), is_discrete = FALSE)
)

# ---- 3. Вспомогательная функция: случайное значение из интервала ----
rand_param <- function(range) {
  runif(1, min = range[1], max = range[2])
}

# ---- 4. Генерация одной выборки по выбранному распределению ----
generate_one <- function(dist_info, n) {
  d <- dist_info$dist
  args <- list(n = n)
  for (pname in names(dist_info$ranges)) {
    args[[pname]] <- rand_param(dist_info$ranges[[pname]])
  }
  do.call(d, args)
}

# ---- 5. Основная функция генерации пяти выборок ----
generate_samples <- function(n_samples = 5,
                              n_min = 150, n_max = 500,
                              verbose = FALSE) {
  # случайные объёмы выборок
  n_vec <- sample(n_min:n_max, size = n_samples, replace = TRUE)
  
  # случайный выбор распределений без повторов (если возможно)
  idx <- sample(seq_along(distributions), size = n_samples, replace = FALSE)
  chosen <- distributions[idx]
  print(chosen)
  
  samples <- vector("list", n_samples)
  meta    <- data.frame(sample_id = integer(0),
                        dist_name = character(0),
                        n = integer(0),
                        stringsAsFactors = FALSE)
  
  for (i in seq_len(n_samples)) {
    x <- generate_one(chosen[[i]], n_vec[i])
    samples[[i]] <- x
    meta <- rbind(meta, data.frame(sample_id = i,
                                   dist_name = chosen[[i]]$name,
                                   n = n_vec[i],
                                   stringsAsFactors = FALSE))
  }
  
  if (verbose) {
    message("Сгенерированы выборки:")
    print(meta)
  }
  
  # сохраняем выборки в CSV
  for (i in seq_len(n_samples)) {
    write.csv(data.frame(x = samples[[i]]),
              file = sprintf("sample%d.csv", i),
              row.names = FALSE)
  }
}

# ---- 6. Запуск ----
result <- generate_samples()
