generate_norm <- function(n = 100, mean = 0, sd = 1, condition = "a") {
  X1 <- rnorm(n, mean, sd)
  switch(
    condition,
    "b" = {
      X2 <- rnorm(n, mean + 50, sd)
    },
    "c" = {
      X2 <- rnorm(n, mean, sd * 5)
    },
    "d" = {
      X2 <- rnorm(n, mean + 50, sd * 5)
    },
    {
      X2 <- rnorm(n, mean, sd)
    }
  )
  return(list("X1" = X1, "X2" = X2))
}

generate_all_norm <- function() {
  conditions <- list(a = "a", b = "b", c = "c", d = "d")
  result <- list()
  for (condition in conditions) {
    print(condition)
    X <- generate_norm(mean = 2, sd = 3, condition = condition)
    Xarr <- list()
    for (i in seq_along(X)) {
      Xarr[[names(X)[i]]] <- list(samples = X[[i]])
    }
    result[[condition]] <- list(name = condition, X = Xarr)
    print(X)
  }
  return(result)
}
