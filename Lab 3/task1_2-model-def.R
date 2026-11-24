get_reg_model <- function(which){
  switch(which,
         "pow" = {
           N     <- 200
           a     <- 1
           b     <- 2
           eps_sigma <- 0.3
           
           x <- runif(N, min = 1, max = 16) # для линеаризации нужны x > 0
           eps_lin <- rnorm(N, mean = 0, sd = eps_sigma)
           eps <- exp(eps_lin)
           y <- a * x^b * eps
           x_lin <- log(x)
           y_lin <- log(y)
         },
         "exp" = {
           N     <- 200
           a     <- 0.3
           b     <- 1.1
           eps_sigma <- 0.5
           
           x <- runif(N, min = 1, max = 7)
           eps_lin <- rnorm(N, mean = 0, sd = eps_sigma)
           eps <- exp(eps_lin)
           y <- a * exp(b * x) * eps
           x_lin <- x
           y_lin <- log(y)
         },
         "log" = {
           N     <- 200
           a     <- 7.8
           b     <- 3.2
           eps_sigma <- 0.5
           
           x <- runif(N, min = 0.03, max = 10)
           eps <- rnorm(N, mean = 0, sd = eps_sigma)
           y <- a + b * log(x) + eps
           eps_lin <- eps
           x_lin <- log(x)
           y_lin <- y
         },
         "sqrt" = {
           N     <- 200
           a     <- 7.8
           b     <- 3.2
           eps_sigma <- 1
           
           x <- runif(N, min = 0, max = 10)
           eps <- rnorm(N, mean = 0, sd = eps_sigma)
           y <- a + b * sqrt(x) + eps
           eps_lin <- eps
           x_lin <- sqrt(x)
           y_lin <- y
         },
         {
           N     <- 200
           a     <- 7.8
           b     <- 3.2
           eps_sigma <- 4.5
           
           x <- rnorm(N, mean = 13, sd = 4.8)
           eps <- rnorm(N, mean = 0, sd = eps_sigma)
           y <- a + b * x + eps  
           eps_lin <- eps
           x_lin <- x
           y_lin <- y
         }
         )
  
  
  reg_model <- list("name" = which, "N" = N, "a" = a, "b" = b, "eps_sigma" = eps_sigma,
                    "x" = x, "eps" = eps, "y" = y,
                    "x_lin" = x_lin, "eps_lin" = eps_lin, y_lin = y_lin
                    )
  return(reg_model)
}

