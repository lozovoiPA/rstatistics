source('plot_distributions.R')

my_params <- list(
  uniform = list(
    min = 0, 
    max = 1
  ),
  normal = list(
    mean = 0, 
    sd = 1
  ),
  exp = list(
    rate = 1
  ),
  chisq = list(
    df = 3
  ),
  lognormal = list(
    meanlog = 0, 
    sdlog = 1
  ),
  gamma = list(
    shape = 2, 
    rate = 1
  ),
  weibull = list(
    shape = 1.5, 
    scale = 1
  ),
  pois = list(
    lambda = 3
  ),
  binom = list(
    size = 10, 
    prob = 0.5
  ),
  geom = list(
    prob = 0.3
  )
)

plot_overlay_cdf(samples$X1, 
                 dists = c("normal", "uniform", "binom"), 
                 params = my_params, 
                 name = "X1")
