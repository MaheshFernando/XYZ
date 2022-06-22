make_histogram <- function(x, mu=10, sigma=1) {
  data = rnorm(x, mu, sigma)
  hist(data)
}

make_histogram(100, 10)