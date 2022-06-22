make_histogram <- function(x, mu=10, sigma=2) {
  data = rnorm(x, mu, sigma)
  hist(data)
}

make_histogram(100, 10)