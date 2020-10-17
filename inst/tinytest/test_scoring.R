
test_scoring <- function() {
  set.seed(2018)
  g <- 50
  d <- data.frame(
    x = 1:(3*g)) # ordered in x
  d$y_ideal <- c(rep(0, g), rep(1, g), rep(-1, g))
  d$y_observed <- d$y_ideal + rnorm(length(d$y_ideal))
  d$w <- 1 + numeric(nrow(d))
  
  # expected loss of using the mean of other points to
  # estimate each point
  inflated_var <- function(x, penalty) {
    n <- length(x)
    if(n<=1) {
      return(100000)
    }
    meanx <- mean(x)
    (n/(n-1))^2*sum((x-meanx)^2) + penalty/sqrt(n)
  }
  
  c1 <- inflated_var(d$y_observed, 0)
  c2 <- const_cost(d$y_observed, d$w, 1, 0, length(d$y_observed-1)-1)
  expect_true(abs(c1-c2)<1.e-6)
  
  y_permuted <- d$y_ideal[sample.int(nrow(d), nrow(d), replace = FALSE)]
  
  create_cost_matrix <- function(ycol, penalty) {
    n <- length(ycol)
    x <- matrix(0, nrow=n, ncol=n)
    for(i in 1:n) {
      x[i,i] <- 100000 # big penalty
      if(i<n) {
        for(j in (i+1):n) {
          cost <- inflated_var(ycol[i:j], penalty)
          x[i,j] <- cost
          x[j,i] <- x[i,j]
        }
      }
    }
    x
  }
  
  m1 <- create_cost_matrix(d$y_observed, 0)
  indices = seq_len(length(d$y_observed))
  m2 <- const_costs(d$y_observed, d$w, 1, indices)
  d <- abs(m1-m2)
  for(i in seq_len(nrow(d))) {
    d[i,i] <- 0
  }
  mx <- max(d)
  
  expect_true(mx<=1.e-6)

  invisible(NULL)
}

test_scoring()

