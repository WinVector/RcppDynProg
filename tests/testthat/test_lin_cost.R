library('RcppDynProg')

context("test_lin_cost")


test_that("test_lin_cost: test scoring", {
  y <- c(1, 2, 1, -1, 4, 5, 10)
  x <- seq_len(length(y))
  w <- 1 + numeric(length(y))
  c1 <- lin_cost(x, y, w, 1, 0, length(y)-1)
  
  d <- data.frame(x = x, y = y)
  out_est <- vapply(
    seq_len(length(y)),
    function(i) {
      m <- lm(y~x, data = d[-i, ], weights = w[-i])
      predict(m, newdata = d[i, ])
    }, numeric(1))
                    
  c2 <- sum(w*(y-out_est)^2)
  
  testthat::expect_true(abs(c1-c2)<=1e-3)
})
  