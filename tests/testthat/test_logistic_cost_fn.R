library('RcppDynProg')

context("test_logistic_cost_fn")


test_that("test_logistic_cost_fn: test scoring", {
  
  d <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7),
    y = c(0, 0, 1, 0, 1, 1, 0)
  )
  w <- c(1, 1, 1, 1, 1, 1, 1)
  cost <- lin_cost_logistic(d$x, d$y, w, 3, 0, 6)
  
  m <- glm(y~x, data=d, family = binomial)
  dev <- summary(m)$deviance
  
  testthat::expect_true(abs(cost-dev)<=1e-3)
})
