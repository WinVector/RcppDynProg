library('RcppDynProg')

context("test_logistic_cost")


test_that("test_logistic_cost: test scoring", {
  
  x <- c(1, 2, 3, 4, 5, 6, 7)
  y <- c(0, 0, 1, 0, 1, 1, 0)
  w <- c(1, 1, 1, 1, 1, 1, 1)
  
  for(k in wrapr::seqi(0, 4)) {
    lf <- logistic_fits(x, y, w, 0, k)
    testthat::expect_true(is.numeric(lf))
    testthat::expect_equal(k+1, length(lf))
    testthat::expect_false(any(is.na(lf)))
    testthat::expect_false(any(is.nan(lf)))
    testthat::expect_false(any(is.infinite(lf)))
  }
})
