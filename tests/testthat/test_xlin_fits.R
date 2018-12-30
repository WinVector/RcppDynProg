library('RcppDynProg')

context("test_xlin_fits")


test_that("test_xlin_fits", {
  x <- c(1, 2, 3, 4)
  y <- c(1, 2, 2, 1)
  w <- c(1, 1, 1, 1)
  
  f <- xlin_fits(x, y, w, 0, length(y)-1)
  f_lm <- xlin_fits_lm(x, y, w)
  testthat::expect_true(max(abs(f - f_lm))<=1e-3)
  f_R <- xlin_fits_R(x, y, w)
  testthat::expect_equivalent(f, f_R)
  f_V <- xlin_fits_V(x, y, w)
  testthat::expect_equivalent(f, f_V)
})