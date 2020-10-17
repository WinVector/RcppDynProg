
test_lin_cost <- function() {
  y <- c(1, 2, 1, -1, 4, 5, 10)
  x <- seq_len(length(y))
  w <- 1 + numeric(length(y))
  c1 <- lin_cost(x, y, w, 1, 0, length(y)-1)
  
  out_est <- xlin_fits_lm(x, y, w)
  c2 <- sum(w*(y-out_est)^2)
  
  expect_true(abs(c1-c2)<=1e-3)

  invisible(NULL)
}

test_lin_cost()

