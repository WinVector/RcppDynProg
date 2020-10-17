
test_xlin <- function() {
  d <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    y = c(1, 1, 2, 2, 3, 3),
    w = c(1, 1, 1, 1, 1, 1))
  fits <- xlin_fits(d$x, d$y, d$w, 0, 5)
  
  fe <- vapply(
    1:6,
    function(i) {
      m <- lm(y~x, d[-i, , drop = FALSE])
      predict(m, newdata = d[i, , drop = FALSE])
    }, numeric(1))
  
  mxdiff = max(abs(fits-fe))
  
  expect_true(mxdiff<=1.0e-3)

  invisible(NULL)
}

test_xlin()
