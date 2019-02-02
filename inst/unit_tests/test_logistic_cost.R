
test_logistic_cost <- function() {
  x <- c(1, 2, 3, 4, 5, 6, 7)
  y <- c(0, 0, 1, 0, 1, 1, 0)
  w <- c(1, 1, 1, 1, 1, 1, 1)
  
  for(k in wrapr::seqi(0, 4)) {
    lf <- logistic_fits(x, y, w, 0, k)
    RUnit::checkTrue(is.numeric(lf))
    RUnit::checkEquals(k+1, length(lf))
    RUnit::checkTrue(!any(is.na(lf)))
    RUnit::checkTrue(!any(is.nan(lf)))
    RUnit::checkTrue(!any(is.infinite(lf)))
    if(k>=3) {
      d <- data.frame(x = x[1:(k+1)], y  = y[1:(k+1)])
      m <- glm(y~x, data=d, family = binomial)
      p <- as.numeric(predict(m, newdata = d, type = "link"))
      diff <- max(abs(lf-p))
      RUnit::checkTrue(diff<=1.0e-5)
    }
  }

  invisible(NULL)
}
