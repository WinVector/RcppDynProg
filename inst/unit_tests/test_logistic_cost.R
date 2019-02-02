
test_logistic_cost <- function() {
  x <- c(1, 2, 3, 4, 5, 6, 7)
  y <- c(0, 0, 1, 0, 1, 1, 0)
  w <- c(1, 1, 1, 1, 1, 1, 1)
  il <- numeric(length(x))
  
  for(k in wrapr::seqi(0, 4)) {
    m1 <- logistic_solve1(x, y, w, il, 0, k, -1)
    msg <- paste("k", k, wrapr::map_to_char(m1))
    RUnit::checkTrue(is.numeric(m1), msg = msg)
    RUnit::checkEquals(2, length(m1), msg = msg)
    RUnit::checkTrue(!any(is.na(m1)), msg = msg)
    RUnit::checkTrue(!any(is.nan(m1)), msg = msg)
    RUnit::checkTrue(!any(is.infinite(m1)), msg = msg)
    lf <- logistic_fits(x, y, w, 0, k)
    msg <- paste("k", k, wrapr::map_to_char(m1), wrapr::map_to_char(lf))
    RUnit::checkTrue(is.numeric(lf), msg = msg)
    RUnit::checkEquals(k+1, length(lf), msg = msg)
    RUnit::checkTrue(!any(is.na(lf)), msg = msg)
    RUnit::checkTrue(!any(is.nan(lf)), msg = msg)
    RUnit::checkTrue(!any(is.infinite(lf)), msg = msg)
    if(k>=3) {
      d <- data.frame(x = x[1:(k+1)], y  = y[1:(k+1)])
      m <- glm(y~x, data=d, family = binomial)
      cm <- as.numeric(coef(m))
      diff1 <- max(abs(m1-cm))
      msg1 <- paste("coef problem", k, diff1, 
                    "RccpDynProg", wrapr::map_to_char(m1), 
                    "glm", wrapr::map_to_char(cm))
      RUnit::checkTrue(diff1<=1e-3, msg = msg1)
      p <- as.numeric(predict(m, newdata = d, type = "link"))
      diff2 <- max(abs(lf-p))
      msg2 <- paste("link problem", k, diff2, 
                    "RccpDynProg", wrapr::map_to_char(lf), 
                    "glm", wrapr::map_to_char(p))
      RUnit::checkTrue(diff2<=1e-3, msg = msg2)
    }
  }

  invisible(NULL)
}
