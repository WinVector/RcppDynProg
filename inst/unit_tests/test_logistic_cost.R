
test_logistic_cost <- function() {
  x <- c(1, 2, 3, 4, 5, 6, 7)
  y <- c(0, 0, 1, 0, 1, 1, 0)
  w <- c(1, 1, 1, 1, 1, 1, 1)
  il <- numeric(length(x))
  
  for(k in wrapr::seqi(0, 4)) {
    m1 <- logistic_solve1(x, y, w, il, 0, k, -1)
    lf <- logistic_fits(x, y, w, 0, k)
    RUnit::checkTrue(is.numeric(lf))
    RUnit::checkEquals(k+1, length(lf))
    RUnit::checkTrue(!any(is.na(lf)))
    RUnit::checkTrue(!any(is.nan(lf)))
    RUnit::checkTrue(!any(is.infinite(lf)))
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
