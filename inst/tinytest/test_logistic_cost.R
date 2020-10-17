
test_logistic_cost <- function() {
  x <- c(1, 2, 3, 4, 5, 6, 7)
  y <- c(0, 0, 1, 0, 1, 1, 0)
  w <- c(1, 1, 1, 1, 1, 1, 1)
  il <- numeric(length(x))
  
  # Think this is the issue we are seeing in Windows.
  sm3 <- summarize_input(x,y,w,0,3,-1)
  expect3 <- list(max_x = 4, min_x = 1, saw_y_pos = TRUE, max_x_pos = 3, min_x_pos = 3, 
                  saw_y_neg = TRUE, max_x_neg = 4, min_x_neg = 1, total_w = 4, 
                  total_wy = 1, k_points = 4, saw_data = TRUE, x_varies = TRUE, 
                  y_varies = TRUE, seperable = FALSE)
  msg <- wrapr::map_to_char(sm3)
  expect_equal(sm3, expect3, info = msg)
  
  for(k in wrapr::seqi(0, 4)) {
    m1 <- logistic_solve1(x, y, w, il, 0, k, -1)
    msg <- paste("k", k, wrapr::map_to_char(m1))
    expect_true(is.numeric(m1), info = msg)
    expect_equal(2, length(m1), info = msg)
    expect_true(!any(is.na(m1)), info = msg)
    expect_true(!any(is.nan(m1)), info = msg)
    expect_true(!any(is.infinite(m1)), info = msg)
    lf <- logistic_fits(x, y, w, 0, k)
    msg <- paste("k", k, wrapr::map_to_char(m1), wrapr::map_to_char(lf))
    expect_true(is.numeric(lf), info = msg)
    expect_equal(k+1, length(lf), info = msg)
    expect_true(!any(is.na(lf)), info = msg)
    expect_true(!any(is.nan(lf)), info = msg)
    expect_true(!any(is.infinite(lf)), info = msg)
    if(k>=3) {
      d <- data.frame(x = x[1:(k+1)], y  = y[1:(k+1)])
      m <- glm(y~x, data=d, family = binomial)
      cm <- as.numeric(coef(m))
      diff1 <- max(abs(m1-cm))
      msg1 <- paste("coef problem", k, diff1, 
                    "RccpDynProg", wrapr::map_to_char(m1), 
                    "glm", wrapr::map_to_char(cm))
      expect_true(diff1<=1e-3, info = msg1)
      p <- as.numeric(predict(m, newdata = d, type = "link"))
      diff2 <- max(abs(lf-p))
      msg2 <- paste("link problem", k, diff2, 
                    "RccpDynProg", wrapr::map_to_char(lf), 
                    "glm", wrapr::map_to_char(p))
      expect_true(diff2<=1e-3, info = msg2)
    }
  }

  invisible(NULL)
}

test_logistic_cost()

