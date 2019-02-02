
test_const_cost <- function() {
  y <- c(1, 2, 1, -1, 4, 5, 10)
  w <- 1 + numeric(length(y))
  c1 <- const_cost(y, w, 1, 0, length(y)-1)
  
  out_est <- (sum(y*w) - y*w)/(sum(w) - w)
  c2 <- sum(w*(y-out_est)^2)
  
  RUnit::checkTrue(abs(c1-c2)<=1e-5)

  invisible(NULL)
}
  
