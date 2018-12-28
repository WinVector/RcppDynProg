


#' Piecewise linear fit.
#' 
#' \code{vtreat} custom coder based on \code{RcppDynProg::solve_for_partition()}.
#' 
#' @param varName character, name of variable to work on.
#' @param x numeric, input values.
#' @param y numeric, values to estimate.
#' @param w numeric, weights.
#' 
#' @export
#' 
piecewise_linear <- function(varName, x, y, w = NULL) {
  x_cuts <- solve_for_partition(x, y, w = w, penalty = 10, max_k = 1000)
  estimate <- approx(x_cuts$x, x_cuts$pred, xout = x, method = "linear", rule = 2)$y
  attr(estimate, "approx_table") <- data.frame(predXs = x_cuts$x, predYs = x_cuts$pred)
  attr(estimate, "method") <- "linear"
  return(estimate)
}


#' Piecwise constant fit.
#' 
#' \code{vtreat} custom coder based on \code{RcppDynProg::solve_for_partition()}.
#' 
#' @param varName character, name of variable to work on.
#' @param x numeric, input values.
#' @param y numeric, values to estimate.
#' @param w numeric, weights.
#' 
#' @export
#' 
piecewise_constant <- function(varName, x, y, w = NULL) {
  x_cuts <- solve_for_partitionc(x, y, w = w, penalty = 10, max_k = 1000)
  estimate <- approx(x_cuts$x, x_cuts$pred, xout = x, method = "constant", rule = 2)$y
  attr(estimate, "approx_table") <- data.frame(predXs = x_cuts$x, predYs = x_cuts$pred)
  attr(estimate, "method") <- "constant"
  return(estimate)
}

