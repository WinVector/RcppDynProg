


#' Piecewise linear fit coder factory.
#' 
#' Build a piecewise linear fit coder with some parameters bound in.
#' 
#' @param penalty per-segment cost penalty.
#' @param min_n_to_chunk minimum n to subdivied problem.
#' @param min_seg positive integer, minimum segment size.
#' @param max_k maximum segments to divide into.
#' @return a vtreat coder
#' 
#' @examples 
#' 
#' coder <- piecewise_linear_coder(min_seg = 1)
#' coder("x", 1:8, c(1, 2, 3, 4, 4, 3, 2, 1))
#' 
#' @export
#' 
piecewise_linear_coder <- function(penalty = 1,
                                   min_n_to_chunk = 1000,
                                   min_seg = 10,
                                   max_k = 1000) {
  force(penalty)
  force(min_n_to_chunk)
  force(min_seg)
  force(max_k)
  function(varName, x, y, w = NULL) {
    if(length(w)<=0) {
      w <- 1 + numeric(length(y))
    }
    x_cuts <- solve_for_partition(x, y, w = w, 
                                  penalty = penalty, 
                                  min_n_to_chunk = min_n_to_chunk,
                                  min_seg = min_seg,
                                  max_k = max_k)
    if(is.null(x_cuts) || (nrow(x_cuts)<=1)) {
      return(NULL)
    }
    estimate <- approx(x_cuts$x, x_cuts$pred, xout = x, method = "linear", rule = 2)$y
    attr(estimate, "approx_table") <- data.frame(predXs = x_cuts$x, predYs = x_cuts$pred)
    attr(estimate, "method") <- "linear"
    return(estimate)
  }
}


#' Piecewise linear fit.
#' 
#' \code{vtreat} custom coder based on \code{RcppDynProg::solve_for_partition()}.
#' 
#' @param varName character, name of variable to work on.
#' @param x numeric, input values.
#' @param y numeric, values to estimate.
#' @param w numeric, weights.
#' 
#' @examples 
#' 
#' piecewise_linear("x", 1:8, c(1, 2, 3, 4, 4, 3, 2, 1))
#' 
#' @export
#' 
piecewise_linear <- piecewise_linear_coder()



#' Piecewise constant fit coder factory.
#' 
#' Build a piecewise constant fit coder with some parameters bound in.
#' 
#' @param penalty per-segment cost penalty.
#' @param min_n_to_chunk minimum n to subdivied problem.
#' @param min_seg positive integer, minimum segment size.
#' @param max_k maximum segments to divide into.
#' @return a vtreat coder
#' 
#' @examples 
#' 
#' coder <- piecewise_constant_coder(min_seg = 1)
#' coder("x", 1:8, c(-1, -1, -1, -1, 1, 1, 1, 1))
#' 
#' @export
#' 
piecewise_constant_coder <- function(penalty = 1,
                                     min_n_to_chunk = 1000,
                                     min_seg = 10,
                                     max_k = 1000) {
  force(penalty)
  force(min_n_to_chunk)
  force(min_seg)
  force(max_k)
  function(varName, x, y, w = NULL) {
    if(length(w)<=0) {
      w <- 1 + numeric(length(y))
    }
    x_cuts <- solve_for_partitionc(x, y, w = w,
                                   penalty = penalty, 
                                   min_n_to_chunk = min_n_to_chunk,
                                   min_seg = min_seg,
                                   max_k = max_k)
    if(is.null(x_cuts) || (nrow(x_cuts)<=1)) {
      return(NULL)
    }
    estimate <- approx(x_cuts$x, x_cuts$pred, xout = x, method = "constant", rule = 2)$y
    attr(estimate, "approx_table") <- data.frame(predXs = x_cuts$x, predYs = x_cuts$pred)
    attr(estimate, "method") <- "constant"
    return(estimate)
  }
}

#' Piecewise constant fit.
#' 
#' \code{vtreat} custom coder based on \code{RcppDynProg::solve_for_partition()}.
#' 
#' @param varName character, name of variable to work on.
#' @param x numeric, input values.
#' @param y numeric, values to estimate.
#' @param w numeric, weights.
#' 
#' @examples 
#' 
#' piecewise_constant("x", 1:8, c(-1, -1, -1, -1, 1, 1, 1, 1))
#' 
#' @export
#' 
piecewise_constant <- piecewise_constant_coder()

