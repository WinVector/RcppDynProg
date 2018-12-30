
#' xlin_fits_R
#' 
#' Calculate out of sample linear fit predictions.
#' 
#' @param x NumericVector, x-coords of values to group (length>=2).
#' @param y NumericVector, values to group in order.
#' @param w NumericVector, weights (positive).
#' @return  vector of predictions.
#' 
#' @keywords internal
#' 
#' @examples
#' 
#' xlin_fits_V(c(1, 2, 3, 4), c(1, 2, 2, 1), c(1, 1, 1, 1))
#' 
#' @export
#' 
xlin_fits_V <- function(x, y, w) {
  n = length(y);
  # build fitting data
  regularization = 1.0e-5;
  xx_0_0 = numeric(n) + sum(w*1);
  xx_1_0 = numeric(n) + sum(w*x);
  xx_0_1 = numeric(n) + sum(w*x);
  xx_1_1 = numeric(n) + sum(w*x*x);
  xy_0 = numeric(n) + sum(w*y);
  xy_1 = numeric(n) + sum(w*x*y);
  xx_1_0 = xx_1_0 + regularization;
  xx_0_1  = xx_0_1  + regularization;
  # pull out k'th observation
  xxk_0_0 = xx_0_0 - w*1;
  xxk_1_0 = xx_1_0 - w*x;
  xxk_0_1 = xx_0_1 - w*x;
  xxk_1_1 = xx_1_1 - w*x*x;
  xyk_0 = xy_0 - w*y;
  xyk_1 = xy_1 - w*x*y;
  # solve linear system
  det = xxk_0_0*xxk_1_1 - xxk_0_1*xxk_1_0;
  c0 = (xxk_1_1*xyk_0 - xxk_0_1*xyk_1)/det;
  c1 = (-xxk_1_0*xyk_0 + xxk_0_0*xyk_1)/det;
  # form estimate
  y_est = c0 + c1*x;
  return(y_est);
}

