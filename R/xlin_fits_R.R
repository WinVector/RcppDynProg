

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
#' xlin_fits_R(c(1, 2, 3, 4), c(1, 2, 2, 1), c(1, 1, 1, 1))
#' 
#' @export
#' 
xlin_fits_R <- function(x, y, w) {
  n = length(y);
  # build fitting data
  regularization = 1.0e-5;
  xx_0_0 = 0;
  xx_1_0 = 0;
  xx_0_1 = 0;
  xx_1_1 = 0;
  xy_0 = 0;
  xy_1 = 0;
  for(k in seq_len(n)) {
    xx_0_0 = xx_0_0 + w[k]*1;
    xx_1_0 = xx_1_0 + w[k]*x[k];
    xx_0_1 = xx_0_1 + w[k]*x[k];
    xx_1_1 = xx_1_1 + w[k]*x[k]*x[k];
    xy_0 = xy_0 + w[k]*y[k];
    xy_1 = xy_1 + w[k]*x[k]*y[k];
  }
  xx_1_0 = xx_1_0 + regularization;
  xx_0_1  = xx_0_1  + regularization;
  fits = numeric(n);
  for(k in seq_len(n)) {
    # pull out k'th observation
    xxk_0_0 = xx_0_0 - w[k]*1;
    xxk_1_0 = xx_1_0 - w[k]*x[k];
    xxk_0_1 = xx_0_1 - w[k]*x[k];
    xxk_1_1 = xx_1_1 - w[k]*x[k]*x[k];
    xyk_0 = xy_0 - w[k]*y[k];
    xyk_1 = xy_1 - w[k]*x[k]*y[k];
    # solve linear system
    det = xxk_0_0*xxk_1_1 - xxk_0_1*xxk_1_0;
    c0 = (xxk_1_1*xyk_0 - xxk_0_1*xyk_1)/det;
    c1 = (-xxk_1_0*xyk_0 + xxk_0_0*xyk_1)/det;
    # form estimate
    y_est = c0 + c1*x[k];
    fits[k] = y_est;
  }
  return(fits);
}

