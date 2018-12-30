
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
#' xlin_fits_lm(c(1, 2, 3, 4), c(1, 2, 2, 1), c(1, 1, 1, 1))
#' 
#' @export
#' 
xlin_fits_lm <- function(x, y, w) {
  n <- length(y)
  d <- data.frame(x = x, y = y)
  vapply(
    seq_len(n),
    function(i) {
      m <- lm(y ~ x, data = d[-i, ], weights = w[-i])
      predict(m, newdata = d[i, ])
    }, numeric(1))
}