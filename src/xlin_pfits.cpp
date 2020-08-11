
#include <RcppArmadillo.h>

#include <math.h>

using Rcpp::NumericVector;
using Rcpp::NumericMatrix;
using Rcpp::IntegerVector;



//' xlin_pfits
//' 
//' Calculate out of sample linear fit predictions using pseudo-inverse.
//' Please see: \url{https://win-vector.com/2019/01/08/a-beautiful-2-by-2-matrix-identity/}.
//' Zero indexed.
//' 
//' @param x NumericVector, explanatory variable (length>=2).
//' @param y NumericVector, values to fit.
//' @param w NumericVector, weights (positive).
//' @param i integer, first index (inclusive).
//' @param j integer, j>=i+2 last index (inclusive);
//' @return  vector of predictions.
//' 
//' @keywords internal
//' 
//' @examples
//' 
//' xlin_pfits(c(1, 2, 3, 4), c(1, 2, 2, 1), c(1, 1, 1, 1), 0, 3)
//' 
//' @export
// [[Rcpp::export]]
NumericVector xlin_pfits(NumericVector x, NumericVector y, NumericVector w,
                        const int i, const int j) {
  // build fitting data
  double xx_0_0 = 0;
  double xx_1_0 = 0;
  double xx_0_1 = 0;
  double xx_1_1 = 0;
  double sy_0 = 0;
  double xy_1 = 0;
  double sum_w = 0;
  for(int k=i; k<=j; ++k) {
    xx_0_0 = xx_0_0 + w(k)*1;
    xx_1_0 = xx_1_0 + w(k)*x(k);
    xx_0_1 = xx_0_1 + w(k)*x(k);
    xx_1_1 = xx_1_1 + w(k)*x(k)*x(k);
    sy_0 = sy_0 + w(k)*y(k);
    xy_1 = xy_1 + w(k)*x(k)*y(k);
    sum_w = sum_w + w(k);
  }
  NumericVector fits = NumericVector(1+j-i);
  for(int k=i; k<=j; ++k) {
    // pull out k'th observation
    const double xxk_0_0 = xx_0_0 - w(k)*1;
    const double xxk_1_0 = xx_1_0 - w(k)*x(k);
    const double xxk_0_1 = xx_0_1 - w(k)*x(k);
    const double xxk_1_1 = xx_1_1 - w(k)*x(k)*x(k);
    const double syk_0 = sy_0 - w(k)*y(k);
    const double xyk_1 = xy_1 - w(k)*x(k)*y(k);
    const double det = xxk_0_0*xxk_1_1 - xxk_0_1*xxk_1_0;
    if(det!=0.0) {
      // solve linear system and form estimate
      const double c0 = (xxk_1_1*syk_0 - xxk_0_1*xyk_1)/det;
      const double c1 = (-xxk_1_0*syk_0 + xxk_0_0*xyk_1)/det;
      // form estimate
      const double y_est = c0 + c1*x(k);
      fits(k-i) = y_est;
    } else {
      // estimate directly from hold-out mean
      fits(k-i) = syk_0/(sum_w - w(k));
    }
  }
  return fits;
}
