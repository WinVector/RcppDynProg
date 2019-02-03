
#include <RcppArmadillo.h>

#include <math.h>

using Rcpp::NumericVector;
using Rcpp::NumericMatrix;
using Rcpp::IntegerVector;


NumericVector xlin_fits_worker(const NumericVector &x, const NumericVector &y, 
                               const NumericVector &w,
                               const int i, const int j) {
  // build fitting data
  const double regularization = 1.0e-5;
  double xx_0_0 = 0;
  double xx_1_0 = 0;
  double xx_0_1 = 0;
  double xx_1_1 = 0;
  double xy_0 = 0;
  double xy_1 = 0;
  for(int k=i; k<=j; ++k) {
    xx_0_0 = xx_0_0 + w(k)*1;
    xx_1_0 = xx_1_0 + w(k)*x(k);
    xx_0_1 = xx_0_1 + w(k)*x(k);
    xx_1_1 = xx_1_1 + w(k)*x(k)*x(k);
    xy_0 = xy_0 + w(k)*y(k);
    xy_1 = xy_1 + w(k)*x(k)*y(k);
  }
  xx_1_0 = xx_1_0 + regularization;
  xx_0_1  = xx_0_1  + regularization;
  NumericVector fits = NumericVector(1+j-i);
  for(int k=i; k<=j; ++k) {
    // pull out k'th observation
    const double xxk_0_0 = xx_0_0 - w(k)*1;
    const double xxk_1_0 = xx_1_0 - w(k)*x(k);
    const double xxk_0_1 = xx_0_1 - w(k)*x(k);
    const double xxk_1_1 = xx_1_1 - w(k)*x(k)*x(k);
    const double xyk_0 = xy_0 - w(k)*y(k);
    const double xyk_1 = xy_1 - w(k)*x(k)*y(k);
    // solve linear system
    double y_est = 0.0;
    if(xx_0_0>0.0) {
      const double det = xxk_0_0*xxk_1_1 - xxk_0_1*xxk_1_0;
      if(det!=0.0) {
        const double c0 = (xxk_1_1*xyk_0 - xxk_0_1*xyk_1)/det;
        const double c1 = (-xxk_1_0*xyk_0 + xxk_0_0*xyk_1)/det;
        // form estimate
        y_est = c0 + c1*x(k);
      } else {
        y_est = xy_0/xx_0_0;
      }
    }
    fits(k-i) = y_est;
  }
  return fits;
}



//' xlin_fits
//' 
//' Calculate out of sample linear fit predictions using regularization.
//' Zero indexed.
//' 
//' @param x NumericVector, explanatory variable (length>=2).
//' @param y NumericVector, values fit.
//' @param w NumericVector, weights (positive).
//' @param i integer, first index (inclusive).
//' @param j integer, j>=i+2 last index (inclusive);
//' @return  vector of predictions.
//' 
//' @keywords internal
//' 
//' @examples
//' 
//' xlin_fits(c(1, 2, 3, 4), c(1, 2, 2, 1), c(1, 1, 1, 1), 0, 3)
//' 
//' @export
// [[Rcpp::export]]
NumericVector xlin_fits(NumericVector x, NumericVector y, NumericVector w,
                        const int i, const int j) {
  return xlin_fits_worker(x, y, 
                   w,
                   i, j);
}
