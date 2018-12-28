
#include <Rcpp.h>
using namespace Rcpp;


//' xlin_fits
//' 
//' Calculate out of sample linear fit predictions.
//' Zero indexed.
//' 
//' @param x NumericVector, x-coords of values to group.
//' @param y NumericVector, values to group in order.
//' @param w NumericVector, weights.
//' @param i integer, first index (inclusive).
//' @param j integer, j>=i+2 last index (inclusive);
//' @return  vector of predictions.
//' 
//' @keywords internal
//' 
//' @export
// [[Rcpp::export]]
NumericVector xlin_fits(NumericVector x, NumericVector y, NumericVector w,
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
    const double det = xxk_0_0*xxk_1_1 - xxk_0_1*xxk_1_0;
    const double c0 = (xxk_1_1*xyk_0 - xxk_0_1*xyk_1)/det;
    const double c1 = (-xxk_1_0*xyk_0 + xxk_0_0*xyk_1)/det;
    // form estimate
    const double y_est = c0 + c1*x(k);
    fits(k-i) = y_est;
  }
  return fits;
}


//' lin_cost
//' 
//' Calculate cost of using linear model fit on points to estimate other points in the interval.
//' Zero indexed.
//' 
//' @param x NumericVector, x-coords of values to group.
//' @param y NumericVector, values to group in order.
//' @param w NumericVector, weights.
//' @param i integer, first index (inclusive).
//' @param j integer, j>=i last index (inclusive);
//' @return  linear cost of [i,...,j] interval (inclusive).
//' 
//' @keywords internal
//' 
//' @export
// [[Rcpp::export]]
double lin_cost(NumericVector x, NumericVector y, NumericVector w,
                const int i, const int j) {
  if(j<=i) {
    return std::numeric_limits<double>::max();
  }
  if(j==i+1) {
    const double diff = y(i)-y(j);
    return 2*diff*diff;
  }
  NumericVector fits = xlin_fits(x, y, w, i, j);
  double sum_loss = 0.0;
  for(int k=i; k<=j; ++k) {
    const double y_est = fits(k-i);
    const double diff = y(k) - y_est;
    const double loss = diff*diff;
    sum_loss = sum_loss + loss;
  }
  return sum_loss;
}

//' lin_costs
//' 
//' Built matrix of interval costs for held-out linear models.
//' One indexed.
//' 
//' @param x NumericVector, x-coords of values to group.
//' @param y NumericVector, values to group in order.
//' @param w NumericVector, weights.
//' @param indices IntegerVector, ordered list of indices to evaluate.
//' @return xcosts NumericMatix, for j>=i xcosts(i,j) is the cost of partition element [i,...,j] (inclusive).
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix lin_costs(NumericVector x, NumericVector y, NumericVector w,
                        IntegerVector indices) {
  const int n = indices.size();
  NumericMatrix xcosts = NumericMatrix(n, n);
  const double single_value = std::numeric_limits<double>::max();
  for(int i=0; i<n; ++i) {
    xcosts(i,i) = single_value;
    for(int j=i+1; j<n; ++j) {
      const double sum_loss = lin_cost(x, y, w, indices(i)-1, indices(j)-1);
      xcosts(i,j) = sum_loss;
      xcosts(j,i) = sum_loss;
    }
  }
  return xcosts;
}
