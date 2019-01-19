
#include <Rcpp.h>
using Rcpp::NumericVector;
using Rcpp::NumericMatrix;
using Rcpp::IntegerVector;


NumericVector xlin_fits(NumericVector x, NumericVector y, NumericVector w,
                        const int i, const int j);

//' lin_cost_logistic logistic deviance pricing
//' 
//' Calculate deviance cost of using linear model fit on points to estimate other points in the interval.
//' Zero indexed.
//' 
//' Note: this is the deviance cost of a linear fit, not the deviance loss of a logistic fit.
//' 
//' @param x NumericVector, x-coords of values to group.
//' @param y NumericVector, values to group in order (should be in interval [0,1]).
//' @param w NumericVector, weights (positive).
//' @param min_seg positive integer, minimum segment size.
//' @param i integer, first index (inclusive).
//' @param j integer, j>=i last index (inclusive);
//' @return scalar, linear cost of [i,...,j] interval (inclusive).
//' 
//' @keywords internal
//' 
//' @examples
//' 
//' lin_cost_logistic(c(1, 2, 3, 4), c(0.1, 0.2, 0.2, 0.1), c(1, 1, 1, 1), 1, 0, 3)
//' 
//' @export
// [[Rcpp::export]]
double lin_cost_logistic(NumericVector x, NumericVector y, NumericVector w,
                const int min_seg,
                const int i, const int j) {
  if(j <= (i + (min_seg-1))) {
    return std::numeric_limits<double>::max();
  }
  NumericVector fits = xlin_fits(x, y, w, i, j);
  double sum_loss = 0.0;
  for(int k=i; k<=j; ++k) {
    if(w(k)>0.0) {
      const double y_est = fits(k-i);
      double loss = 0.0;
      if(y(k)>0.0) {
        loss = loss + y(k)*std::log(y_est);
      }
      if(y(k)<1.0) {
        loss = loss = (1.0-y(k))*std::log(1.0-y_est);
      }
      sum_loss = sum_loss + -w(k)*2.0*loss;
    }
  }
  return sum_loss;
}

//' lin_costs_logistic deviance costs.
//' 
//' Built matrix of interval deviance costs for held-out linear models.
//' One indexed.
//' 
//' Note: this is the deviance cost of a linear fit, not the deviance loss of a logistic fit.
//' 
//' @param x NumericVector, x-coords of values to group.
//' @param y NumericVector, values to group in order (should be in interval [0,1]).
//' @param w NumericVector, weights (should be positive).
//' @param min_seg positive integer, minimum segment size.
//' @param indices IntegerVector, ordered list of indices to pair.
//' @return xcosts NumericMatix, for j>=i xcosts(i,j) is the cost of partition element [i,...,j] (inclusive).
//' 
//' @examples
//' 
//' lin_costs_logistic(c(1, 2, 3, 4), c(0.1, 0.2, 0.2, 0.1), c(1, 1, 1, 1), 1, 1:4)
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix lin_costs_logistic(NumericVector x, NumericVector y, NumericVector w,
                        const int min_seg,
                        IntegerVector indices) {
  const int n = indices.size();
  NumericMatrix xcosts = NumericMatrix(n, n);
  const double single_value = std::numeric_limits<double>::max();
  for(int i=0; i<n; ++i) {
    xcosts(i,i) = single_value;
    for(int j=i+1; j<n; ++j) {
      const double sum_loss = lin_cost_logistic(x, y, w, min_seg, indices(i)-1, indices(j)-1);
      xcosts(i,j) = sum_loss;
      xcosts(j,i) = sum_loss;
    }
  }
  return xcosts;
}
