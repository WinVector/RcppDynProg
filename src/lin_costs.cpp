
#include <Rcpp.h>
using namespace Rcpp;



NumericVector xlin_fits(NumericVector x, NumericVector y, NumericVector w,
                        const int i, const int j);

//' lin_cost
//' 
//' Calculate cost of using linear model fit on points to estimate other points in the interval.
//' Zero indexed.
//' 
//' @param x NumericVector, x-coords of values to group.
//' @param y NumericVector, values to group in order.
//' @param w NumericVector, weights.
//' @param min_seg positive integer, minimum segment size.
//' @param i integer, first index (inclusive).
//' @param j integer, j>=i last index (inclusive);
//' @return scalar, linear cost of [i,...,j] interval (inclusive).
//' 
//' @keywords internal
//' 
//' @examples
//' 
//' lin_cost(c(1, 2, 3, 4), c(1, 2, 2, 1), c(1, 1, 1, 1), 1, 0, 3)
//' 
//' @export
// [[Rcpp::export]]
double lin_cost(NumericVector x, NumericVector y, NumericVector w,
                const int min_seg,
                const int i, const int j) {
  if(j <= (i + (min_seg-1))) {
    return std::numeric_limits<double>::max();
  }
  if(j == (i+1)) {
    // special case small systems
    // weights not needed here
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
//' @param min_seg positive integer, minimum segment size.
//' @param indices IntegerVector, ordered list of indices to pair.
//' @return xcosts NumericMatix, for j>=i xcosts(i,j) is the cost of partition element [i,...,j] (inclusive).
//' 
//' @examples
//' 
//' lin_costs(c(1, 2, 3, 4), c(1, 2, 2, 1), c(1, 1, 1, 1), 1, 1:4)
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix lin_costs(NumericVector x, NumericVector y, NumericVector w,
                        const int min_seg,
                        IntegerVector indices) {
  const int n = indices.size();
  NumericMatrix xcosts = NumericMatrix(n, n);
  const double single_value = std::numeric_limits<double>::max();
  for(int i=0; i<n; ++i) {
    xcosts(i,i) = single_value;
    for(int j=i+1; j<n; ++j) {
      const double sum_loss = lin_cost(x, y, w, min_seg, indices(i)-1, indices(j)-1);
      xcosts(i,j) = sum_loss;
      xcosts(j,i) = sum_loss;
    }
  }
  return xcosts;
}
