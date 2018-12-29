
#include <Rcpp.h>
using namespace Rcpp;

//' const_cost
//' 
//' Calculate cost of using mean of points to estimate other points in interval.
//' Zero indexed.
//' 
//' @param y NumericVector, values to group in order.
//' @param w NumericVector, weights.
//' @param min_seg positive integer, minimum segment size.
//' @param i integer, first index (inclusive).
//' @param j integer, j>=i last index (inclusive);
//' @return scalar, const cost of [i,...,j] interval (inclusive).
//' 
//' @keywords internal
//' 
//' @examples
//' 
//' const_cost(c(1, 1, 2, 2), c(1, 1, 1, 1), 1, 0, 3)
//' 
//' @export
// [[Rcpp::export]]
double const_cost(NumericVector y, NumericVector w, 
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
  double w_ij = 0;
  double sum_ij = 0;
  for(int k=i; k<=j; ++k) {
    sum_ij = sum_ij + y(k)*w(k);
    w_ij = w_ij + w(k);
  }
  double sum_loss = 0.0;
  for(int k=i; k<=j; ++k) {
    const double mean_ijk = (sum_ij - y(k)*w(k))/(w_ij - w(k));
    const double diff = y(k) - mean_ijk;
    const double loss = w(k)*diff*diff;
    sum_loss = sum_loss + loss;
  }
  return sum_loss;
}

//' const_costs
//' 
//' Built matrix of interval costs for held-out means.
//' One indexed.
//' 
//' @param y NumericVector, values to group in order.
//' @param w NumericVector, weights.
//' @param min_seg positive integer, minimum segment size.
//' @param indices IntegerVector, order list of indices to pair.
//' @return xcosts NumericMatix, for j>=i xcosts(i,j) is the cost of partition element [i,...,j] (inclusive).
//' 
//' 
//' @examples
//' 
//' const_costs(c(1, 1, 2, 2), c(1, 1, 1, 1), 1, 1:4)
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix const_costs(NumericVector y, NumericVector w, 
                          const int min_seg,
                          IntegerVector indices) {
  const int n = indices.size();
  NumericMatrix xcosts = NumericMatrix(n, n);
  const double single_value = std::numeric_limits<double>::max();
  for(int i=0; i<n; ++i) {
    xcosts(i,i) = single_value;
    for(int j=i+1; j<n; ++j) {
      const double sum_loss = const_cost(y, w, min_seg, indices(i)-1, indices(j)-1);
      xcosts(i,j) = sum_loss;
      xcosts(j,i) = sum_loss;
    }
  }
  return xcosts;
}
