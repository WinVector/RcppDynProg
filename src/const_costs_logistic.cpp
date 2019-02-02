
#include <RcppArmadillo.h>

#include <math.h>

using Rcpp::NumericVector;
using Rcpp::NumericMatrix;
using Rcpp::IntegerVector;


double const_cost_logistic_worker(const NumericVector &y, const NumericVector &w, 
                                  const int min_seg,
                                  const int i, const int j) {
  if(j <= (i + (min_seg-1))) {
    return std::numeric_limits<double>::max();
  }
  double w_ij = 0;
  double sum_ij = 0;
  for(int k=i; k<=j; ++k) {
    sum_ij = sum_ij + y(k)*w(k);
    w_ij = w_ij + w(k);
  }
  double sum_loss = 0.0;
  for(int k=i; k<=j; ++k) {
    if(w(k)>0.0) {
      // out of sample estimate
      const double mean_ijk = (sum_ij - y(k)*w(k))/(w_ij - w(k));
      double loss = 0.0;
      if(y(k)>0.0) {
        loss = loss + y(k)*std::log(mean_ijk);
      }
      if(y(k)<1.0) {
        loss = loss + (1.0-y(k))*std::log(1.0-mean_ijk);
      }
      sum_loss = sum_loss + w(k)*loss;
    }
  }
  return sum_loss;
}

//' const_cost_logistic
//' 
//' Calculate logistic cost of using mean of points to estimate other points in interval.
//' Zero indexed.
//' 
//' @param y NumericVector, 0/1 values to group in order (should be in interval [0,1]).
//' @param w NumericVector, weights (should be positive).
//' @param min_seg positive integer, minimum segment size.
//' @param i integer, first index (inclusive).
//' @param j integer, j>=i last index (inclusive);
//' @return scalar, const cost of [i,...,j] interval (inclusive).
//' 
//' @keywords internal
//' 
//' @examples
//' 
//' const_cost_logistic(c(0.1, 0.1, 0.2, 0.2), c(1, 1, 1, 1), 1, 0, 3)
//' 
//' @export
// [[Rcpp::export]]
double const_cost_logistic(NumericVector y, NumericVector w, 
                           const int min_seg,
                           const int i, const int j) {
  return const_cost_logistic_worker(y, w, 
                                    min_seg,
                                    i, j);
}

//' const_costs_logistic
//' 
//' Built matrix of interval logistic costs for held-out means.
//' One indexed.
//' 
//' @param y NumericVector, 0/1 values to group in order (should be in interval [0,1]).
//' @param w NumericVector, weights (should be positive).
//' @param min_seg positive integer, minimum segment size.
//' @param indices IntegerVector, order list of indices to pair.
//' @return xcosts NumericMatix, for j>=i xcosts(i,j) is the cost of partition element [i,...,j] (inclusive).
//' 
//' 
//' @examples
//' 
//' const_costs_logistic(c(0.1, 0.1, 0.2, 0.2), c(1, 1, 1, 1), 1, 1:4)
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix const_costs_logistic(NumericVector y, NumericVector w, 
                                   const int min_seg,
                                   IntegerVector indices) {
  const int n = indices.size();
  NumericMatrix xcosts = NumericMatrix(n, n);
  const double single_value = std::numeric_limits<double>::max();
  for(int i=0; i<n; ++i) {
    xcosts(i,i) = single_value;
    for(int j=i+1; j<n; ++j) {
      const double sum_loss = const_cost_logistic_worker(y, w, min_seg, indices(i)-1, indices(j)-1);
      xcosts(i,j) = sum_loss;
      xcosts(j,i) = sum_loss;
    }
  }
  return xcosts;
}
