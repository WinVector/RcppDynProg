
#include <RcppArmadillo.h>

using Rcpp::NumericVector;
using Rcpp::NumericMatrix;
using Rcpp::IntegerVector;

#include "input_summary.h"

NumericVector xlogistic_fits_worker(const NumericVector &x, const NumericVector &y, 
                                    const NumericVector &w,
                                    const int i, const int j);

NumericVector logistic_fits_worker(const NumericVector &x, const NumericVector &y, 
                                   const NumericVector &w,
                                   const int i, const int j);


double lin_cost_logistic_worker(const NumericVector &x, const NumericVector &y, 
                                const NumericVector &w,
                                const int min_seg,
                                const int i, const int j) {
  if(j <= (i + (min_seg-1))) {
    return std::numeric_limits<double>::max();
  }
  const int vlen = x.length();
  if((i<0) || (j>=vlen) || (vlen!=y.length()) || (vlen!=w.length()) || (min_seg<1)) {
    throw std::range_error("Inadmissible value");
  }
  // look for some corner cases
  const input_summary isum = input_summary(x, y, w, i, j, -1);
  if((isum.k_points<=1L)||(!isum.y_varies())) {
    // no data or small enough for perfect fit
    return 0.0;
  }
  if(isum.seperable()) {
    return 0.0;
  }
  // // TODO: try to get out of sample calculation working.
  // NumericVector fits;
  // if((j-i)<=100) {
  //   fits = xlogistic_fits_worker(x, y, w, i, j);
  // } else {
  //   fits = logistic_fits_worker(x, y, w, i, j);
  // }
  NumericVector fits = logistic_fits_worker(x, y, w, i, j);
  double sum_loss = 0.0;
  for(int k=i; k<=j; ++k) {
    if(w(k)>0.0) {
      const double y_est = 1/(1+std::exp(-fits(k-i)));
      double loss = 0.0;
      if(y(k)>0.0) {
        loss = loss + y(k)*std::log(y_est);
      }
      if(y(k)<1.0) {
        loss = loss + (1.0-y(k))*std::log(1.0-y_est);
      }
      sum_loss = sum_loss + -w(k)*2.0*loss;
    }
  }
  return sum_loss;
}

//' lin_cost_logistic logistic deviance pricing
//' 
//' Calculate deviance cost of using logistic model fit on points to estimate other points in the interval.
//' Fits are evaluated in-sample.
//' Zero indexed.
//' 
//' 
//' @param x NumericVector, x-coords of values to group.
//' @param y NumericVector, values to group in order (should be in interval [0,1]).
//' @param w NumericVector, weights (positive).
//' @param min_seg positive integer, minimum segment size (>=1).
//' @param i integer, first index (inclusive).
//' @param j integer, j>=i last index (inclusive);
//' @return scalar, linear cost of [i,...,j] interval (inclusive).
//' 
//' @keywords internal
//' 
//' @examples
//' 
//' lin_cost_logistic(c(1, 2, 3, 4, 5, 6, 7), c(0, 0, 1, 0, 1, 1, 0), c(1, 1, 1, 1, 1, 1, 1), 3, 0, 6)
//' 
//' @export
// [[Rcpp::export]]
double lin_cost_logistic(NumericVector x, NumericVector y, NumericVector w,
                         const int min_seg,
                         const int i, const int j) {
  return lin_cost_logistic_worker(x, y, w,
                                  min_seg,
                                  i, j);
}

//' lin_costs_logistic deviance costs.
//' 
//' Built matrix of interval deviance costs for held-out logistic models.
//' Fits are evaluated in-sample.
//' One indexed.
//' 
//' 
//' @param x NumericVector, x-coords of values to group.
//' @param y NumericVector, values to group in order (should be in interval [0,1]).
//' @param w NumericVector, weights (should be positive).
//' @param min_seg positive integer, minimum segment size (>=1).
//' @param indices IntegerVector, ordered list of indices to pair.
//' @return xcosts NumericMatix, for j>=i xcosts(i,j) is the cost of partition element [i,...,j] (inclusive).
//' 
//' @examples
//' 
//' lin_costs_logistic(c(1, 2, 3, 4, 5, 6, 7), c(0, 0, 1, 0, 1, 1, 0), c(1, 1, 1, 1, 1, 1, 1), 3, 1:7)
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix lin_costs_logistic(NumericVector x, NumericVector y, NumericVector w,
                                 const int min_seg,
                                 IntegerVector indices) {
  const int vlen = x.length();
  if((vlen!=y.length()) || (vlen!=w.length()) || (min_seg<1)) {
    throw std::range_error("Inadmissible value");
  }
  const int n = indices.size();
  NumericMatrix xcosts = NumericMatrix(n, n);
  const double single_value = std::numeric_limits<double>::max();
  for(int i=0; i<n; ++i) {
    xcosts(i,i) = single_value;
    for(int j=i+1; j<n; ++j) {
      const double sum_loss = lin_cost_logistic_worker(x, y, w, min_seg, indices(i)-1, indices(j)-1);
      xcosts(i,j) = sum_loss;
      xcosts(j,i) = sum_loss;
    }
  }
  return xcosts;
}
