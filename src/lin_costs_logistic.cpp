
#include <Rcpp.h>
using Rcpp::NumericVector;
using Rcpp::NumericMatrix;
using Rcpp::IntegerVector;


NumericVector xlogistic_fits(NumericVector x, NumericVector y, 
                             NumericVector w,
                             const int i, const int j);

NumericVector logistic_fits(NumericVector x, NumericVector y, 
                            NumericVector w,
                            const int i, const int j);

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
//' @param min_seg positive integer, minimum segment size.
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
  if(j <= (i + (min_seg-1))) {
    return std::numeric_limits<double>::max();
  }
  // get corner cases
  if(j<i+1) {
    // no data or small enough for perfect fit
    return 0.0;
  }
  // look for corner cases
  double max_x = -std::numeric_limits<double>::max();
  double min_x = std::numeric_limits<double>::max();
  double max_x_pos = -std::numeric_limits<double>::max();
  double min_x_pos = std::numeric_limits<double>::max();
  double max_x_neg = -std::numeric_limits<double>::max();
  double min_x_neg = std::numeric_limits<double>::max();
  double max_y = -std::numeric_limits<double>::max();
  double min_y = std::numeric_limits<double>::max();
  double total_w = 0.0;
  for(int k=i; k<=j; ++k) {
    max_x = std::max(max_x, x(k));
    min_x = std::min(min_x, x(k));
    max_y = std::max(max_y, y(k));
    min_y = std::min(min_y, y(k));
    total_w = total_w + w(k);
    if(y(k)>=0.5) {
      max_x_pos = std::max(max_x_pos, x(k));
      min_x_pos = std::min(min_x_pos, x(k));
    } else {
      max_x_neg = std::max(max_x_neg, x(k));
      min_x_neg = std::min(min_x_neg, x(k));
    }
  }
  if(total_w<=0.0) {
    return 0.0;
  }
  if(min_y>=max_y) {
    // y-pure constant
    return 0.0;
  }
  // we now know y varies
  if(min_x<max_x) {
    // check for seperable data cases, x able to perfectly sort y
    if(min_x_pos>max_x_neg) {
      return 0.0;
    }
    if(min_x_neg>max_x_pos) {
      return 0.0;
    }
  }
  // // TODO: try to get out of sample calculation working.
  // NumericVector fits;
  // if((j-i)<=100) {
  //   fits = xlogistic_fits(x, y, w, i, j);
  // } else {
  //   fits = logistic_fits(x, y, w, i, j);
  // }
  NumericVector fits = logistic_fits(x, y, w, i, j);
  double sum_loss = 0.0;
  for(int k=i; k<=j; ++k) {
    if(w(k)>0.0) {
      const double y_est = 1/(1+std::exp(-fits(k-i)));
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
//' Built matrix of interval deviance costs for held-out logistic models.
//' Fits are evaluated in-sample.
//' One indexed.
//' 
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
//' lin_costs_logistic(c(1, 2, 3, 4, 5, 6, 7), c(0, 0, 1, 0, 1, 1, 0), c(1, 1, 1, 1, 1, 1, 1), 3, 1:7)
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
