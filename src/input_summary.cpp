
#include <RcppArmadillo.h>

#include <math.h>

using Rcpp::NumericVector;
using Rcpp::List;

#include "input_summary.h"


input_summary::input_summary(const NumericVector &x, const NumericVector &y,
                             const NumericVector &w,
                             const int i, const int j,
                             const int skip) {
  const int vlen = x.length();
  if((i<0) || (j>=vlen) || (vlen!=y.length()) || (vlen!=w.length())) {
    throw std::range_error("Inadmissible value");
  }
  max_x = std::numeric_limits<double>::quiet_NaN();
  min_x = std::numeric_limits<double>::quiet_NaN();
  saw_y_pos = false;
  max_x_pos = std::numeric_limits<double>::quiet_NaN();
  min_x_pos = std::numeric_limits<double>::quiet_NaN();
  saw_y_neg = false;
  max_x_neg = std::numeric_limits<double>::quiet_NaN();
  min_x_neg = std::numeric_limits<double>::quiet_NaN();
  total_w = 0.0;
  total_wy = 0.0;
  k_points = 0L;
  
  for(int k=i; k<=j; ++k) {
    if((k!=skip)&&(w(k)>0)) {
      if(k_points<=0L) {
        max_x = x(k);
        min_x = x(k);
      } else {
        max_x = std::max(max_x, x(k));
        min_x = std::min(min_x, x(k));
      }
      total_w = total_w + w(k);
      total_wy = total_wy + w(k)*y(k);
      k_points = k_points + 1L;
      if(y(k)>=0.5) {
        if(!saw_y_pos) {
          saw_y_pos = true;
          max_x_pos = x(k);
          min_x_pos = x(k);
        } else {
          max_x_pos = std::max(max_x_pos, x(k));
          min_x_pos = std::min(min_x_pos, x(k));
        }
      } else {
        if(!saw_y_neg) {
          saw_y_neg = true;
          max_x_neg = x(k);
          min_x_neg = x(k);
        } else {
          max_x_neg = std::max(max_x_neg, x(k));
          min_x_neg = std::min(min_x_neg, x(k));
        }
      }
    }
  }
}

bool input_summary::saw_data() const {
  return k_points>0L;
}

bool input_summary::x_varies() const {
  return (k_points>1L)&&(min_x<max_x);
}

bool input_summary::y_varies() const {
  return (k_points>1L)&&(saw_y_neg)&&(saw_y_pos);
}

bool input_summary::seperable() const {
  if(!y_varies()) {
    return true;
  }
  if(!x_varies()) {
    return false;
  }
  if(min_x_pos>max_x_neg) {
    return true;
  }
  if(min_x_neg>max_x_pos) {
    return true;
  }
  return false;
}


//' Summarize data (for debugging).
//' 
//' @param x NumericVector, expanatory variable.
//' @param y NumericVector, 0/1 values to fit.
//' @param w NumericVector, weights (required, positive).
//' @param i integer, first index (inclusive).
//' @param j integer, last index (inclusive).
//' @param skip integer, index to skip (-1 to not skip).
//' @return summary list
//' 
//' @keywords internal
//' 
//' 
//' @examples
//' 
//' costs <- matrix(c(1.5, NA ,NA ,1 ,0 , NA, 5, -1, 1), nrow = 3)
//' solve_interval_partition(costs, nrow(costs))
//'
//' @export
// [[Rcpp::export]]
List summarize_input(NumericVector x, NumericVector y, 
                     NumericVector w,
                     const int i, const int j,
                     const int skip) {
  const input_summary isum = input_summary(x, y, w, i, j, skip);
  List ret;
  ret["max_x"] = isum.max_x;
  ret["min_x"] = isum.min_x;
  ret["saw_y_pos"] = isum.saw_y_pos;
  ret["max_x_pos"] = isum.max_x_pos;
  ret["min_x_pos"] = isum.min_x_pos;
  ret["saw_y_neg"] = isum.saw_y_neg;
  ret["max_x_neg"] = isum.max_x_neg;
  ret["min_x_neg"] = isum.min_x_neg;
  ret["total_w"] = isum.total_w;
  ret["total_wy"] = isum.total_wy;
  ret["k_points"] = isum.k_points;
  ret["saw_data"] = isum.saw_data();
  ret["x_varies"] = isum.x_varies();
  ret["y_varies"] = isum.y_varies();
  ret["seperable"] = isum.seperable();
  return ret;
}
  