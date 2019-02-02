
#include <RcppArmadillo.h>

#include <math.h>

using Rcpp::NumericVector;

#include "input_summary.h"


input_summary::input_summary(NumericVector x, NumericVector y, 
              NumericVector w,
              const int i, const int j,
              const int skip) {
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
  
  