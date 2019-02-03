

#include <RcppArmadillo.h>

#include <math.h> 

// [[Rcpp::depends(RcppArmadillo)]]

using Rcpp::NumericVector;

#include "input_summary.h"

double logit(const double x) {
  return log(x/(1.0-x));
}

double sigmoid(const double x) {
  return 1/(1+exp(-x));
}

NumericVector logistic_solve1_worker(const NumericVector &x, const NumericVector &y, 
                                     const NumericVector &w,
                                     const NumericVector &initial_link,
                                     const int i, const int j,
                                     const int skip) {
  // init return structure
  NumericVector coef = NumericVector(2);
  // look for corner cases
  const input_summary isum = input_summary(x, y, w, i, j, skip);
  // no-data cases
  if(isum.k_points<=0L) {
    coef(0) = 0;
    coef(1) = 0;
    return coef;
  }
  if(!isum.y_varies()) {
    // y-pure
    if(isum.saw_y_pos) {
      coef(0) = std::numeric_limits<double>::max();
      coef(1) = 0;
    } else {
      coef(0) = -std::numeric_limits<double>::max();
      coef(1) = 0;
    }
    return(coef);
  }
  // we now know y varies
  if(!isum.x_varies()) {
    // x not varying case
    coef(0) = logit(isum.total_wy/isum.total_w);
    coef(1) = 0;
    return(coef);
  }
  if(isum.seperable()) {
    // check for seperable data cases, x able to perfectly sort y
    if(isum.min_x_pos>isum.max_x_neg) {
      // Note: no solution possible in this case, just returning info
      coef(0) = logit(isum.total_wy/isum.total_w);
      coef(1) = std::numeric_limits<double>::max();
      return(coef);
    } else {
      // Note: no solution possible in this case, just returning info
      coef(0) = logit(isum.total_wy/isum.total_w);
      coef(1) = -std::numeric_limits<double>::max();
      return(coef);
    }
  }
  // Set up structers for IRwLS
  // https://www.cs.purdue.edu/homes/zhang923/notes/irwls.pdf
  arma::colvec link(j-i+1, arma::fill::zeros);
  arma::colvec preds(j-i+1, arma::fill::zeros);
  arma::colvec z(j-i+1, arma::fill::zeros);
  arma::colvec wadj(j-i+1, arma::fill::ones);
  for(int k=i; k<=j; ++k) {
    if(k!=skip) {
      link(k-i) = initial_link(k);
    }
  }
  // Iteratively re-weighted least squares solution
  double c0 = 0.0;
  double c1 = 0.0;
  double diff = 1.0;
  for(int rep = 0; (rep<20) && (diff>1.e-6); ++rep) {
    // build composite weights and z-target
    for(int k=i; k<=j; ++k) {
      if(k!=skip) {
        preds(k-i) = sigmoid(link(k-i));
        z(k-i) = link(k-i) + (y(k) - preds(k-i))/(preds(k-i)*(1.0-preds(k-i)));
        wadj(k-i) = preds(k-i)*(1.0-preds(k-i))*w(k);
      }
    }
    // build linear fitting data
    double xx_0_0 = 0;
    double xx_1_0 = 0;
    double xx_0_1 = 0;
    double xx_1_1 = 0;
    double sy_0 = 0;
    double xy_1 = 0;
    double sum_w = 0;
    for(int k=i; k<=j; ++k) {
      if(k!=skip) {
        xx_0_0 = xx_0_0 + wadj(k-i)*1.0;
        xx_1_0 = xx_1_0 + wadj(k-i)*x(k);
        xx_0_1 = xx_0_1 + wadj(k-i)*x(k);
        xx_1_1 = xx_1_1 + wadj(k-i)*x(k)*x(k);
        sy_0 = sy_0 + wadj(k-i)*z(k-i);
        xy_1 = xy_1 + wadj(k-i)*x(k)*z(k-i);
        sum_w = sum_w + wadj(k-i);
      }
    }
    // fit linear model
    c0 = 0.0;
    c1 = 0.0;
    if(sum_w>0.0) {
      const double det = xx_0_0*xx_1_1 - xx_0_1*xx_1_0;
      if(det!=0) {
        // solve linear system and form estimate
        c0 = (xx_1_1*sy_0 - xx_0_1*xy_1)/det;
        c1 = (-xx_1_0*sy_0 + xx_0_0*xy_1)/det;
      } else {
        c0 = sy_0/sum_w;
        c1 = 0.0;
      }
    }
    // build next link
    diff = 0.0;
    for(int k=i; k<=j; ++k) {
      if(k!=skip) {
        const double nvi = c0 + c1*x(k);
        const double diffi = nvi - link(k-i);
        diff = diff + diffi*diffi;
        link(k-i) = nvi;
      }
    }
  }
  coef(0) = c0;
  coef(1) = c1;
  return coef;
}


//' logistic_fit
//' 
//' Calculate y ~ sigmoid(a + b x) using iteratively re-weighted least squares.
//' Zero indexed.
//' 
//' @param x NumericVector, expanatory variable.
//' @param y NumericVector, 0/1 values to fit.
//' @param w NumericVector, weights (required, positive).
//' @param initial_link, initial link estimates (required, all zeroes is a good start).
//' @param i integer, first index (inclusive).
//' @param j integer, last index (inclusive).
//' @param skip integer, index to skip (-1 to not skip).
//' @return vector of a and b.
//' 
//' @keywords internal
//' 
//' @examples
//' 
//' set.seed(5)
//' d <- data.frame(
//'   x =  rnorm(10),
//'   y = sample(c(0,1), 10, replace = TRUE)
//' )
//' weights <- runif(nrow(d))
//' m <- glm(y~x, data = d, family = binomial, weights = weights)
//' coef(m)
//' logistic_solve1(d$x, d$y, weights, rep(0.0, nrow(d)), 0, nrow(d)-1, -1)
//' 
//' @export
// [[Rcpp::export]]
NumericVector logistic_solve1(NumericVector x, NumericVector y, 
                              NumericVector w,
                              NumericVector initial_link,
                              const int i, const int j,
                              const int skip) {
  return logistic_solve1_worker(x, y, 
                                w,
                                initial_link,
                                i, j,
                                skip);
}






NumericVector xlogistic_fits_worker(const NumericVector &x, const NumericVector &y, 
                                    const NumericVector &w,
                                    const int i, const int j) {
  const int n = j-i+1;
  Rcpp::NumericVector final_links(n);
  for(int k=0; k<n; ++k) {
    final_links(k) = 0.0;
  }
  // look for corner cases
  if(n<=2) {
    // for out of sample- force links to zero unless we have more than 2 points
    return final_links;
  }
  // look for corner cases
  const input_summary isum = input_summary(x, y, w, i, j, -1);
  if(!isum.saw_data()) {
    return final_links;
  }
  if(!isum.y_varies()) {
    // y-pure constant
    if(isum.saw_y_pos) {
      for(int k=0; k<n; ++k) {
        final_links(k) = std::numeric_limits<double>::max();
      }
    } else {
      for(int k=0; k<n; ++k) {
        final_links(k) = -std::numeric_limits<double>::max();
      }
    }
    return final_links;
  }
  // we now know y varies
  if(!isum.x_varies()) {
    const double lk = logit(isum.total_wy/isum.total_w);
    for(int k=0; k<n; ++k) {
      final_links(k) = lk;
    }
    return final_links;
  }
  // we now know x varies
  if(isum.seperable()) {
    // check for seperable data cases, x able to perfectly sort y
    // NOTE: in this case this estimate is optimistic, as we guess the seperation point
    for(int k=0; k<n; ++k) {
      if(y(i+k)>=0.5) {
        final_links(k) = std::numeric_limits<double>::max();
      } else {
        final_links(k) = -std::numeric_limits<double>::max();
      }
    }
    return final_links;
  }
  // now on to general case
  const int nx = x.length();
  Rcpp::NumericVector initial_link(nx);
  for(int k = 0; k<nx; ++k) {
    initial_link(i) = 0.0;
  }
  // solve whole system to get a good start for hold-out systems.
  Rcpp::NumericVector coefs = logistic_solve1_worker(x, y, 
                                                     w,
                                                     initial_link,
                                                     i, j,
                                                     -1);
  for(int k=i; k<=j; ++k) {
    initial_link(k) = coefs(0) + coefs(1)*x(k);
  }
  // solve hold-out systems
  for(int k=i; k<=j; ++k) {
    Rcpp::NumericVector coefsi = logistic_solve1_worker(x, y, 
                                                        w,
                                                        initial_link,
                                                        i, j,
                                                        k);
    final_links(k-i) = coefsi(0) + coefsi(1)*x(k-i);
  }
  return final_links;
}


//' Out of sample logistic predictions (in link space).
//' 
//' 1-hold out logistic regression predections.
//' Zero indexed.
//' 
//' @param x NumericVector, expanatory variable.
//' @param y NumericVector, 0/1 values to fit.
//' @param w NumericVector, weights (required, positive).
//' @param i integer, first index (inclusive).
//' @param j integer, last index (inclusive).
//' @return vector of predictions for interval.
//' 
//' @keywords internal
//' 
//' @examples
//' 
//' set.seed(5)
//' d <- data.frame(x = rnorm(10))
//' d$y <- d$x + rnorm(nrow(d))>0
//' weights <- runif(nrow(d))
//' m <- glm(y~x, data = d, family = binomial, weights = weights)
//' d$pred1 <- predict(m, newdata = d, type = "link")
//' d$pred2 <- xlogistic_fits(d$x, d$y, weights, 0, nrow(d)-1)
//' d <- d[order(d$x), , drop = FALSE]
//' print(d)
//' 
//' @export
// [[Rcpp::export]]
NumericVector xlogistic_fits(NumericVector x, NumericVector y, 
                             NumericVector w,
                             const int i, const int j) {
  return xlogistic_fits_worker(x, y, 
                               w,
                               i, j);
}






NumericVector logistic_fits_worker(const NumericVector &x, const NumericVector &y, 
                                   const NumericVector &w,
                                   const int i, const int j) {
  const int n = j-i+1;
  // initialize return structure
  Rcpp::NumericVector final_links(n);
  for(int k=0; k<n; ++k) {
    final_links(k) = 0.0;
  }
  // look for corner cases
  if(n<=1) {
    if(n==1) {
      if(y(0)>0.5) {
        final_links(0) = std::numeric_limits<double>::max();
      } else {
        final_links(0) = -std::numeric_limits<double>::max();
      }
    }
    return final_links;
  }
  // look for more corner cases
  const input_summary isum = input_summary(x, y, w, i, j, -1);
  if(!isum.saw_data()) {
    return final_links;
  }
  if(!isum.y_varies()) {
    // y-pure constant
    if(isum.saw_y_pos>=0.5) {
      for(int k=0; k<n; ++k) {
        final_links(k) = std::numeric_limits<double>::max();
      }
    } else {
      for(int k=0; k<n; ++k) {
        final_links(k) = -std::numeric_limits<double>::max();
      }
    }
    return final_links;
  }
  // we now know y varies
  if(!isum.x_varies()) {
    const double lk = logit(isum.total_wy/isum.total_w);
    for(int k=0; k<n; ++k) {
      final_links(k) = lk;
    }
    return final_links;
  }
  // we now know x varies
  if(isum.seperable()) {
    // check for seperable data cases, x able to perfectly sort y
    for(int k=0; k<n; ++k) {
      if(y(i+k)>0.5) {
        final_links(k) = std::numeric_limits<double>::max();
      } else {
        final_links(k) = -std::numeric_limits<double>::max();
      }
    }
    return final_links;
  }
  // on to general case
  const int nx = x.length();
  Rcpp::NumericVector initial_link(nx);
  for(int k = 0; k<nx; ++k) {
    initial_link(i) = 0.0;
  }
  // solve whole system 
  Rcpp::NumericVector coefs = logistic_solve1_worker(x, y, 
                                                     w,
                                                     initial_link,
                                                     i, j,
                                                     -1);
  for(int k=i; k<=j; ++k) {
    final_links(k-i) = coefs(0) + coefs(1)*x(k-i);
  }
  return final_links;
}




//' In sample logistic predictions (in link space).
//' 
//' logistic regression predections.
//' Zero indexed.
//' 
//' @param x NumericVector, expanatory variable.
//' @param y NumericVector, 0/1 values to fit.
//' @param w NumericVector, weights (required, positive).
//' @param i integer, first index (inclusive).
//' @param j integer, last index (inclusive).
//' @return vector of predictions for interval.
//' 
//' @keywords internal
//' 
//' @examples
//' 
//' set.seed(5)
//' d <- data.frame(x = rnorm(10))
//' d$y <- d$x + rnorm(nrow(d))>0
//' weights <- runif(nrow(d))
//' m <- glm(y~x, data = d, family = binomial, weights = weights)
//' d$pred1 <- predict(m, newdata = d, type = "link")
//' d$pred2 <- logistic_fits(d$x, d$y, weights, 0, nrow(d)-1)
//' d <- d[order(d$x), , drop = FALSE]
//' print(d)
//' 
//' @export
// [[Rcpp::export]]
NumericVector logistic_fits(NumericVector x, NumericVector y, 
                            NumericVector w,
                            const int i, const int j) {
  return logistic_fits_worker(x, y, 
                              w,
                              i, j);
}

