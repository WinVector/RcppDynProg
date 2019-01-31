
#include <RcppArmadillo.h>
#include <math.h>

// [[Rcpp::depends(RcppArmadillo)]]

using Rcpp::NumericVector;


double logit(const double x) {
  return log(x/(1.0-x));
}

double sigmoid(const double x) {
  return 1/(1+exp(-x));
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
  // init return structure
  NumericVector coef = NumericVector(2);
  coef(0) = 0;
  coef(1) = 0;
  // no-data cases
  if((j<i) || ((j==i)&&(skip==j))) {
    return coef;
  }
  // corner cases
  double max_x = std::numeric_limits<double>::min();
  double min_x = std::numeric_limits<double>::max();
  double max_y = std::numeric_limits<double>::min();
  double min_y = std::numeric_limits<double>::max();
  double total_wy = 0.0;
  double total_w = 0.0;
  for(int k=i; k<=j; ++k) {
    if(k!=skip) {
      max_x = std::max(max_x, x(k));
      min_x = std::min(min_x, x(k));
      max_y = std::max(max_y, y(k));
      min_y = std::min(min_y, y(k));
      total_w = total_w + w(k);
      total_wy = total_wy + w(k)*y(k);
    }
  }
  if(min_y>=max_y) {
    // y-pure
    if(min_y>0.5) {
      coef(1) = std::numeric_limits<double>::max();
    } else {
      coef(1) = std::numeric_limits<double>::min();
    }
    return(coef);
  }
  if(min_x>=max_x) {
    // x not varying
    coef(0) = logit(total_wy/total_w);
    return(coef);
  }
  // TODO: check for seperable data
  arma::colvec link(j-i+1, arma::fill::zeros);
  arma::colvec preds(j-i+1, arma::fill::zeros);
  arma::colvec z(j-i+1, arma::fill::zeros);
  arma::colvec wadj(j-i+1, arma::fill::ones);
  for(int k=i; k<=j; ++k) {
    link(k-i) = initial_link(k);
  }
  // Iteratively re-weighted least squares solution
  double c0 = 0.0;
  double c1 = 0.0;
  double diff = 1.0;
  for(int rep = 0; (rep<20) && (diff>1.e-6); ++rep) {
    // build composite weights and z-target
    for(int k=i; k<=j; ++k) {
      preds(k-i) = sigmoid(link(k-i));
      z(k-i) = link(k-i) + (y(k) - preds(k-i))/(preds(k-i)*(1.0-preds(k-i)));
      wadj(k-i) = preds(k-i)*(1.0-preds(k-i))*w(k);
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
      if(det>0) {
        // solve linear system and form estimate
        c0 = (xx_1_1*sy_0 - xx_0_1*xy_1)/det;
        c1 = (-xx_1_0*sy_0 + xx_0_0*xy_1)/det;
      } else {
        c0 = sy_0/sum_w;
      }
    }
    // build next link
    diff = 0.0;
    for(int k=i; k<=j; ++k) {
      const double nvi = c0 + c1*x(k);
      const double diffi = nvi - link(k-i);
      diff = diff + diffi*diffi;
      link(k-i) = nvi;
    }
  }
  coef(0) = c0;
  coef(1) =  c1;
  return coef;
}
