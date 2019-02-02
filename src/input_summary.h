
#ifndef INPUT_SUMMARY_H
#define INPUT_SUMMARY_H


using Rcpp::NumericVector;


class input_summary {
public:
  double max_x;
  double min_x;
  bool saw_y_pos;
  double max_x_pos;
  double min_x_pos;
  bool saw_y_neg;
  double max_x_neg;
  double min_x_neg;
  double total_w;
  double total_wy;
  long k_points;
  
  input_summary(NumericVector x, NumericVector y, 
                NumericVector w,
                const int i, const int j,
                const int skip);
  
  bool saw_data() const;
  
  bool x_varies() const;
  
  bool y_varies() const;
  
  bool seperable() const;
};

#endif