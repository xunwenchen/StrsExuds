#include <Rcpp.h>
using namespace Rcpp;

// Function to calculate Pearson correlation
double pearson_correlation(NumericVector x, NumericVector y) {
  int n = x.size();
  double mean_x = mean(x);
  double mean_y = mean(y);
  double sum_xy = 0, sum_x2 = 0, sum_y2 = 0;
  
  for (int i = 0; i < n; i++) {
    sum_xy += (x[i] - mean_x) * (y[i] - mean_y);
    sum_x2 += pow(x[i] - mean_x, 2);
    sum_y2 += pow(y[i] - mean_y, 2);
  }
  
  return sum_xy / sqrt(sum_x2 * sum_y2);
}

// [[Rcpp::export]]
NumericMatrix pairwise_correlation(NumericMatrix x) {
  int n = x.nrow();
  NumericMatrix cor_matrix(n, n);
  
  for (int i = 0; i < n; i++) {
    for (int j = i; j < n; j++) {
      NumericVector xi = x(i, _);
      NumericVector xj = x(j, _);
      double cor = pearson_correlation(xi, xj);
      cor_matrix(i, j) = cor;
      cor_matrix(j, i) = cor;
    }
  }
  
  return cor_matrix;
}
