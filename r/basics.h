#include <Rcpp.h>
#ifndef basics_h
#define basics_h

double l2norm(Rcpp::NumericVector);
Rcpp::NumericMatrix matmul(Rcpp::NumericMatrix, Rcpp::NumericMatrix);
#endif