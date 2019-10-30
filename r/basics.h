#include <Rcpp.h>
#ifndef BASICS_H
#define BASICS_H

double l2norm(Rcpp::NumericVector);
Rcpp::NumericMatrix matmul(Rcpp::NumericMatrix, Rcpp::NumericMatrix);
#endif