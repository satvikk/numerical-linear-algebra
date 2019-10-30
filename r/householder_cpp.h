#include <Rcpp.h>
#ifndef HOUSEHOLDER_H
#define HOUSEHOLDER_H

double l2norm(Rcpp::NumericVector);
Rcpp::NumericMatrix householder_cpp(Rcpp::NumericVector, Rcpp::NumericVector );
Rcpp::NumericMatrix householder2_cpp(Rcpp::NumericVector);
Rcpp::NumericVector householder3_cpp(Rcpp::NumericVector);
  
#endif