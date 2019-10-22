#include <Rcpp.h>
#ifndef householder_h
#define householder_h

double l2norm(Rcpp::NumericVector);
Rcpp::NumericMatrix householder_cpp(Rcpp::NumericVector, Rcpp::NumericVector );
Rcpp::NumericMatrix householder2_cpp(Rcpp::NumericVector);

#endif