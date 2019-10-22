#include <Rcpp.h>
#include "basics.h"
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix householder_cpp(NumericVector x, NumericVector u){
  NumericMatrix P = NumericMatrix::diag(x.size(), 1);
  for(int i = 0; i < x.size(); i++){
    for(int j = 0; j < x.size(); j++){
      P(i,j) = P(i,j) - 2*u[i]*u[j];
    }
  }
  return P;
}

// [[Rcpp::export]]
NumericMatrix householder2_cpp(NumericVector x){
  NumericVector u = x;
  u[0] = u[0] - l2norm(x); 
  return householder_cpp(x, u/l2norm(u));
}
