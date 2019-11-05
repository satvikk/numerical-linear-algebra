#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix back_sub_cpp(NumericMatrix R, NumericMatrix x){
  int m = R.nrow();
  int n = R.ncol();
  int o = x.ncol();
  NumericMatrix out(n,o);
  for(int i = n - 1; i >= 0; i--){
    for(int j = 0; j < o; j++){
      double sp = 0;
      for(int k = n-1; k > i; k--){
        sp = sp + R(i,k)*out(k,j);
      }
      out(i,j) = (x(i,j) - sp)/R(i,i);
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix forw_sub_cpp(NumericMatrix L, NumericMatrix x){
  int m = L.nrow();
  int n = L.ncol();
  int o = x.ncol();
  NumericMatrix out(n,o);
  for(int i = 0; i < n; i++){
    for(int j = 0; j < o; j++){
      double sp = 0;
      for(int k = 0; k < i; k++){
        sp = sp + L(i,k)*out(k,j);
      }
      out(i,j) = (x(i,j) - sp)/L(i,i);
    }
  }
  return out;
}