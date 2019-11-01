#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix back_sub_cpp(NumericMatrix R, NumericMatrix x){
  int m = R.nrow();
  int n = x.ncol();
    throw(Rcpp::exception("R should be square"));
  if(m != x.nrow())
    throw(Rcpp::exception("Non-conformable matrix dimensions"));
  NumericMatrix out(m,n);
  for(int i = m - 1; i >= 0; i--){
    for(int j = 0; j < n; j++){
      double sp = 0;
      for(int k = m-1; k > i; k--){
        sp = sp + R(i,k)*out(k,j);
      }
      out(i,j) = (x(i,j) - sp)/R(i,i);
    }
  }
  return out;
}