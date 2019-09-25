#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cholesky_cpp(NumericMatrix A) {
  int n = A.nrow();
  NumericMatrix L(n,n);
  for(int i = 0; i < n; i++){
    for(int j = 0; j < n; j++){
      if(i == j){
        double itered = 0;
        for(int k =0; k < i; k++){
          itered += L(i,k) * L(i,k);
        }
        L(i,j) = sqrt(A(i,j) - itered);
      }
      if(i > j){
        double itered =0;
        for(int k = 0; k < j; k++){
          itered += L(i,k)*L(j,k);
        }
        L(i,j) = (A(i,j) - itered)/L(j,j);
      }
    }
  }
  return L;
}
