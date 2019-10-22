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

double l2norm(NumericVector x){
  double sum = 0;
  for(int i = 0; i < x.size(); i++){
    sum += x[i]*x[i];
  }
  return sqrt(sum);
}

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

// [[Rcpp::export]]
NumericVector qr_cpp(NumericMatrix A){
  int m = A.nrow();
  int n = A.ncol();
  NumericMatrix q = NumericMatrix::diag(m, 1);
  for(int i=0; i<n; i++){
    NumericMatrix q2 = NumericMatrix::diag(m, 1);
    NumericMatrix hh2 = householder2_cpp( A(Range(1,2)) );
    for(int j=i; j<m; j++){
      
    }
  }
  return 0;
}
  