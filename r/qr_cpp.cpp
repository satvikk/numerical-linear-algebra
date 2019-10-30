#include <Rcpp.h>
#include "householder_cpp.h"
#include "basics.h"
using namespace Rcpp;

// [[Rcpp::export]]
List qr_cpp(NumericMatrix input){
  NumericMatrix A = clone(input);
  int n = A.ncol();
  int m = A.nrow();
  NumericMatrix q(m);
  q.fill_diag(1);
  List tuqs;
  List tuas;
  
  for(int i = 0; i < n; i++){
    NumericVector u = householder3_cpp(A.import(A.begin() + m*i + i, A.begin() + m*i + m));
    NumericVector tua(n - i);
    NumericVector tuq(n);
    
    for(int j = 0; j < i; j++){
      for(int k = i; k < m; k++){
        tuq[j] = tuq[j] + u[k - i]*q(k,j);
      }
    }
    for(int j = i; j < n; j++){
      for(int k = i; k < m; k++){
        tuq[j] = tuq[j] + u[k - i]*q(k,j);
        tua[j - i] = tua[j - i] + u[k - i]*A(k,j);
      }
    }
    
    
    for(int j = 0; j < i; j++){
      for(int k = i; k < m; k++){
        q(k,j) = q(k,j) - 2*u[k - i]*tuq[j];
      }
    }
    for(int j = i; j < n; j++){
      for(int k = i; k < m; k++){
        q(k,j) = q(k,j) - 2*u[k - i]*tuq[j ];
        A(k,j) = A(k,j) - 2*u[k - i]*tua[j - i];
      }
    }
    tuqs.push_back(tuq);
    tuas.push_back(tua);
  }
  return List::create(Named("q") = transpose(q), Named("r") = A, Named("tuas") = tuas, Named("tuqs") = tuqs);
}

