#include <Rcpp.h>
#include "householder_cpp.h"
#include "basics.h"
using namespace Rcpp;

// [[Rcpp::export]]
List qr_cpp(NumericMatrix A){
  int n = A.ncol();
  int m = A.nrow();
  NumericMatrix q(m);
  q.fill_diag(1);
  
  for(int i = 0; i < n; i++){
    NumericMatrix q2(m);
    q2.fill_diag(1);
    q2(Range(i,m), Range(i,m));
    NumericMatrix hh = householder2_cpp(A.import(A.begin() + i*m + i, A.begin() + i*m + m));
    for(int j1 = i; j1 < m; j1 ++){
      for(int j2 = i; j2 < m; j2++){
        q2(j1,j2) = hh(j1 - i, j2 - i);
      }
    }
    //std::cout << "q2:" <<q2.nrow() << "x" << q2.ncol() << " A:" << A.ncol() << "x" << A.nrow() << " q:" << q.ncol() << "x" << q.nrow() << std::endl;
    A = matmul(q2,A);
    q = matmul(q2,q);
  }
  return List::create(Named("q") = transpose(q) , Named("r") = A);
}

