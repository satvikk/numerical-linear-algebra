#include <stdexcept>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double l2norm(NumericVector x){
  double sum = 0;
  for(int i = 0; i < x.size(); i++){
    sum += x[i]*x[i];
  }
  return sqrt(sum);
}

// [[Rcpp::export]]
NumericMatrix matmul(NumericMatrix a, NumericMatrix b){
  if(a.ncol() != b.nrow())
    throw std::invalid_argument( "Matrix dimension do not conform" );
  NumericMatrix out(a.nrow(), b.ncol());
  for(int i = 0; i < a.nrow(); i++){
    for(int j = 0; j < b.ncol(); j++){
      for(int k = 0; k < a.ncol(); k++){
        out(i,j) += a(i,k)*b(k,j);
      }
    }
  }
  return out;
}
