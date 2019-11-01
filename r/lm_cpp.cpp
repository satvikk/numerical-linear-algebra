#include <Rcpp.h>
#include "basics.h"
#include "qr_cpp.h"
#include "back_sub_cpp.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix lm_cpp(NumericMatrix x, NumericVector y){
  int m = x.nrow();
  int n = x.ncol();
  if(m != y.length())
    throw(Rcpp::exception("x and y are of unequal lengths"));
  NumericVector B(n);
  List qrx = qr_cpp(x);
  NumericMatrix q = qrx[0];
  NumericMatrix r = qrx[1];
  q = transpose(q);
  y.attr("dim") = Dimension(m, 1);
  NumericMatrix rhs = as<NumericMatrix>(y);
  rhs = matmul(q,rhs);
  return back_sub_cpp(r,rhs);
}