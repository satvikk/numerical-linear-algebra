#include <Rcpp.h>
#include "basics.h"
#include "qr_cpp.h"
#include "back_sub_cpp.h"

using namespace Rcpp;

// [[Rcpp::export]]
List lm_cpp(NumericMatrix x, NumericVector y){
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
  NumericMatrix beta = back_sub_cpp(r,rhs);
  NumericMatrix preds = matmul(x, beta);
  double sig2 = 0;
  for(int i = 0; i < m; i++){
    sig2 += (preds[i] - y[i])*(preds[i] - y[i]);
  }
  sig2 = sig2/(m - n);
  NumericMatrix sig2m(n,n);
  sig2m.fill_diag(sig2);
  NumericMatrix rt = transpose(r);
  NumericMatrix zz = forw_sub_cpp(rt, sig2m);
  NumericMatrix var_beta = back_sub_cpp(r, zz);
  return List::create(Named("beta") = beta, Named("var_beta") = var_beta );
}