householder_r = function(x, u){
  P = diag(1,nrow = length(u)) - 2*(matrix(u) %*% t(matrix(u))) 
  return(list(P=P, x_reflec = P %*% matrix(x)))
}
householder2_r = function(x){
  l2norm = function(x) sqrt(sum(x^2))
  u = x - l2norm(x)*c(1,rep(0,length(x)-1))
  u = u/l2norm(u)
  householder_r(x, u)
}
