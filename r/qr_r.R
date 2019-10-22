source("householder_r.R")
qr_r = function(A){
  n = ncol(A)
  m = nrow(A)
  q = diag(1,nrow = m, ncol = m)
  for(i in 1:n){
    u = householder3_r(A[i:m,i])
    A[i:m,i:m] = A[i:m,i:m] - 2 * matrix(u) %*% (t(matrix(u)) %*% A[i:m,i:m])
    q[i:m,] = q[i:m,] - 2 * matrix(u) %*% (t(matrix(u)) %*% q[i:m,])
  }
  A[lower.tri(A)] = 0
  return(list(q = t(q), r = A))
}