source("householder_r.R")
qr_r = function(A){
  q = diag(1,nrow = m, ncol = m)
  n = ncol(A)
  m = nrow(A)
  for(i in 1:(n)){
    q2 = diag(1, nrow = m)
    q2[i:m,i:m] = householder2_r(A[i:m,i])$P
    A = q2 %*% A
    q = q2 %*% q
  }
  A[lower.tri(A)] = 0
  return(list(q = t(q), r = A))
}

