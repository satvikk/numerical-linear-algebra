cholesky_r <- function(A){
  if(class(A) != "matrix")
    stop(paste0("found ",class(A), ", expected matrix"))
  if((ncol(A) != nrow(A)) | nrow(A) <= 3)
    stop("not a square matrix of n>3")
  
  n = ncol(A)
  L = matrix(as.numeric(NA), nrow = n, ncol = n)
  L[1,1] = sqrt(A[1,1])
  L[-1,1] = A[-1,1]/L[1,1]
  for(i in 2:(ncol(A)-1)){
    L[i,i] = sqrt(A[i,i] - sum(L[i,-(i:n)]^2) )
    L[-(1:i),i] = c(A[-(1:i),i,drop=F] - L[-(1:i),1:(i-1),drop=F] %*% t(L[i,1:(i-1),drop=F]))/L[i,i]
  }
  L[n,n] = sqrt(A[n,n] - sum(L[n,-n]^2) )
  L[is.na(L)] = 0
  return(L)
}

set.seed(84)
mat = matrix(rnorm(100), ncol = 10, nrow = 10)
mat = mat %*% t(mat)
l_cho = cholesky_r(mat)

norm((l_cho %*% t(l_cho)) - mat, "F")
