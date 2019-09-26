library(Rcpp)
library(magrittr)
library(microbenchmark)
set.seed(84)
mat = matrix(rnorm(600^2), ncol = 600, nrow = 600)
mat = mat %*% t(mat)
#base::chol pivot=F----
l_cho = chol(mat)
norm((t(l_cho) %*% l_cho) - mat, "F")

# cholesky_r ----
source("cholesky_r.R")
l_cho = cholesky_r(mat)
norm((l_cho %*% t(l_cho)) - mat, "F")

#cholesky_cpp ----
sourceCpp("cholesky_cpp.cpp")
l_cho = cholesky_cpp(mat)
norm((l_cho %*% t(l_cho)) - mat, "F")

#microbenchmarking ----
set.seed(42)
mbc = list()
nns = c(2:8)*100
ns = lapply(nns, function(z) matrix(rnorm(z^2), nrow = z) %>% (function(zz) zz %*% t(zz)))
for(i in 1:length(ns)){
  print(i)
  mat3 = ns[[i]]
  mbc[[i]] = microbenchmark(chol(mat3), cholesky_r(mat3), cholesky_cpp(mat3)) %>% summary
}
mbc_df = sapply(mbc, `[[`, "median") %>% t %>% cbind(nns,.) %>% as.data.frame %>% `names<-`(c("n", "base", "r", "cpp"))
mbc_df2 = cbind(mbc_df, sapply(mbc_df[,-1], function(z) z/(mbc_df$n^2)))
names(mbc_df2) = c("n",'base','r','cpp','base_adj','r_adj','cpp_adj')
