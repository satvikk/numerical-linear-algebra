library(magrittr)
library(microbenchmark)
set.seed(84)
mat = matrix(rnorm(10000), ncol = 100, nrow = 100)
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
mat2 = matrix(rnorm(500^2), ncol = 500, nrow = 500)
mat2 = mat2 %*% t(mat2)
mbc = list()
ns = c(50,100,200,300)
for(i in 1:length(ns)){
  n = ns[i]
  mat3 = mat2[1:n,1:n]
  mbc[[i]] = microbenchmark(chol(mat3), cholesky_r(mat3), cholesky_cpp(mat3)) %>% summary
}
mbc_df = sapply(mbc, `[[`, "median") %>% t %>% cbind(ns,.) %>% as.data.frame %>% `names<-`(c("n", "base", "r", "cpp"))
mbc_df2 = cbind(mbc_df, sapply(mbc_df[,-1], function(z) z/(mbc_df$n^2)) %>% `names<-`(paste0(names(mbc_df),"_adj")))
