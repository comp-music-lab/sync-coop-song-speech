h_standata <- function(datalist, pattern) {
  y = datalist$y
  X = unname(datalist$X)
  p = dim(X)[2]
  M = datalist$M
  N = datalist$N
  n = datalist$n
  idx_u_1 = sapply(1:dim(datalist$Z)[1], function(i){which(datalist$Z[i, 1:M] == 1)})
  idx_u_2 = sapply(1:dim(datalist$Z)[1], function(i){which(datalist$Z[i, (M + 1):dim(datalist$Z)[2]] == 1)})
  idx_st = c(1, which(diff(idx_u_1) != 0) + 1)
  
  standata <- list(
    y=y, X=X, p=p,
    M=M, N=N, n=n,
    idx_u_1=idx_u_1, idx_u_2=idx_u_2,
    pattern=pattern, idx_st=idx_st,
    beta=1
  )
  
  return(standata)
}