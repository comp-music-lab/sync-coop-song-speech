h_standata <- function(datalist, pattern) {
  y = datalist$y
  X = unname(datalist$X)
  p = dim(X)[2]
  M = datalist$M
  N = datalist$N
  n = datalist$n
  idx_u_1 = unlist(mapply(function(n_i, i) {rep(i, n_i)}, n, 1:M))
  idx_u_2 = c(sapply(1:N, function(i){c(i, i)}))
  idx_st = c(1, which(diff(idx_u_1) == 1) + 1)
  
  standata <- list(
    y=y, X=X, p=p,
    M=M, N=N, n=n,
    idx_u_1=idx_u_1, idx_u_2=idx_u_2,
    pattern=pattern, idx_st=idx_st,
    beta=1
  )
  
  return(standata)
}