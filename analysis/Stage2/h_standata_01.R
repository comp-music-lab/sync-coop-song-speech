h_standata <- function(datalist, H0, q=NULL) {
  y = datalist$y
  if(H0 == 1) {
    X = unname(datalist$X[, -2])
  } else {
    X = unname(datalist$X)
  }
  p = dim(X)[2]
  M = datalist$M
  N = datalist$N
  n = datalist$n
  idx_u_1 = unlist(mapply(function(n_i, i) {rep(i, n_i)}, n, 1:M))
  idx_u_2 = c(sapply(1:N, function(i){c(i, i)}))
  idx_st = c(1, which(diff(idx_u_1) == 1) + 1)
  
  if(!is.null(q)) {
    q = array(q, dim=1)
    p_0 = length(q)
    qq = array(sapply(q, function(x) {(x - 1)*p + q}), dim=p_0*p_0)
  } else {
    p_0 = NULL
    qq = NULL
  }
  
  standata <- list(
    y=y, X=X, p=p,
    M=M, N=N, n=n,
    idx_u_1=idx_u_1, idx_u_2=idx_u_2,
    q=q, qq=qq, p_0=p_0, idx_st=idx_st,
    beta=1
  )
  
  return(standata)
}