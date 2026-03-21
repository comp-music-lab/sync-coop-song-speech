h_standata <- function(datalist, H0, q=NULL) {
  y = datalist$y
  if(H0 == 0) {
    X = unname(cbind(datalist$X, datalist$N_X))
  } else {
    X = unname(datalist$X)
  }
  p = dim(X)[2]
  M = datalist$M
  N = datalist$N
  n = datalist$n
  idx_u_1 = sapply(1:dim(datalist$Z)[1], function(i){which(datalist$Z[i, 1:M] == 1)})
  idx_u_2 = sapply(1:dim(datalist$Z)[1], function(i){which(datalist$Z[i, (M + 1):dim(datalist$Z)[2]] == 1)})
  idx_st = c(1, which(diff(idx_u_1) != 0) + 1)
  
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