h_Lmd <- function(X, sgm, s_1, s_2, M, n) {
  p = dim(X)[2]
  N = sum(n)
  Lmd = matrix(0, p, p)
  
  iQ_block = matrix(0, 2, 2)
  iQ_block[1, 1] = sgm^2 + s_2^2
  iQ_block[2, 2] = sgm^2 + s_2^2
  iQ_block[1, 2] = -s_2^2
  iQ_block[2, 1] = -s_2^2
  iQ_block = iQ_block/(sgm^2 * (sgm^2 + 2*s_2^2))
  
  offset_m = 1
  for (m in 1:M) {
    iV = matrix(0, n[m], n[m])
    for (i in 1:(n[m]/2)) {
      iV[(2*i-1):(2*i), (2*i-1):(2*i)] = iQ_block
    }
    iV = iV - s_1^2/((sgm^2 + 2*s_2^2)*(sgm^2 + 2*s_2^2 + n[m]*s_1^2))
    
    Lmd = Lmd + t(X[offset_m:(offset_m + n[m] - 1), ])%*%iV%*%X[offset_m:(offset_m + n[m] - 1), ]
    
    offset_m = offset_m + n[m]
  }
  
  Lmd = Lmd/N
  
  return(Lmd)
}