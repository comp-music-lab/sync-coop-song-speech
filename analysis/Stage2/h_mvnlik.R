h_mvnlik <- function(y, X, be, sgm, s_1, s_2, M, n) {
  z = y - X%*%be
  lnp_1 = 0
  lnp_2 = 0
  d = length(y)
  
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
    
    lnp_1 = lnp_1 - 0.5*t(z[offset_m:(offset_m + n[m] - 1), ])%*%iV%*%z[offset_m:(offset_m + n[m] - 1), ]
    lnp_2 = lnp_2 + 0.5*log(det(iV))
    
    offset_m = offset_m + n[m]
  }
  
  lnp = -d/2*log(2*pi) + lnp_1 + lnp_2
  return(lnp)
}

'
d = length(y)
library(Matrix)
N = sum(n)/2
z = y - X%*%be
iV = solve(Z%*%bdiag(s_1^2*diag(M), s_2^2*diag(N))%*%t(Z) + sgm^2*diag(d))
lnp = -d/2*log(2*pi) + 0.5*determinant(iV, logarithm=TRUE)$modulus[1] - 0.5*t(z)%*%iV%*%(z)
'