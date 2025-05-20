h_uipmvn <- function(X, be, sgm, s_1, s_2, g, M, n){
  Lmd = h_Lmd(X, sgm, s_1, s_2, M, n)
  Lmd = Lmd/g
  
  d = length(be)
  m = rep(0, d)
  
  lnexpquad = -0.5*(be - m)%*%Lmd%*%(be - m)
  lndetLmd = 0.5*log(det(Lmd))
  
  lnp = -d/2*log(2*pi) + lndetLmd + lnexpquad
  
  return(lnp)
}