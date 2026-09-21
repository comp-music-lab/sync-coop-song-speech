h_nlmvn <- function(X, be, sgm, s_1, s_2, g, M, n, q, qq){
  Lmd = h_Lmd(X, sgm, s_1, s_2, M, n)
  Lmd = Lmd/g;
  
  Sgm <- try(solve(Lmd), silent=TRUE)
  
  if(class(Sgm)[1] == "try-error") {
    lnp <- NaN
  } else {
    d = length(be)
    m = rep(0, d)
    be_q = be[q]
    m_q = m[q]
    p_0 = length(q)
    
    Sgm_q = matrix(c(Sgm)[qq], nrow=p_0, ncol=p_0)
    
    lnexpquad = -0.5*(be - m)%*%Lmd%*%(be - m)
    lndetLmd = 0.5*log(det(Lmd))
    
    lnp = log(t(be_q - m_q)%*%(be_q - m_q)) - log(sum(diag(Sgm_q))) + -d/2*log(2*pi) + lndetLmd + lnexpquad
  }
  
  return(lnp)
}