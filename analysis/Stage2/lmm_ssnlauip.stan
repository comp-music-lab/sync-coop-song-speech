data {
  int<lower=0> M;
  int<lower=0> N;
  int<lower=0> p;
  matrix[2*N, p] X;
  int idx_u_1[2*N];
  int idx_u_2[2*N];
  int p_0;
  int q[p_0];
  int qq[p_0*p_0];
  int n[M];
  int idx_st[M];
  vector[2*N] y;
  real beta;
}

parameters {
  real<lower=0> s_1;
  real<lower=0> s_2;
  vector[M] u_1;
  vector[N] u_2;
  real<lower=0> sgm;
  vector[p] be;
  real r;
}

transformed parameters {
  matrix[p, p] Lmd;
  Lmd = rep_matrix(0, p, p);

  matrix[2, 2] iQ_block;
  iQ_block = rep_matrix(0, 2, 2);
  iQ_block[1, 1] = sgm^2 + s_2^2;
  iQ_block[2, 2] = sgm^2 + s_2^2;
  iQ_block[1, 2] = -s_2^2;
  iQ_block[2, 1] = -s_2^2;
  iQ_block *= 1/(sgm^2 * (sgm^2 + 2*s_2^2));
  
  for (m in 1:M) {
    matrix[n[m], n[m]] iV;
    iV = rep_matrix(0, n[m], n[m]);
    for (i in 1:(n[m]/2)) {
      iV[(2*i-1):(2*i), (2*i-1):(2*i)] = iQ_block;
    }
    iV -= s_1^2/((sgm^2 + 2*s_2^2)*(sgm^2 + 2*s_2^2 + n[m]*s_1^2));
    
    Lmd += X[idx_st[m]:(idx_st[m] + n[m] - 1), ]'*iV*X[idx_st[m]:(idx_st[m] + n[m] - 1), ];
  }
  
  real g;
  g = 1/r - 1;
  Lmd /= (g*N);
  
  vector[2*N] mu;
  mu = be[1] + X[, 2:p]*be[2:p] + u_1[idx_u_1] + u_2[idx_u_2];
  
  vector[p_0] be_q;
  be_q = be[q];
  
  matrix[p_0, p_0] Sgm_q;
  Sgm_q = to_matrix(to_vector(inverse(Lmd))[qq], p_0, p_0);
}

model {
  vector[p] m = rep_vector(0, p);
  vector[p_0] m_q = m[q];
  
  r ~ beta(0.01, 0.01*N);
  s_1 ~ student_t(2, 0, 1000);
  s_2 ~ student_t(2, 0, 1000);
  u_1 ~ normal(0, s_1);
  u_2 ~ normal(0, s_2);
  sgm ~ student_t(2, 0, 1000);
  be ~ multi_normal_prec(m, Lmd);
  target += log((be_q - m_q)'*(be_q - m_q)/trace(Sgm_q)) + beta*normal_lpdf(y | mu, sgm);
}

generated quantities {
  vector[2*N] log_lik;
  vector[2*N] y_rep;
  for(i in 1:2*N) {
    log_lik[i] = beta*normal_lpdf(y[i] | mu[i], sgm);
    y_rep[i] = normal_rng(mu[i], sgm);
  }
}
