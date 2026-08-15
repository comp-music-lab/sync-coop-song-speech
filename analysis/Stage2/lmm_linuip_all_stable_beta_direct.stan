data {
  int<lower=0> M;
  int<lower=0> N;
  int<lower=0> p;
  matrix[2*N, p] X;
  int n[M];
  int idx_st[M];
  int pattern;
  vector[2*N] y;
  real beta;
}

transformed data {
  // Pairwise sum/difference design summaries.
  // For participant i:
  //   x_sum  = x_(2i-1) + x_(2i)
  //   x_diff = x_(2i-1) - x_(2i)
  // These give a numerically stable representation of the inverse
  // covariance and the powered marginal likelihood.
  matrix[p, p] XtX_diff;
  matrix[p, p] XtX_sum;
  vector[p] Xty_diff;
  vector[p] Xty_sum;
  real yty_diff;
  real yty_sum;

  matrix[N, p] X_sum;
  matrix[N, p] X_diff;
  vector[N] y_sum;
  vector[N] y_diff;

  array[M] vector[p] cohort_X_sum;
  vector[M] cohort_y_sum;

  XtX_diff = rep_matrix(0, p, p);
  XtX_sum = rep_matrix(0, p, p);
  Xty_diff = rep_vector(0, p);
  Xty_sum = rep_vector(0, p);
  yty_diff = 0;
  yty_sum = 0;

  X_sum = rep_matrix(0, N, p);
  X_diff = rep_matrix(0, N, p);
  y_sum = rep_vector(0, N);
  y_diff = rep_vector(0, N);

  for (i in 1:N) {
    X_sum[i, ] = X[2*i - 1, ] + X[2*i, ];
    X_diff[i, ] = X[2*i - 1, ] - X[2*i, ];
    y_sum[i] = y[2*i - 1] + y[2*i];
    y_diff[i] = y[2*i - 1] - y[2*i];
  }

  XtX_diff = X_diff' * X_diff;
  XtX_sum = X_sum' * X_sum;
  Xty_diff = X_diff' * y_diff;
  Xty_sum = X_sum' * y_sum;
  yty_diff = dot_self(y_diff);
  yty_sum = dot_self(y_sum);

  for (m in 1:M) {
    int start_m = idx_st[m];
    int end_m = idx_st[m] + n[m] - 1;
    cohort_X_sum[m] = rep_vector(0, p);
    cohort_y_sum[m] = 0;

    for (j in start_m:end_m) {
      cohort_X_sum[m] += to_vector(X[j, ]);
      cohort_y_sum[m] += y[j];
    }
  }
}

parameters {
  real<lower=0> s_1;
  real<lower=0> s_2;
  real<lower=0> sgm;
  vector[p] be;
  real eta_r;
}

transformed parameters {
  real<lower=0, upper=1> r;
  real<lower=0> g;
  matrix[p, p] Lmd;

  r = inv_logit(eta_r);
  g = 1 / r - 1;

  // Stable form of X' V^{-1} X for
  //   V = sgm^2 I + s_2^2 K + s_1^2 J.
  // Within each participant pair, the difference direction has
  // eigenvalue sgm^2 and the sum direction has eigenvalue
  // sgm^2 + 2*s_2^2. The cohort random intercept modifies only
  // the cohort-wide sum direction.
  {
    real sgm2 = square(sgm);
    real s2sq = square(s_2);
    real a = sgm2 + 2 * s2sq;

    Lmd = 0.5 * XtX_diff / sgm2
        + 0.5 * XtX_sum / a;

    for (m in 1:M) {
      real c_m = s_1^2 /
        (a * (a + n[m] * square(s_1)));
      Lmd -= c_m * (cohort_X_sum[m] * cohort_X_sum[m]');
    }

    Lmd /= (g * N);
  }
}

model {
  vector[p] zero = rep_vector(0, p);

  // Original prior r ~ beta(0.01, 0.01*N), transformed to
  // eta_r = logit(r), including the Jacobian.
  target += beta_lpdf(r | 0.01, 0.01 * N)
            + log(r) + log1m(r);

  s_1 ~ student_t(2, 0, 1000);
  s_2 ~ student_t(2, 0, 1000);
  sgm ~ student_t(2, 0, 1000);

  // Same g-prior as the original model.
  be ~ multi_normal_prec(zero, Lmd);

  // ------------------------------------------------------------------
  // Stable analytically marginalized powered likelihood
  // ------------------------------------------------------------------
  //
  // We integrate u_1 and u_2 directly from
  //
  //   [N(y | X*be + Z*u, sgm^2 I)]^beta p(u)
  //
  // rather than introducing q = sgm^2 / beta. This formulation remains
  // well behaved as beta -> 0 and has exactly zero likelihood contribution
  // at beta = 0.
  //
  // Let
  //   a_m = sgm^2 + 2*beta*s_2^2
  //   b_m = a_m + beta*n_m*s_1^2.
  //
  // The pair-difference eigenvalue is sgm^2, while the pair-sum eigenvalue
  // is a_m, and the final cohort-wide direction has eigenvalue b_m.
  //
  // The determinant ratios are evaluated with log1p() for stability when
  // beta is very small.
  {
    real sgm2 = square(sgm);
    real s2sq = square(s_2);
    real s1sq = square(s_1);
    real a = sgm2 + 2 * beta * s2sq;

    // Residual pair differences and sums.
    real e_diff2 = yty_diff
      - 2 * dot_product(Xty_diff, be)
      + dot_product(be, XtX_diff * be);

    real e_sum2 = yty_sum
      - 2 * dot_product(Xty_sum, be)
      + dot_product(be, XtX_sum * be);

    real quad = 0.5 * beta / sgm2 * e_diff2
      + 0.5 * beta / a * e_sum2;
    real log_det_ratio = 0;

    for (m in 1:M) {
      real a_m = a;
      real b_m = a_m + beta * n[m] * s1sq;
      real cohort_e = cohort_y_sum[m]
        - dot_product(cohort_X_sum[m], be);

      // Pair-sum contribution is applied globally below; this loop
      // contains only the cohort-level rank-one correction.
      quad -= square(beta) * s1sq / (a_m * b_m) * square(cohort_e);

      // log |I + beta/sigma^2 * Z D Z'|
      log_det_ratio +=
          (0.5 * n[m] - 1) *
            log1p(2 * beta * s2sq / sgm2)
        + log1p(beta * (2 * s2sq + n[m] * s1sq) / sgm2);
    }

    // Powered Gaussian normalizing constant plus the exact marginalization
    // correction. At beta = 0, all three terms are exactly zero.
    target +=
        -0.5 * beta * (2 * N) * log(2 * pi() * sgm2)
        -0.5 * log_det_ratio
        -0.5 * quad;
  }
}
