// main model (fml101) 
// with adjusted prior on sigma_lambda and tau~cauchy(0,1)

functions {
  // function to return the number of observations in a group
  int group_size(int[] ref, int value) {
    int count;
    count = 0;
    for (ii in 1:size(ref))
      if (ref[ii]==value)
        count = count + 1;
      return count;
  }
  
  // function to subset a vector (return just those observations in a given group)
  vector subset_vector(vector ck, int[] ref, int value) {
    int jj;
    vector[group_size(ref, value)] res;
    if (size(ref) != rows(ck))
      reject("illegal input: non-matching dimensions")
    jj = 1;
    for(ii in 1:size(ref)) {
      if (ref[ii] == value) {
        res[jj] = ck[ii];
        jj = jj+1;
      }
    }
    return res;
  }
  
  // function to subset an integer array (return just those observations in a given group)
  int[] subset_intarray(int[] ck, int[] ref, int value) {
    int jj;
    int res[group_size(ref, value)];
    if (size(ref) != size(ck))
      reject("illegal input: non-matching dimensions")
    jj = 1;
    for(ii in 1:size(ref)) {
      if (ref[ii] == value) {
        res[jj] = ck[ii];
        jj = jj+1;
      }
    }
    return res;
  }
  
  // recursive function to evaluate the denominator of the conditional likelihood
  real cl_denom(int N_g, int D_g, vector xb);
  real cl_denom(int N_g, int D_g, vector xb) {
    real res;
    if (N_g < D_g) {
      return 0;
    }
    if (D_g == 0) {
      return 1;
    }
    res = cl_denom(N_g-1, D_g, xb) + exp(log(cl_denom(N_g-1, D_g-1, xb)) + xb[N_g]);
    return res;
  }
}

data {
  int<lower=0> N;                                 // number of cases and controls together
  int<lower=1> nr;                                // number of groups
  int<lower=1, upper=nr> grp[N];                  // group (stratum) identifier
  
  int<lower=0> K;                                 // number of traffic-related predictors
  int<lower=0> L;                                 // number of non-traffic-related predictors
  int<lower=0> M;                                 // number of covariates
  
  int<lower=0, upper=1> ck[N];                    // available outcomes (0 or 1)
  
  matrix[N, K] x;                                 // traffic-related predictor values
  matrix[N, L] x2;                                // non-traffic-related predictor values
  matrix[N, M] x3;                                // covariate values
  }

transformed data {
  int n_group[nr]; // number of observations in the group
  int n_case[nr];  // number of cases/events in the group
  for (ii in 1:nr) {
    n_group[ii] = group_size(grp, ii);
    {
      int subset_ck[n_group[ii]];
      subset_ck = subset_intarray(ck, grp, ii);
      n_case[ii] = group_size(subset_ck, 1);
    }
  }
}

parameters {
  vector[K] beta_traffic;                         // slopes for traffic-related predictors
  vector[L] beta_nontraffic;                      // slopes for non-traffic-related predictors
  vector[M] beta_covariates;                      // slopes for covariates
  vector[K] mu;                                   // a vector of mean values for multi_normal
  real lambda;                                    // traffic effect
  real<lower=0> sigma_lambda;                     // variance of traffic effect
  corr_matrix[K] omega;                           // correlation matrix of predictors for multi_normal
  vector<lower=0>[K] tau;                         // scale value for correlation matrix of predictors for multi_normal
}

transformed parameters {
  vector[K] beta_traffic_exp;                     // exponentiated slopes for traffic-related predictors
  vector[L] beta_nontraffic_exp;                  // exponentiated slopes for non-traffic-related predictors
  vector[M] beta_covariates_exp;                  // exponentiated slopes for covariates
  real lambda_exp;                                // exponentiated traffic effect
  real phi;                                       // exponentiated sum of traffic-related predictors
  cov_matrix[K] sigma;                            // covariance matrix of traffic-related predictors
  
  beta_traffic_exp = exp(beta_traffic);
  beta_nontraffic_exp = exp(beta_nontraffic);
  beta_covariates_exp = exp(beta_covariates);
  lambda_exp = exp(lambda);
  phi = exp(sum(beta_traffic));
  sigma = quad_form_diag(omega, tau);
}

model {
  vector[N] xb; // observation level linear predictor
  real ll; // log likelihood
  int pos; // incrementing index
  
  beta_traffic ~ multi_normal(mu, sigma);        // prior on beta_traffic
  beta_nontraffic ~ normal(0,10);                // prior on beta_nontraffic
  beta_covariates ~ normal(0,10);                // prior on beta_covariates
  mu ~ normal(lambda, sigma_lambda);             // prior on mu
  lambda ~ normal(0,0.1);                        // prior on lambda
  sigma_lambda ~ cauchy(0,1);                    // prior on sigma_lambda 
  omega ~ lkj_corr(1);                           // lkj prior on the correlation matrix
  tau ~ cauchy(0,1);                             // prior scale value
  
  // log likelihood is a sum over each group
  xb = x * beta_traffic + x2 * beta_nontraffic + x3 * beta_covariates;
  pos = 1;
  for (ii in 1:nr) {
    int ck_g[n_group[ii]];
    vector[n_group[ii]] xb_g;
    ck_g = segment(ck, pos, n_group[ii]);
    xb_g = segment(xb, pos, n_group[ii]);
    ll = dot_product(to_vector(ck_g), xb_g) - log(cl_denom(n_group[ii], n_case[ii], xb_g));
    target += ll;
    pos = pos + n_group[ii];
  }
}
