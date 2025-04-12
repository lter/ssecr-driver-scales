
// Simple Community-Level Model

data {
  int<lower=0> N; // number of datapoints
  int<lower=0> Si; // number of sites
  vector[N] com; // community metric (diversity or total CPUE)
  int site[N]; // site
  vector[N] DO; // DO
  vector[N] temp; // temp
}

parameters {
  real beta0_mu; // mean intercept value
  real delta_beta0_si[Si]; // site level deviations from mean intercept
  real<lower=0> sigma_beta0_si; // scale of site level deviations from mean intercept
  
  real beta_temp; // effect of temperature on community metric
  real beta_DO; // effect of DO on community metric
  
  real<lower=0> sigma; // residual variation
}

transformed parameters{
  real mu_hat[N]; // linear predictor for each datapoint
  for(i in 1:N){
    mu_hat[i] = (beta0_mu + delta_beta0_si[site[i]] * sigma_beta0_si) +
    beta_DO * DO[i] + beta_temp * temp[i];
  }
}
model {
  // priors
  beta0_mu ~ std_normal(); 
  delta_beta0_si[Si] ~ std_normal(); 
  sigma_beta0_si ~ exponential(1); 
  
  beta_temp ~ std_normal(); 
  beta_DO ~ std_normal(); 
  
  sigma ~ exponential(1);
  
  // likelihood
  for(i in 1:N){
    com[i] ~ normal(mu_hat[i], sigma);
  }
}

generated quantities{
  real log_lik[N]; // pointwise log likelihoods

  // calculate the log likelihood of each data point
  for(i in 1:N){
    log_lik[i] = normal_lpdf(com[i] | mu_hat[i], sigma);
  }
}