
// Simple Population/Individual-Level Model

data {
  int<lower=0> N; // number of datapoints
  int<lower=0> Si; // number of sites
  int<lower=0> Sp; // number of species
  vector[N] y; // community metric (diversity or total CPUE)
  int site[N]; // site
  int species[N]; // species
  vector[N] DO; // DO
  vector[N] temp; // temp
}

parameters {
  real beta0_sp[Sp]; // species contribution to intercept
  real beta0_si[Si]; // site contribution to intercept
  real beta0_mu; // mean intercept
  real<lower=0> sigma_beta0_sp; // scale of species level deviations from mean intercept
  real<lower=0> sigma_beta0_si; // scale of site level deviations from mean intercept
  
  real beta_temp[Sp]; // hierarchical effect of temperature
  real beta_temp_mu; // mean temperature effect
  real<lower=0> sigma_beta_temp; // scale of species level deviations from mean temperature effect
  
  real beta_DO[Sp]; // hierarchical effect of DO
  real beta_DO_mu; // mean DO effect
  real<lower=0> sigma_beta_DO; // scale of species level deviations from mean DO effect
  
  real<lower=0> sigma; // residual variation
}

transformed parameters{
  real mu_hat[N]; // linear predictor for each datapoint
  for(i in 1:N){
    mu_hat[i] = beta0_sp[species[i]] +
    beta0_si[site[i]] + 
    beta_DO[species[i]] * DO[i] + 
    beta_temp[species[i]] * temp[i];
  }
}

model {
  // priors
  beta0_sp ~ normal(beta0_mu, sigma_beta0_sp); 
  beta0_si ~ normal(beta0_mu, sigma_beta0_si); 
  beta0_mu ~ std_normal(); 
  sigma_beta0_sp ~ exponential(1); 
  sigma_beta0_si ~ exponential(1); 
  
  beta_temp ~ normal(beta_temp_mu, sigma_beta_temp); 
  beta_temp_mu ~ std_normal(); 
  sigma_beta_temp ~ exponential(1); 
  
  beta_DO ~ normal(beta_temp_mu, sigma_beta_temp); 
  beta_DO_mu ~ std_normal(); 
  sigma_beta_DO ~ exponential(1); 
  sigma ~ exponential(1);
  
  // likelihood
  for(i in 1:N){
    y[i] ~ normal(mu_hat[i], sigma);
  }
}

generated quantities{
  real log_lik[N]; // pointwise log likelihoods

  // calculate the log likelihood of each data point
  for(i in 1:N){
    log_lik[i] = normal_lpdf(y[i] | mu_hat[i], sigma);
  }
}