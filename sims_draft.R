
# Function to run cross-scale climate effects simulations
# Jeremy Collings, October 2024

sim_fun <- function(S = 10, Ti = 30, N0 = 100, NI = 20, 
         dd = FALSE, iv = FALSE,
         beta0_mean = 50, beta0_sd = 20, 
         beta_mean = 0, beta_sd = 1,
         alpha.sd = 0.01, ind_sd = .1, 
         p1 = 1, p2 = 1, burnin = 50, 
         model = TRUE, plot = TRUE){
  # S = number of species; Ti = length of time series; 
  # N0 = starting population sizes; NI = number of individuals samples yearly;
  # dd = density-dependence; # iv = individual-level variation in climate responses; 
  # beta0_mean = mean population-level baseline performance; 
  # beta0_sd = standard deviation of population-level baseline performances;
  # beta_mean = mean population-level climate responses; 
  # beta_sd = standard deviation of population-level climate responses; 
  # alpha.sd = standard deviation of self-limitation strengths; 
  # ind_sd = standard deviation of individual-level variation in climate responses; 
  # p1 & p2 = alpha & beta for beta distribution of detection probabilities
  # burnin = length of burn in
  # model = report parameter estimates
  # plot = report figures
  
  if(dd){
    # generate vector of self limitation terms
    alpha <- abs(rnorm(S, 0, alpha.sd))
  }
  
  # generate vector of population-level baseline performances
  lambda0 <- abs(rnorm(S, beta0_mean, beta0_sd))
  # generate vector of population-level climate responses
  beta <- rnorm(S, beta_mean, beta_sd)
  # generate vector of detection probabilities
  ps <- rbeta(S, p1, p2)
  
  Ns <- rep(N0, S)
  
  # Burn in to get starting population sizes
  # Ns <- rep(N0, S) # initialize all populations at size N0
  # for(i in 1:burnin){
  #   if(dd){
  #     Ns <- round(Ns*(lambda0/(1 + alpha*Ns))) # Ricker style self-regulation
  #   }
  #   else{
  #     Ns <- round(Ns*lambda0)
  #   }
  # }
  
  # Measurements
  com_dat <- matrix(nrow = S, ncol = Ti + 1) # create an empty matrix for community survey data
  com_dat[,1] <- rbinom(Ns, Ns, ps) # fill first column with starting pop sizes
  ind_dat <- array(dim = c(S, Ti, NI)) # create an empty array for individual level measurements
  clim <- rnorm(Ti) # create climate data
  
  for(i in 1:Ti){
    for(s in 1:S){
      if(dd){
        if(iv){
          fit <- rnbinom(Ns[s], mu = (lambda0[s] + clim[i]*
                                        rnorm(1, beta[s], ind_sd))/
                           (1 + Ns[s]*alpha[s]), size = S)
        }
        else{
          fit <- rnbinom(Ns[s], mu = (lambda0[s] + clim[i]*beta[s])/
                           (1 + Ns[s]*alpha[s]), size = S)
        }
      }
      else{
        if(iv){
          fit <- rnbinom(Ns[s], mu = lambda0[s] + clim[i]*
                                        rnorm(1, beta[s], ind_sd), size = S)
        }
        else{
          fit <- rnbinom(Ns[s], mu = lambda0[s] + clim[i]*beta[s], size = S)
        }
      }
      com_dat[s,i+1] <- rbinom(1, sum(fit), ps[s])
      ind_dat[s,i,] <- sample(fit, NI)
    }
  }
  
  fit_dat <- cbind.data.frame(fit = c(ind_dat), 
                              sp = rep(1:S, NI*Ti), 
                              time = rep(rep(1:Ti, each = S), NI))
  fit_dat$clim <- clim[fit_dat$time]
  
  pop_dat <- cbind.data.frame(size = c(com_dat[,-1]), 
                              sp = rep(1:S, Ti), 
                              time = rep(1:Ti, each = S))
  pop_dat$clim <- clim[pop_dat$time]
  
  div_dat <- cbind.data.frame(div = apply(com_dat, 2, diversity)[-1], 
                              time = 1:Ti, 
                              tot = apply(com_dat, 2, sum)[-1])
  div_dat$clim <- clim[div_dat$time]
  
  if(model){
    mod.ind <- lme4::lmer(scale(fit) ~ clim + (clim|sp), data = fit_dat)
    mod.pop <- lme4::lmer(scale(size) ~ clim + (clim|sp), data = pop_dat)
    mod.div <- lm(scale(div) ~ clim, data = div_dat)
    mod.tot <- lm(scale(tot) ~ clim, data = div_dat)
    
    print("Individual & Population Estimates")
    print(cbind.data.frame(real_effects = beta, 
                           ind_estimates = coefficients(mod.ind)[[1]][,2],
                           pop_estimates = coefficients(mod.pop)[[1]][,2]))
    print("Community Estimates")
    print(cbind.data.frame(div = coefficients(mod.div)[2], 
                           tot = coefficients(mod.tot)[2]))
    
  }
  
  if(plot){
    p1 <- ggplot(fit_dat, aes(x = clim, y = fit, color = as.factor(sp))) + 
      geom_point() + geom_smooth(method = "lm") + 
      theme_classic(base_size = 15) + 
      xlab("Climate Variable") + ylab("Fitness") + 
      scale_color_discrete(name = "Species")
    print(p1)
    
    p2 <- ggplot(pop_dat, aes(x = clim, y = size, color = as.factor(sp))) + 
      geom_point() + geom_smooth(method = "lm") + 
      theme_classic(base_size = 15) + 
      xlab("Climate Variable") + ylab("Population Count") + 
      scale_color_discrete(name = "Species")
    print(p2)
    
    p3 <- ggplot(div_dat, aes(x = clim, y = div)) + 
      geom_point() + 
      theme_classic(base_size = 15) + 
      xlab("Climate Variable") + ylab("Shannon Diversity")
    print(p3)
    
    p4 <- ggplot(div_dat, aes(x = clim, y = tot)) + 
      geom_point() +
      theme_classic(base_size = 15) + 
      xlab("Climate Variable") + ylab("Total Count")
    print(p4)
  }
}

set.seed(6)
# what does increasing the variation in population-level responses do to inference?
sim_fun(beta_mean = -2.5, beta_sd = 1)
sim_fun(beta_mean = -2.5, beta_sd = 2.5)
sim_fun(beta_mean = -2.5, beta_sd = 5)
sim_fun(beta_mean = -2.5, beta_sd = 10)


