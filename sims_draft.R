
library(tidyverse)
library(vegan)

# Function to run cross-scale climate effects simulations
# Jeremy Collings, October 2024

sim_fun <- function(S = 10, Ti = 30, N0 = 100, NI = 20, 
         dd = FALSE, iv = FALSE, mind = FALSE, 
         disp = 1, beta0_mean = 10, beta0_sd = 2, 
         beta_mean = 0, beta_sd = 0.1,
         alpha.sd = 0.1, ind_sd = .1, 
         p1 = 1, p2 = 1, burnin = 50, 
         model = TRUE, plot = TRUE){
  # S = number of species; Ti = length of time series; 
  # N0 = starting population sizes; NI = number of individuals samples yearly;
  # iv = individual-level variation in climate responses; 
  # mind = marked individuals;
  # disp = dispersion parameter for negative binomial of pop size;
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
  
  if(mind & !iv) stop("Cannot mark individuals if there is no individual variation.")
  
  alpha <- abs(rnorm(S, 0, alpha.sd))
  
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
  
  if(mind){
    ind_eff <- matrix(NA, nrow = NI, ncol = S)
    for(i in 1:S){
      ind_eff[,i] <- rnorm(NI, beta[i], ind_sd)
    }
  }
  
  for(i in 1:Ti){
    for(s in 1:S){
      if(iv){
        if(mind){
          fit <- rnbinom(Ns[s] - NI, mu = (lambda0[s] + clim[i]*
                                             rnorm(1, beta[s], ind_sd))/
                           (1 + Ns[s]*alpha[s]), size = disp) 
          ind_dat[s,i,] <- rnbinom(NI, mu = (lambda0[s] + clim[i]*
                                               ind_eff[,s])/
                                     (1 + Ns[s]*alpha[s]), size = disp)
          fit <- c(fit, ind_dat[s,i,])
          }
        else{
          fit <- rnbinom(Ns[s], mu = (lambda0[s] + clim[i]*
                                        rnorm(1, beta[s], ind_sd))/
                           (1 + Ns[s]*alpha[s]), size = disp)       
        }
        }
      else{
        fit <- rnbinom(Ns[s], mu = (lambda0[s] + clim[i]*beta[s])/
                         (1 + Ns[s]*alpha[s]), size = disp)
        }
      
      Ns[s] <- sum(fit)
      com_dat[s,i+1] <- rbinom(1, sum(fit), ps[s])
      if(!mind) ind_dat[s,i,] <- ifelse(length(fit) >= NI, 
                                        sample(fit, NI), fit)
    }
  }
  
  fit_dat <- cbind.data.frame(fit = c(ind_dat), 
                              sp = rep(1:S, NI*Ti), 
                              time = rep(rep(1:Ti, each = S), NI))
  fit_dat$clim <- clim[fit_dat$time]
  if(mind) fit_dat$ind <- paste("sp", fit_dat$sp, "ind", rep(1:NI, each = S*Ti), sep = "")
  
  pop_dat <- cbind.data.frame(size = c(com_dat[,-1]), 
                              sp = rep(1:S, Ti), 
                              time = rep(1:Ti, each = S))
  pop_dat$clim <- clim[pop_dat$time]
  
  div_dat <- cbind.data.frame(div = apply(com_dat, 2, diversity)[-1], 
                              time = 1:Ti, 
                              tot = apply(com_dat, 2, sum)[-1])
  div_dat$clim <- clim[div_dat$time]
  
  if(model){
    if(mind){
      mod.ind <- lme4::lmer(scale(fit) ~ clim + (clim|sp/ind), data = fit_dat)
    }
    else{
      mod.ind <- lme4::lmer(scale(fit) ~ clim + (clim|sp), data = fit_dat)
    }
    
    mod.pop <- lme4::lmer(scale(size) ~ clim + (clim|sp), data = pop_dat)
    mod.div <- lm(scale(div) ~ clim, data = div_dat)
    mod.tot <- lm(scale(tot) ~ clim, data = div_dat)
    
    print("Individual & Population Estimates")
    print(cbind.data.frame(real_effects = beta, 
                           ind_estimates = coefficients(mod.ind)[["sp"]][,2],
                           pop_estimates = coefficients(mod.pop)[["sp"]][,2]))
    print("Community Estimates")
    print(cbind.data.frame(div = coefficients(mod.div)[2], 
                           tot = coefficients(mod.tot)[2]))
    
  }
  
  if(plot){
    if(mind){
      p1 <- ggplot(fit_dat, aes(x = clim, y = fit, 
                                color = as.factor(sp), group = ind)) + 
        geom_point() + geom_smooth(method = "lm", se = FALSE) + 
        theme_classic(base_size = 15) + 
        xlab("Climate Variable") + ylab("Fitness") + 
        scale_color_discrete(name = "Species")
    }
    else{
      p1 <- ggplot(fit_dat, aes(x = clim, y = fit, color = as.factor(sp))) + 
        geom_point() + geom_smooth(method = "lm", se = FALSE) + 
        theme_classic(base_size = 15) + 
        xlab("Climate Variable") + ylab("Fitness") + 
        scale_color_discrete(name = "Species")
    }
    
    print(p1)
    
    p2 <- ggplot(pop_dat, aes(x = clim, y = size, color = as.factor(sp))) + 
      geom_point() + geom_smooth(method = "lm", se = FALSE) + 
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
sim_fun(iv = TRUE, Ti = 30, NI = 10, beta_mean = -1, beta_sd = .5)
sim_fun(beta_mean = -2.5, beta_sd = 2.5)
sim_fun(beta_mean = -2.5, beta_sd = 5)
sim_fun(beta_mean = -2.5, beta_sd = 10)

# to do:
# 1. test the hypothesis that diversity scales with population impacts IF their 
# is a negative correlation between the population-level response and the 
# population-level baseline fitness

# 3. add multiple sites and test with hierarchical model

# bugs: 
# fake individual-level variation
# safe guard against mu in negbinom being negative

