
### Fitting Individual Model (Un-centered)

library(tidyverse)
library(rstan)

ind_dat <- readRDS("data/ind_dat.RDS")

ind_dat2 <- ind_dat %>%
  filter(!is.na(temp_scaled) &
           !is.na(DO_scaled) & 
           !is.na(SIZE))

stan_datI <- list(N = nrow(ind_dat2), 
                  y = ind_dat2$SIZE, 
                  Si = n_distinct(ind_dat2$Site1), 
                  site = as.numeric(as.factor(ind_dat2$Site1)), 
                  Sp = n_distinct(ind_dat2$SCI_NAME), 
                  species = as.numeric(as.factor(ind_dat2$SCI_NAME)),
                  temp = c(ind_dat2$temp_scaled), 
                  DO = c(ind_dat2$DO_scaled))

ind_mod <- stan(file = "Stan_Scripts/ind_or_pop_mod.stan", 
                data = stan_datI, iter = 1000, 
                cores = 4, chains = 4)

saveRDS(ind_mod, file = "ind_modUC.RDS")