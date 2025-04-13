
### Fitting Population Model (Un-centered)

library(tidyverse)
library(rstan)

pop_dat <- readRDS("data/pop_dat.RDS")

pop_dat2 <- pop_dat %>%
  filter(!is.na(temp_scaled) &
           !is.na(DO_scaled) & 
           !is.na(CPUE))

stan_datP <- list(N = nrow(pop_dat2), 
                  y = pop_dat2$CPUE, 
                  Si = n_distinct(pop_dat2$Site1), 
                  site = as.numeric(as.factor(pop_dat2$Site1)), 
                  Sp = n_distinct(pop_dat2$SCI_NAME), 
                  species = as.numeric(as.factor(pop_dat2$SCI_NAME)),
                  temp = c(pop_dat2$temp_scaled), 
                  DO = c(pop_dat2$DO_scaled))

pop_mod <- stan(file = "Stan_Scripts/ind_or_pop_mod.stan", 
                data = stan_datP, iter = 1000, 
                cores = 4, chains = 4)

saveRDS(pop_mod, file = "pop_modUC.RDS")