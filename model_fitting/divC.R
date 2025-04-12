
### Fitting Diversity Model (Centered)

library(tidyverse)
library(rstan)

com_dat <- readRDS("data/com_dat.RDS")

com_dat2 <- com_dat %>%
  filter(!is.na(temp_scaled) &
           !is.na(DO_scaled) & 
           !is.na(diversity) & 
           !is.na(total))

stan_datC1 <- list(N = nrow(com_dat2), 
                   com = com_dat2$diversity, 
                   Si = n_distinct(com_dat2$Site1), 
                   site = as.numeric(as.factor(com_dat2$Site1)), 
                   temp = c(com_dat2$temp_scaled), 
                   DO = c(com_dat2$DO_scaled))

div_mod <- stan(file = "Stan_Scripts/com_fullC.stan", 
                data = stan_datP, iter = 1000, 
                cores = 4, chains = 4)

saveRDS(div_mod, file = "div_modC.RDS")