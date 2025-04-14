
# Quick and Dirty Look at Individual and Population Level Effects

library(tidyverse)
library(rstan)

ind_dat <- readRDS("data/ind_dat.RDS")
pop_dat <- readRDS("data/pop_dat.RDS")
com_dat <- readRDS("data/com_dat.RDS")

ind_dat2 <- ind_dat %>%
  filter(!is.na(temp_scaled) &
           !is.na(DO_scaled) & 
           !is.na(SIZE))

pop_dat2 <- pop_dat %>%
  filter(!is.na(temp_scaled) &
           !is.na(DO_scaled) & 
           !is.na(CPUE)) %>%
  group_by(SCI_NAME, Site1) %>%
  filter(any(count != 0)) %>%
  ungroup()

com_dat2 <- com_dat %>%
  filter(!is.na(temp_scaled) &
           !is.na(DO_scaled) & 
           !is.na(diversity) & 
           !is.na(total))

slopesI <- psI <- varsI <- sitesI <- speciesI <- c()
for(i in unique(ind_dat2$Site1)){
  temp_dat <- ind_dat2 %>%
    filter(Site1 == i)
  for(j in unique(temp_dat$SCI_NAME)){
    temp_dat2 <- temp_dat %>%
      filter(SCI_NAME == j)
    temp_mod <- try(lm(scale(temp_dat$SIZE) ~ 
                         temp_dat$temp_scaled + 
                         temp_dat$DO_scaled))
    if(!is.character(temp_mod)){
      if(all(dim(summary(temp_mod)$coefficients) == c(3, 4))){
      slopesI <- c(slopesI, unname(coef(temp_mod)[2:3]))
      psI <- c(psI, unname(summary(temp_mod)$coefficients[2:3, 4]))
      varsI <- c(varsI, c("temp", "DO"))
      sitesI <- c(sitesI, rep(i, 2))
      speciesI <- c(speciesI, rep(j, 2))
      }
    }
  }
}
ind_mod_dat <- cbind.data.frame(slopesI, psI, varsI, sitesI, speciesI)
ind_mod_dat %>%
  filter(psI < 0.05) %>%
  ggplot(aes(x = slopesI, fill = varsI)) + 
  geom_histogram(position = "identity", alpha = .5) + 
  xlim(-10, 10)

slopesP <- psP <- varsP <- sitesP <- speciesP <-  c()
for(i in unique(pop_dat2$Site1)){
  temp_dat <- pop_dat2 %>%
    filter(Site1 == i)
  for(j in unique(temp_dat$SCI_NAME)){
    temp_dat2 <- temp_dat %>%
      filter(SCI_NAME == j)
    temp_mod <- try(lm(scale(temp_dat$CPUE) ~ 
                         temp_dat$temp_scaled + 
                         temp_dat$DO_scaled))
    if(!is.character(temp_mod)){
      if(all(dim(summary(temp_mod)$coefficients) == c(3, 4))){
        slopesP <- c(slopesP, unname(coef(temp_mod)[2:3]))
        psP <- c(psP, unname(summary(temp_mod)$coefficients[2:3, 4]))
        varsP <- c(varsP, c("temp", "DO"))
        sitesP <- c(sitesP, rep(i, 2))
        speciesP <- c(speciesP, rep(j, 2))
      }
    }
  }
}
pop_mod_dat <- cbind.data.frame(slopesP, psP, varsP, sitesP, speciesP)
pop_mod_dat %>%
  filter(psP < 0.05) %>%
  ggplot(aes(x = slopesP, fill = varsP)) + 
  geom_histogram(position = "identity", alpha = .5) + 
  xlim(-1, 1)

names(ind_mod_dat) <- names(pop_mod_dat) <- c("slopes", "ps", "vars", 
                                              "sites", "species")

full_mod_dat <- merge(ind_mod_dat, pop_mod_dat, by = c("vars", "sites", "species"), 
      suffixes = c(".ind", ".pop"))

full_mod_dat %>%
  ggplot(aes(x = slopes.ind, y = slopes.pop, color = vars)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_classic(base_size = 15) + 
  xlab("Effect on Size") + 
  ylab("Effect on CPUE")
