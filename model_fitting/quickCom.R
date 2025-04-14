
# quick look at community models 

library(tidyverse)
library(tidybayes)

totC <- readRDS(file.path("RDSs", "tot_modC.RDS"))
totUC <- readRDS(file.path("RDSs", "tot_modUC.RDS"))
divC <- readRDS(file.path("RDSs", "div_modC.RDS"))
divUC <- readRDS(file.path("RDSs", "div_modUC.RDS"))

summary(totC)
summary(divC)

com_dat <- readRDS("data/com_dat.RDS")

com_dat2 <- com_dat %>%
  filter(!is.na(temp_scaled) &
           !is.na(DO_scaled) & 
           !is.na(diversity) & 
           !is.na(total))

# diagnostics for UC models are really bad... let's go with C models for now
# JC note... check UC models later

# refresher on total CPUE data
ggplot(com_dat2, aes(x = temp_scaled, y = total, 
                     color = Site1)) + 
  geom_point() + ylab("Total CPUE") + xlab("Scaled Temperature") + 
  theme_classic(base_size = 15)

ggplot(com_dat2, aes(x = DO_scaled, y = total, 
                     color = Site1)) + 
  geom_point() + ylab("Total CPUE") + xlab("Scaled DO") + 
  theme_classic(base_size = 15)

# Probability of effects on total CPUE being positive? 
totC %>%
  spread_draws(beta_temp, beta_DO) %>%
  pivot_longer(cols = 4:5, names_to = "var", values_to = "val") %>%
  group_by(var) %>%
  summarise(PD = sum(val > 0) / nrow(.))

# Plotting effects on total CPUE
totC %>%
  spread_draws(beta_temp, beta_DO) %>%
  pivot_longer(cols = 4:5, names_to = "var", values_to = "val") %>%
  ggplot(aes(x = var, y = val, color = var)) + 
  stat_halfeye() + 
  theme_classic(base_size = 15) + 
  scale_x_discrete(name = "Driver", labels = c("DO", "Temperature")) + 
  ylab("Effect on Total CPUE") + 
  theme(legend.position = "none") + 
  geom_hline(yintercept = 0, linetype = "dashed")

# refresher on diversity data
ggplot(com_dat2, aes(x = temp_scaled, y = diversity, 
                     color = Site1)) + 
  geom_point() + ylab("Shannon Diversity") + xlab("Scaled Temperature") + 
  theme_classic(base_size = 15)

ggplot(com_dat2, aes(x = DO_scaled, y = diversity, 
                     color = Site1)) + 
  geom_point() + ylab("Shannon Diversity") + xlab("Scaled DO") + 
  theme_classic(base_size = 15)

# Probability of effects on diversity being positive? 
divC %>%
  spread_draws(beta_temp, beta_DO) %>%
  pivot_longer(cols = 4:5, names_to = "var", values_to = "val") %>%
  group_by(var) %>%
  summarise(PD = sum(val > 0) / nrow(.))

# Plotting effects on diversity
divC %>%
  spread_draws(beta_temp, beta_DO) %>%
  pivot_longer(cols = 4:5, names_to = "var", values_to = "val") %>%
  ggplot(aes(x = var, y = val, color = var)) + 
  stat_halfeye() + 
  theme_classic(base_size = 15) + 
  scale_x_discrete(name = "Driver", labels = c("DO", "Temperature")) + 
  ylab("Effect on Diversity") + 
  theme(legend.position = "none") + 
  geom_hline(yintercept = 0, linetype = "dashed")
