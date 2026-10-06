#_________________________________
# Creating visualizations by midsite and PDF output
# SCALES/ SSECR                  
# Allie Case 
# R version 4.4.2 (2024-10-31)
#_________________________________

# SETUP ---------------------

rm(list = ls())

## LOAD PACKAGES AND FUNCTIONS --------------------

#install.packages("librarian")

librarian::shelf(supportR, tidyverse, summarytools, 
                 datacleanr, lterdatasampler,
                 cowplot, gt, corrplot,
                 vegan, neonUtilities, ggcorrplot, 
                 emmeans, ggpubr, gridExtra)

source(file = file.path("scripts",
                        "functions.R"))

source(file = file.path("scripts",
                        "viz_ideas.R"))

## LOAD DATA ----

#this is the R object that is all of our harmonized data so far: 
NEON_data <- readRDS(file = file.path("data",
                                      "NEON_harmonized.Rds"))
# VIZ LOOP DEMOS ----- 

#create demo site just to make sure visuals work 

### Correlation matrix of predictor variables -----

demo <- NEON_data %>% 
  filter(SITE == "NEON_ARIK")

## Correlation matrix of all predictor variables 

predictor_cor <- demo %>% 
  select(SIZE, mean_daily_DO:mean_min_temp) %>% 
  cor(use = "complete.obs")

corrplot(predictor_cor,
         method = "color", 
         addCoef.col="black", 
         order = "AOE", 
         number.cex=.75,
         type = "upper",
         tl.cex = .75,
         tl.col = "black",
         diag = F,
         tl.srt = 45)

### Size ~ Year colored by species -----

demo %>% 
  ggplot(aes(x=YEAR,y=SIZE, color = SCI_NAME))+
  geom_point(alpha = .25) +
  theme_cowplot() +
  theme(legend.position = "none")

### mean daily temp ~ year -----

demo %>% 
  ggplot(aes(x=YEAR, y=mean_daily_temp))+
  geom_point(color="darkred") +
  ylab("MEAN DAILY TEMP") +
  scale_x_continuous(labels = function(x) x -1) +
  theme_cowplot()

### average annual DO ~ year -----

demo %>% 
  ggplot(aes(x=YEAR, y=annual_avg_DO))+
  geom_point(color="navy") + 
  ylab("ANNUAL AVG. DO") +
  scale_x_continuous(labels = function(x) x -1) +
  theme_cowplot()

### individual level ~ temperature ------

demo %>% 
  ggplot(aes(x=mean_daily_temp, y=SIZE))+
  geom_point(aes(color = SCI_NAME), alpha = .25) +
  geom_smooth(aes(color = SCI_NAME), method=lm,se=F)+
  geom_smooth(color = "black", linetype = "dashed", method=lm,se=F)+
  xlab("MEAN DAILY TEMP") +
  theme_cowplot() +
  theme(legend.position = "none")

### individual level ~ temperature model accounting for DO ------

do_temp_size_model <- lm(log(SIZE) ~ mean_daily_temp * 
                           annual_avg_DO, demo)

predict_size_temp <- data.frame(emmeans(do_temp_size_model, 
                                        ~mean_daily_temp * annual_avg_DO, 
                                        at=list(mean_daily_temp=c(15.5, 16, 16.5,17, 17.5))), 
                                type = "response")

ggplot(predict_size_temp,aes(x=mean_daily_temp, y=emmean))+
  geom_line(color = "darkred", linewidth =1) +
  geom_ribbon(data = predict_size_temp, aes(ymin=lower.CL, ymax=upper.CL), alpha = .25, fill = "darkred") +
  geom_ribbon(data = predict_size_temp, aes(ymin=lower.CL, ymax=upper.CL), alpha = .25, fill = "darkred") +
  xlab("MEAN DAILY TEMP") +
  ylab("SIZE") +
  theme_cowplot() +
  theme(legend.position = "none")

### individual level ~ DO ------

demo %>% 
  ggplot(aes(x=annual_avg_DO, y=SIZE))+
  geom_point(aes(color = SCI_NAME), alpha = .25)+
  geom_smooth(aes(color = SCI_NAME), method=lm,se=F)+
  geom_smooth(color = "black", linetype = "dashed", method=lm,se=F)+
  xlab("ANNUAL AVG. DO") +
  theme_cowplot() +
  theme(legend.position = "none")

### individual level ~ DO model accounting for temperature ------

predict_size_do <- data.frame(emmeans(do_temp_size_model, ~mean_daily_temp * annual_avg_DO, 
                                      at=list(annual_avg_DO=c(8.5, 9, 9.5, 10, 10.5)), 
                                      type = "response"))

ggplot(predict_size_do,aes(x=annual_avg_DO, y=response))+
  geom_line(color = "navy", linewidth =1) +
  geom_ribbon(data = predict_size_do, aes(ymin=lower.CL, ymax=upper.CL), alpha = .25, fill = "navy") +
  geom_ribbon(data = predict_size_do, aes(ymin=lower.CL, ymax=upper.CL), alpha = .25, fill = "navy") +
  xlab("ANNUAL AVG. DO") +
  ylab("SIZE") +
  theme_cowplot() +
  theme(legend.position = "none")

### population level skip for now until effort is more ironed out 

##all of those graphs work so now build into a loop 

NEON_data %>% 
  select(SITE, SIZE, mean_daily_DO:mean_min_temp) %>% 
  group_by(SITE) %>% 
  summarise(across(everything(), ~sum(is.na(.))))


#MAKE PDF LOOP ---- 

sites <- unique(NEON_data$SITE)

for(i in seq_along(sites)) {
  
  predictor_cor <- NEON_data %>%
    filter(SITE == sites[i]) %>%
    select(SIZE, mean_daily_DO:mean_min_temp) %>%
    cor()
  
  ### Convert correlation matrix into a ggplot object ----
  plot1 <- ggcorrplot(
    predictor_cor,
    type = "lower",
    outline.color = "white",
    ggtheme = ggplot2::theme_bw,
    colors = c("#6D9EC1", "white", "#E46726"), 
    lab = TRUE) +
    labs(title = 
           paste0("Correlation of environmental drivers and size at site ", sites[i])) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ### Size ~ Year colored by species -----
  plot2 <- NEON_data %>% 
    filter(SITE == sites[i]) %>% 
    ggplot(aes(x = YEAR, y = SIZE, color = SCI_NAME)) +
    geom_point(alpha = 0.25) +
    theme_cowplot() +
    theme(legend.position = "none") +
    labs(title = paste0("Size versus year at site ", sites[i]," colored by species")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ###Mean daily temp versus year ----
  
  plot3 <- NEON_data %>% 
    filter(SITE == sites[i]) %>% 
    ggplot(aes(x=YEAR, y=mean_daily_temp))+
    geom_point(color="darkred") +
    ylab("MEAN DAILY TEMP") +
    scale_x_continuous(labels = function(x) x -1) +
    theme_cowplot() +
    labs(title = paste0("Mean daily water temperature verus year at site ", sites[i])) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  ### average annual DO ~ year -----
  
  plot4 <- NEON_data %>% 
    filter(SITE == sites[i]) %>% 
    ggplot(aes(x=YEAR, y=annual_avg_DO))+
    geom_point(color="navy") + 
    ylab("ANNUAL AVG. DO") +
    scale_x_continuous(labels = function(x) x -1) +
    theme_cowplot() +
    labs(title = paste0("Mean annual DO verus year at site ", sites[i])) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ### individual level ~ temperature ------
  
  plot5 <- NEON_data %>% 
    filter(SITE == sites[i]) %>% 
    ggplot(aes(x=mean_daily_temp, y=SIZE))+
    geom_point(aes(color = SCI_NAME), alpha = .25) +
    geom_smooth(aes(color = SCI_NAME), method=lm,se=F)+
    geom_smooth(color = "black", linetype = "dashed", 
                method=lm,se=F)+
    xlab("MEAN DAILY TEMP") +
    theme_cowplot() +
    theme(legend.position = "none") +
    labs(title = paste0("Size versus mean daily water temp at site ", sites[i])) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ### individual level ~ temperature model accounting for DO ------
  
  do_temp_size_model <- lm(log(SIZE) ~ mean_daily_temp * annual_avg_DO, 
                           data = 
                             NEON_data[NEON_data$SITE == sites[i], ])
  
  predict_size_temp <- data.frame(emmeans(do_temp_size_model,
                                          ~mean_daily_temp * annual_avg_DO,
                                          at=list(mean_daily_temp=c(15.5, 16, 16.5,17, 17.5))),
                                  type = "response")
  
  if (all(c("lower.CL", "upper.CL") %in% names(predict_size_temp))) {
    plot6 <- ggplot(predict_size_temp, aes(x = mean_daily_temp, y = emmean)) +
      geom_line(color = "darkred", linewidth = 1) +
      geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.25, fill = "darkred") +
      xlab("MEAN DAILY TEMP") +
      ylab("SIZE") +
      theme_cowplot() +
      theme(legend.position = "none") +
      labs(title = paste0("Size vs. temp accounting for DO at site ", sites[i]))
  } else {
    message("Skipping plot for site ", sites[i], ": CI could not be estimated.")
  }
  # 
  # ### individual level ~ DO ------
  
  plot7 <- NEON_data %>% 
    filter(SITE == sites[i]) %>%
    ggplot(aes(x=annual_avg_DO, y=SIZE))+
    geom_point(aes(color = SCI_NAME), alpha = .25)+
    geom_smooth(aes(color = SCI_NAME), method=lm,se=F)+
    geom_smooth(color = "black", linetype = "dashed", method=lm,se=F)+
    xlab("ANNUAL AVG. DO") +
    theme_cowplot() +
    theme(legend.position = "none") +
    labs(title = paste0("Size verus DO at site ", sites[i]))
  
  ### individual level ~ DO model accounting for temperature ------
  
  predict_size_do <- data.frame(emmeans(do_temp_size_model, ~mean_daily_temp * annual_avg_DO,
                                        at=list(annual_avg_DO=c(8.5, 9, 9.5, 10, 10.5)),
                                        type = "response"))
  
  if (all(c("lower.CL", "upper.CL") %in% names(predict_size_temp))) {
    plot8 <- ggplot(predict_size_do,aes(x=annual_avg_DO, y=response))+
      geom_line(color = "navy", linewidth =1) +
      geom_ribbon(data = predict_size_do, aes(ymin=lower.CL, ymax=upper.CL), alpha = .25, fill = "navy") +
      geom_ribbon(data = predict_size_do, aes(ymin=lower.CL, ymax=upper.CL), alpha = .25, fill = "navy") +
      xlab("ANNUAL AVG. DO") +
      ylab("SIZE") +
      theme_cowplot() +
      theme(legend.position = "none") +
      labs(title = paste0("Size vs. DO accounting for temp at site ", sites[i]))   } else {
        message("Skipping plot for site ", sites[i], ": CI could not be estimated.")
      }
  
  # Arrange both plots using ggarrange()
  
  pdf(file = paste0("viz/", sites[i],"_plots.pdf"),
      paper = "a4")
  print(plot1)
  print(plot2)
  print(plot3)
  print(plot4)
  print(plot5)
  print(plot6)
  print(plot7)
  print(plot8)
  
  dev.off()
  
}



