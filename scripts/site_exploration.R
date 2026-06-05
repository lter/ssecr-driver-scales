#LTER Site Exploration
# SCALES/ SSECR                  
# Bethany Williams  
# R Version: 4.4.1 (2024-6-14) -- "Race for your life"
#_________________________________

# setup ---------------------

rm(list = ls())


#install.packages("librarian")

librarian::shelf(supportR, tidyverse, summarytools, 
                 datacleanr, lterdatasampler,
                 cowplot, gt,
                 vegan,readxl,GGally,brms,bayestestR,
                 ggpubr,tidybayes,ggpubr)

source(file = file.path("scripts",
                        "functions.R"))

source(file = file.path("scripts",
                        "viz_ideas.R"))

## load Site data ---------------------

#internal note, make sure to download data from Google drive first
#load info on sites that were included in dataset
sitedata <- read_csv(file = file.path("data",
                                         "final_harmonized_midsites_summary.csv"))
#rename some things
sitedata<-sitedata%>%
  rename(Unique_Species=`Unique Species`,
         Years_Fish_Data=`Years of Fish Data`,
         Years_Temp_Data=`Years of Temp Data`)
#load final harmonized data to extract do and temp average for site
envdata<- read_csv(file = file.path("data",
                                    "final_harmonized.csv"))

#load coordinates for each site
coord<-read_xlsx(path = file.path("data",
                          "allsites_coords.xlsx"))
coord$...8<-NULL
#combine site and coordinate data

sitedata<-merge(sitedata,coord, by="MIDSITE")
sitedata$...8<-NULL
sitedata$...1<-NULL

#get averages for temp and do for each site
TEMP <- envdata %>% 
  group_by(MIDSITE) %>%
  reframe(mean_temp = mean(mean_daily_temp, na.rm = T))

DO <- envdata %>% 
  group_by(MIDSITE) %>%
  reframe(mean_DO = mean(annual_avg_DO, na.rm = T))

sitedata<-merge(sitedata,TEMP,by="MIDSITE")
sitedata<-merge(sitedata,DO,by="MIDSITE")


## load Parameter Estimates ---------------------
div <- read_csv(file = file.path("data",
                                      "marg_si_com.csv"))

cpue <- read_csv(file = file.path("data",
                                 "marg_si_pop.csv"))

sl <- read_csv(file = file.path("data",
                                  "marg_si_ind.csv"))

#cpue2 <- read_csv(file = file.path("data",
#                                  "pop.csv"))

sl2 <- read_csv(file = file.path("data",
                              "ind.csv"))

#remove prefixes from site names to be able to merge

#patterns_to_remove <- c("LTER_","NEON_","IEP_")
#combined_pattern <- paste(patterns_to_remove, collapse = "|")
#sitedata$MIDSITE<-str_remove_all(sitedata$MIDSITE,combined_pattern)

#get names matching
sitedata <- sitedata %>%   rename(si = MIDSITE)


#merge to get final datasets for graphing

cpue<-merge(sitedata,cpue,by="si")
cpue<-subset(cpue,si!="MCR_Backreef"& si!="MCR_FringingReef" &si!="MCR_Forereef")
#cpue2<-merge(sitedata,cpue2,by="si")
#cpue2<-subset(cpue2,si!="MCR_Backreef"& si!="MCR_FringingReef" &si!="MCR_Forereef")

div<-merge(sitedata,div,by="si")
div<-subset(div,si!="MCR_Backreef"& si!="MCR_FringingReef" &si!="MCR_Forereef")

sl<-left_join(sl,sitedata,by="si")

#try without morea since its lat is so different
sl<-subset(sl,si!="MCR_Backreef"& si!="MCR_FringingReef" &si!="MCR_Forereef")

sl2<-left_join(sl2,sitedata,by="si")

#try without morea since its lat is so different
sl2<-subset(sl2,si!="MCR_Backreef"& si!="MCR_FringingReef" &si!="MCR_Forereef")


#Checking for Overall Differences Between Marine and Freshwater

ggplot(data=sl,aes(x=Habitat_Broad,y=median))+
  geom_boxplot()+
  facet_grid(~.variable)+
  theme_cowplot()

ggplot(data=cpue,aes(x=Habitat_Broad,y=median))+
  geom_boxplot()+
  facet_grid(~.variable)+
  theme_cowplot()

ggplot(data=div,aes(x=Habitat_Broad,y=median))+
  geom_boxplot()+
  facet_grid(~.variable)+
  theme_cowplot()


###Make Correlelograms---------------------
inddatatemp<-subset(sl,.variable=="beta_temp_si")
inddatado<-subset(sl,.variable=="beta_DO_si")


popdatatemp<-subset(cpue,.variable=="beta_temp_si")
popdatado<-subset(cpue,.variable=="beta_DO_si")

inddatatemp2<-subset(sl2,.variable=="beta_temp")
inddatado2<-subset(sl2,.variable=="beta_DO")

#popdatatemp2<-subset(cpue2,.variable=="beta_temp")
#popdatado2<-subset(cpue2,.variable=="beta_DO")


divdatatemp<-subset(div,.variable=="beta_temp")
divdatado<-subset(div,.variable=="beta_DO")





#individual
ggpairs(inddatatemp, columns = c(7,8,12:13,18:19,3),ggplot2::aes(color=Habitat_Broad)) 
ggpairs(inddatado, columns = c(8,13,14,18:20,4),ggplot2::aes(color=Habitat_Broadest)) 
cor.test(inddatatemp$median,inddatatemp$Unique_Species,method="pearson")
cor.test(inddatatemp$median,inddatatemp$Years_Fish_Data,method="pearson")
cor.test(inddatatemp$median,inddatatemp$`Years of Temp Data`,method="pearson")
cor.test(inddatatemp$median,inddatatemp$Lat,method="pearson")
cor.test(inddatatemp$median,inddatatemp$Long,method="pearson")
cor.test(inddatatemp$median,inddatatemp$mean_temp,method="pearson")
cor.test(inddatatemp$median,inddatatemp$mean_DO,method="pearson")
str(inddatatemp)

##Temperature Correlations
#brms correlation following this set up https://solomonkurz.netlify.app/blog/2019-02-16-bayesian-correlations-let-s-talk-options/
#first standardize data
inddatatemp2<-inddatatemp2 %>% 
  mutate(median_s = (median - mean(median)) / sd(median),
         Species_s = (Unique_Species - mean(Unique_Species)) / sd(Unique_Species),
         Years_Fish_Data_s = (Years_Fish_Data - mean(Years_Fish_Data)) / sd(Years_Fish_Data),
         Lat_s=(Lat - mean(Lat)) / sd(Lat),
         Long_s=(Long - mean(Long)) / sd(Long),
         mean_temp_s=(mean_temp - mean(mean_temp)) / sd(mean_temp),
         mean_DO_s=(mean_DO - mean(mean_DO)) / sd(mean_DO))

tempcor1<-brm(data = inddatatemp2, 
             family = gaussian,
             median_s~1+Species_s,
             chains = 4, cores = 4, 
             seed = 1)
p_direction(tempcor1)
#should be same correlation as pearson
cor.test(inddatatemp$median,inddatatemp$Unique_Species,method="pearson")
print(tempcor1)

tempcor2<-update(tempcor1,formula=median_s~1+Years_Fish_Data_s,newdata=inddatatemp2,seed=1)
print(tempcor2)
cor.test(inddatatemp$median,inddatatemp$Years_Fish_Data,method="pearson")
p_direction(tempcor2)

tempcor3<-update(tempcor1,formula=median_s~1+Lat_s,newdata=inddatatemp2,seed=1)
print(tempcor3)
p_direction(tempcor3)

tempcor4<-update(tempcor1,formula=median_s~1+Long_s,newdata=inddatatemp2,seed=1)
print(tempcor4)
p_direction(tempcor4)

tempcor5<-update(tempcor1,formula=median_s~1+mean_temp,newdata=inddatatemp2,seed=1)
print(tempcor5)
p_direction(tempcor5)

tempcor6<-update(tempcor1,formula=median_s~1+mean_DO,newdata=inddatatemp2,seed=1)
print(tempcor6)
p_direction(tempcor6)

#make graph
get_rho <- function(fit) {
  as_draws_df(fit) %>% 
    as_tibble()%>%
    select(starts_with("b_"), -contains("Intercept")) %>% 
    set_names("rho") 
}
library(tidybayes)

# collect the posteriors from the univariate models and graph
indtempg<-tibble(name = str_c("tempcor", 1:6)) %>% 
  mutate(fit = map(name, get)) %>% 
  mutate(rho = map(fit, get_rho)) %>% 
  unnest(rho) %>% 
  mutate(predictor = rep(c("median", "median", "median","median","median","median"), each = 4000) %>% rep(., times = 1),
         criterion = rep(c("Unique_Species", "Years_Fish_Data", "Lat","Long","Mean_temp","Mean_DO"), each = 4000) %>% rep(., times = 1)) %>% 
  mutate(label = str_c(predictor, " with ", criterion)) %>%
  select(-c(predictor:criterion))%>%
  # wrangle a bit just to make the y axis easier to understand
  mutate(name = factor(name, 
                       levels = c(str_c("tempcor", 1:6)),
                       labels = c("1. Median vs. # Unique Species",
                                  "2. Median vs. Years of Fish Data",
                                  "3. Median vs. Latitude",
                                  "4. Median vs. Longitude",
                                  "5. Median vs. Mean Temperature",
                                  "6. Median vs. Mean DO")))%>%
  # plot
  ggplot(aes(x = rho, y = name))+
  stat_halfeye(.width = .95, size = 5/4) +
  #scale_x_continuous(breaks = c(-1,1)) +
  labs(x = expression(rho),
       y = NULL) +
  xlim(c(-1,1))+
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))+
  geom_vline(xintercept = 0,linetype = "dashed")+
  theme_cowplot()+
  scale_y_discrete(limits=rev)+
  ggtitle("Individual")

print(indtempg)
#pop
ggpairs(popdatatemp, columns = c(2:3,7,8,13:14,16),ggplot2::aes(color=Habitat_Broad)) 
ggpairs(popdatado, columns = c(2:3,7,12:14,16),ggplot2::aes(color=Habitat_Broadest)) 
cor.test(popdatatemp$median,popdatatemp$`Unique Species`,method="pearson")
cor.test(popdatatemp$median,popdatatemp$`Years of Fish Data`,method="pearson")
cor.test(popdatatemp$median,popdatatemp$`Years of Temp Data`,method="pearson")
cor.test(popdatatemp$median,popdatatemp$Lat,method="pearson")
cor.test(popdatatemp$median,popdatatemp$Long,method="pearson")
cor.test(popdatatemp$median,popdatatemp$mean_temp,method="pearson")
cor.test(popdatatemp$median,popdatatemp$mean_DO,method="pearson")
#standardize 
popdatatemp<-popdatatemp %>% 
  mutate(median_s = (median - mean(median)) / sd(median),
         Species_s = (Unique_Species - mean(Unique_Species)) / sd(Unique_Species),
         Years_Fish_Data_s = (Years_Fish_Data - mean(Years_Fish_Data)) / sd(Years_Fish_Data),
         Lat_s=(Lat - mean(Lat)) / sd(Lat),
         Long_s=(Long - mean(Long)) / sd(Long),
         mean_temp_s=(mean_temp - mean(mean_temp)) / sd(mean_temp),
         mean_DO_s=(mean_DO - mean(mean_DO)) / sd(mean_DO))
#bayesian correlations
poptempcor1<-brm(data = popdatatemp, 
              family = gaussian,
              median_s~1+Species_s,
              chains = 4, cores = 4, 
              seed = 1)
p_direction(poptempcor1)
print(poptempcor1)

poptempcor2<-update(poptempcor1,formula=median_s~1+Years_Fish_Data_s,newdata=popdatatemp,seed=1)
print(poptempcor2)
p_direction(poptempcor2)

poptempcor3<-update(poptempcor1,formula=median_s~1+Lat_s,newdata=popdatatemp,seed=1)
print(poptempcor3)
p_direction(poptempcor3)

poptempcor4<-update(poptempcor1,formula=median_s~1+Long_s,newdata=popdatatemp,seed=1)
print(poptempcor4)
p_direction(poptempcor4)

poptempcor5<-update(poptempcor1,formula=median_s~1+mean_temp,newdata=popdatatemp,seed=1)
print(poptempcor5)
p_direction(poptempcor5)

poptempcor6<-update(poptempcor1,formula=median_s~1+mean_DO,newdata=popdatatemp,seed=1)
print(poptempcor6)
p_direction(poptempcor6)

#graph
poptempg<-tibble(name = str_c("poptempcor", 1:6)) %>% 
  mutate(fit = map(name, get)) %>% 
  mutate(rho = map(fit, get_rho)) %>% 
  unnest(rho) %>% 
  mutate(predictor = rep(c("median", "median", "median","median","median","median"), each = 4000) %>% rep(., times = 1),
         criterion = rep(c("Unique_Species", "Years_Fish_Data", "Lat","Long","Mean_temp","Mean_DO"), each = 4000) %>% rep(., times = 1)) %>% 
  mutate(label = str_c(predictor, " with ", criterion)) %>%
  select(-c(predictor:criterion))%>%
  # wrangle a bit just to make the y axis easier to understand
  mutate(name = factor(name, 
                       levels = c(str_c("poptempcor", 1:6)),
                       labels = c("1. Median vs. # Unique Species",
                                  "2. Median vs. Years of Fish Data",
                                  "3. Median vs. Latitude",
                                  "4. Median vs. Longitude",
                                  "5. Median vs. Mean Temperature",
                                  "6. Median vs. Mean DO")))%>%
  # plot
  ggplot(aes(x = rho, y = name))+
  stat_halfeye(.width = .95, size = 5/4) +
  #scale_x_continuous(breaks = c(-1,1)) +
  labs(x = expression(rho),
       y = NULL) +
  theme_cowplot()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank())+
  geom_vline(xintercept = 0,linetype = "dashed")+
  scale_y_discrete(limits=rev)+
  ggtitle("Population")
print(poptempg)
#div
ggpairs(divdatatemp, columns = c(2:3,7,12:14,16),ggplot2::aes(color=Habitat_Broad)) 
ggpairs(divdatado, columns = c(2:3,7,12:14,16),ggplot2::aes(color=Habitat_Broadest)) 
cor.test(divdatatemp$median,divdatatemp$`Unique Species`,method="pearson")
cor.test(divdatatemp$median,divdatatemp$`Years of Fish Data`,method="pearson")
cor.test(divdatatemp$median,divdatatemp$`Years of Temp Data`,method="pearson")
cor.test(divdatatemp$median,divdatatemp$Lat,method="pearson")
cor.test(divdatatemp$median,divdatatemp$Long,method="pearson")
cor.test(divdatatemp$median,divdatatemp$mean_temp,method="pearson")
cor.test(divdatatemp$median,divdatatemp$mean_DO,method="pearson")

divdatatemp<-divdatatemp %>% 
  mutate(median_s = (median - mean(median)) / sd(median),
         Species_s = (Unique_Species - mean(Unique_Species)) / sd(Unique_Species),
         Years_Fish_Data_s = (Years_Fish_Data - mean(Years_Fish_Data)) / sd(Years_Fish_Data),
         Lat_s=(Lat - mean(Lat)) / sd(Lat),
         Long_s=(Long - mean(Long)) / sd(Long),
         mean_temp_s=(mean_temp - mean(mean_temp)) / sd(mean_temp),
         mean_DO_s=(mean_DO - mean(mean_DO)) / sd(mean_DO))
#bayesian correlations
divtempcor1<-brm(data = divdatatemp, 
                 family = gaussian,
                 median_s~1+Species_s,
                 chains = 4, cores = 4, 
                 seed = 1)
p_direction(divtempcor1)
print(divtempcor1)

divtempcor2<-update(divtempcor1,formula=median_s~1+Years_Fish_Data_s,newdata=divdatatemp,seed=1)
print(divtempcor2)
p_direction(divtempcor2)

divtempcor3<-update(divtempcor1,formula=median_s~1+Lat_s,newdata=divdatatemp,seed=1)
print(divtempcor3)
p_direction(divtempcor3)

divtempcor4<-update(divtempcor1,formula=median_s~1+Long_s,newdata=divdatatemp,seed=1)
print(divtempcor4)
p_direction(divtempcor4)

divtempcor5<-update(divtempcor1,formula=median_s~1+mean_temp,newdata=divdatatemp,seed=1)
print(divtempcor5)
p_direction(divtempcor5)

divtempcor6<-update(divtempcor1,formula=median_s~1+mean_DO,newdata=divdatatemp,seed=1)
print(divtempcor6)
p_direction(divtempcor6)

divtempg<-tibble(name = str_c("divtempcor", 1:6)) %>% 
  mutate(fit = map(name, get)) %>% 
  mutate(rho = map(fit, get_rho)) %>% 
  unnest(rho) %>% 
  mutate(predictor = rep(c("median", "median", "median","median","median","median"), each = 4000) %>% rep(., times = 1),
         criterion = rep(c("Unique_Species", "Years_Fish_Data", "Lat","Long","Mean_temp","Mean_DO"), each = 4000) %>% rep(., times = 1)) %>% 
  mutate(label = str_c(predictor, " with ", criterion)) %>%
  select(-c(predictor:criterion))%>%
  # wrangle a bit just to make the y axis easier to understand
  mutate(name = factor(name, 
                       levels = c(str_c("divtempcor", 1:6)),
                       labels = c("1. Median vs. # Unique Species",
                                  "2. Median vs. Years of Fish Data",
                                  "3. Median vs. Latitude",
                                  "4. Median vs. Longitude",
                                  "5. Median vs. Mean Temperature",
                                  "6. Median vs. Mean DO")))%>%
  # plot
  ggplot(aes(x = rho, y = name))+
  stat_halfeye(.width = .95, size = 5/4) +
  #scale_x_continuous(breaks = c(-1,1)) +
  labs(x = expression(rho),
       y = NULL) +
  theme_cowplot()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank())+
  geom_vline(xintercept = 0,linetype = "dashed")+
  scale_y_discrete(limits=rev)+
  ggtitle("Community")

print(divtempg)
##DO Correlations
#standardize data
inddatado<-inddatado %>% 
  mutate(median_s = (median - mean(median)) / sd(median),
         Species_s = (Unique_Species - mean(Unique_Species)) / sd(Unique_Species),
         Years_Fish_Data_s = (Years_Fish_Data - mean(Years_Fish_Data)) / sd(Years_Fish_Data),
         Lat_s=(Lat - mean(Lat)) / sd(Lat),
         Long_s=(Long - mean(Long)) / sd(Long),
         mean_temp_s=(mean_temp - mean(mean_temp)) / sd(mean_temp),
         mean_DO_s=(mean_DO - mean(mean_DO)) / sd(mean_DO))

docor1<-brm(data = inddatado, 
              family = gaussian,
              median_s~1+Species_s,
              chains = 4, cores = 4, 
              seed = 1)
p_direction(docor1)
#should be same correlation as pearson
print(docor1)

docor2<-update(docor1,formula=median_s~1+Years_Fish_Data_s,newdata=inddatado,seed=1)
print(docor2)
cor.test(inddatado$median,inddatado$Years_Fish_Data,method="pearson")
p_direction(docor2)

docor3<-update(docor1,formula=median_s~1+Lat_s,newdata=inddatado,seed=1)
print(docor3)
p_direction(docor3)

docor4<-update(docor1,formula=median_s~1+Long_s,newdata=inddatado,seed=1)
print(docor4)
p_direction(docor4)

docor5<-update(docor1,formula=median_s~1+mean_temp,newdata=inddatado,seed=1)
print(docor5)
p_direction(docor5)

docor6<-update(docor1,formula=median_s~1+mean_DO,newdata=inddatado,seed=1)
print(docor6)
p_direction(docor6)

# collect the posteriors from the univariate models and graph
inddog<-tibble(name = str_c("docor", 1:6)) %>% 
  mutate(fit = map(name, get)) %>% 
  mutate(rho = map(fit, get_rho)) %>% 
  unnest(rho) %>% 
  mutate(predictor = rep(c("median", "median", "median","median","median","median"), each = 4000) %>% rep(., times = 1),
         criterion = rep(c("Unique_Species", "Years_Fish_Data", "Lat","Long","Mean_temp","Mean_DO"), each = 4000) %>% rep(., times = 1)) %>% 
  mutate(label = str_c(predictor, " with ", criterion)) %>%
  select(-c(predictor:criterion))%>%
  # wrangle a bit just to make the y axis easier to understand
  mutate(name = factor(name, 
                       levels = c(str_c("docor", 1:6)),
                       labels = c("1. Median vs. # Unique Species",
                                  "2. Median vs. Years of Fish Data",
                                  "3. Median vs. Latitude",
                                  "4. Median vs. Longitude",
                                  "5. Median vs. Mean Temperature",
                                  "6. Median vs. Mean DO")))%>%
  # plot
  ggplot(aes(x = rho, y = name))+
  stat_halfeye(.width = .95, size = 5/4) +
  #scale_x_continuous(breaks = c(-1,1)) +
  labs(x = expression(rho),
       y = NULL) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))+
  geom_vline(xintercept = 0,linetype = "dashed")+
  theme_cowplot()+
  scale_y_discrete(limits=rev)+
  ggtitle("Individual")
print(inddog)

#pop
ggpairs(popdatado, columns = c(2:3,7,8,13:14,16),ggplot2::aes(color=Habitat_Broad)) 
ggpairs(popdatado, columns = c(2:3,7,12:14,16),ggplot2::aes(color=Habitat_Broadest)) 
cor.test(popdatado$median,popdatado$`Unique Species`,method="pearson")
cor.test(popdatado$median,popdatado$`Years of Fish Data`,method="pearson")
cor.test(popdatado$median,popdatado$`Years of Temp Data`,method="pearson")
cor.test(popdatado$median,popdatado$Lat,method="pearson")
cor.test(popdatado$median,popdatado$Long,method="pearson")
cor.test(popdatado$median,popdatado$mean_temp,method="pearson")
cor.test(popdatado$median,popdatado$mean_DO,method="pearson")
#standardize 
popdatado<-popdatado %>% 
  mutate(median_s = (median - mean(median)) / sd(median),
         Species_s = (Unique_Species - mean(Unique_Species)) / sd(Unique_Species),
         Years_Fish_Data_s = (Years_Fish_Data - mean(Years_Fish_Data)) / sd(Years_Fish_Data),
         Lat_s=(Lat - mean(Lat)) / sd(Lat),
         Long_s=(Long - mean(Long)) / sd(Long),
         mean_temp_s=(mean_temp - mean(mean_temp)) / sd(mean_temp),
         mean_DO_s=(mean_DO - mean(mean_DO)) / sd(mean_DO))
#bayesian correlations
popdocor1<-brm(data = popdatado, 
                 family = gaussian,
                 median_s~1+Species_s,
                 chains = 4, cores = 4, 
                 seed = 1)
p_direction(popdocor1)
print(popdocor1)

popdocor2<-update(popdocor1,formula=median_s~1+Years_Fish_Data_s,newdata=popdatado,seed=1)
print(popdocor2)
p_direction(popdocor2)

popdocor3<-update(popdocor1,formula=median_s~1+Lat_s,newdata=popdatado,seed=1)
print(popdocor3)
p_direction(popdocor3)

popdocor4<-update(popdocor1,formula=median_s~1+Long_s,newdata=popdatado,seed=1)
print(popdocor4)
p_direction(popdocor4)

popdocor5<-update(popdocor1,formula=median_s~1+mean_temp,newdata=popdatado,seed=1)
print(popdocor5)
p_direction(popdocor5)

popdocor6<-update(popdocor1,formula=median_s~1+mean_DO,newdata=popdatado,seed=1)
print(popdocor6)
p_direction(popdocor6)

#graph
popdog<-tibble(name = str_c("popdocor", 1:6)) %>% 
  mutate(fit = map(name, get)) %>% 
  mutate(rho = map(fit, get_rho)) %>% 
  unnest(rho) %>% 
  mutate(predictor = rep(c("median", "median", "median","median","median","median"), each = 4000) %>% rep(., times = 1),
         criterion = rep(c("Unique_Species", "Years_Fish_Data", "Lat","Long","Mean_temp","Mean_DO"), each = 4000) %>% rep(., times = 1)) %>% 
  mutate(label = str_c(predictor, " with ", criterion)) %>%
  select(-c(predictor:criterion))%>%
  # wrangle a bit just to make the y axis easier to understand
  mutate(name = factor(name, 
                       levels = c(str_c("popdocor", 1:6)),
                       labels = c("1. Median vs. # Unique Species",
                                  "2. Median vs. Years of Fish Data",
                                  "3. Median vs. Latitude",
                                  "4. Median vs. Longitude",
                                  "5. Median vs. Mean Temperature",
                                  "6. Median vs. Mean DO")))%>%
  # plot
  ggplot(aes(x = rho, y = name))+
  stat_halfeye(.width = .95, size = 5/4) +
  #scale_x_continuous(breaks = c(-1,1)) +
  labs(x = expression(rho),
       y = NULL) +
  xlim(c(-1,1))+
  scale_y_discrete(limits=rev)+
  theme_cowplot()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank())+
  geom_vline(xintercept = 0,linetype = "dashed")+
  ggtitle("Population")
print(popdog)
#div
ggpairs(divdatado, columns = c(2:3,7,12:14,16),ggplot2::aes(color=Habitat_Broad)) 
ggpairs(divdatado, columns = c(2:3,7,12:14,16),ggplot2::aes(color=Habitat_Broadest)) 
cor.test(divdatado$median,divdatado$`Unique Species`,method="pearson")
cor.test(divdatado$median,divdatado$`Years of Fish Data`,method="pearson")
cor.test(divdatado$median,divdatado$`Years of Temp Data`,method="pearson")
cor.test(divdatado$median,divdatado$Lat,method="pearson")
cor.test(divdatado$median,divdatado$Long,method="pearson")
cor.test(divdatado$median,divdatado$mean_temp,method="pearson")
cor.test(divdatado$median,divdatado$mean_DO,method="pearson")

divdatado<-divdatado %>% 
  mutate(median_s = (median - mean(median)) / sd(median),
         Species_s = (Unique_Species - mean(Unique_Species)) / sd(Unique_Species),
         Years_Fish_Data_s = (Years_Fish_Data - mean(Years_Fish_Data)) / sd(Years_Fish_Data),
         Lat_s=(Lat - mean(Lat)) / sd(Lat),
         Long_s=(Long - mean(Long)) / sd(Long),
         mean_temp_s=(mean_temp - mean(mean_temp)) / sd(mean_temp),
         mean_DO_s=(mean_DO - mean(mean_DO)) / sd(mean_DO))
#bayesian correlations
divdocor1<-brm(data = divdatado, 
                 family = gaussian,
                 median_s~1+Species_s,
                 chains = 4, cores = 4, 
                 seed = 1)
p_direction(divdocor1)
print(divdocor1)

divdocor2<-update(divdocor1,formula=median_s~1+Years_Fish_Data_s,newdata=divdatado,seed=1)
print(divdocor2)
p_direction(divdocor2)

divdocor3<-update(divdocor1,formula=median_s~1+Lat_s,newdata=divdatado,seed=1)
print(divdocor3)
p_direction(divdocor3)

divdocor4<-update(divdocor1,formula=median_s~1+Long_s,newdata=divdatado,seed=1)
print(divdocor4)
p_direction(divdocor4)

divdocor5<-update(divdocor1,formula=median_s~1+mean_temp,newdata=divdatado,seed=1)
print(divdocor5)
p_direction(divdocor5)

divdocor6<-update(divdocor1,formula=median_s~1+mean_DO,newdata=divdatado,seed=1)
print(divdocor6)
p_direction(divdocor6)

divdog<-tibble(name = str_c("divdocor", 1:6)) %>% 
  mutate(fit = map(name, get)) %>% 
  mutate(rho = map(fit, get_rho)) %>% 
  unnest(rho) %>% 
  mutate(predictor = rep(c("median", "median", "median","median","median","median"), each = 4000) %>% rep(., times = 1),
         criterion = rep(c("Unique_Species", "Years_Fish_Data", "Lat","Long","Mean_temp","Mean_DO"), each = 4000) %>% rep(., times = 1)) %>% 
  mutate(label = str_c(predictor, " with ", criterion)) %>%
  select(-c(predictor:criterion))%>%
  # wrangle a bit just to make the y axis easier to understand
  mutate(name = factor(name, 
                       levels = c(str_c("divdocor", 1:6)),
                       labels = c("1. Median vs. # Unique Species",
                                  "2. Median vs. Years of Fish Data",
                                  "3. Median vs. Latitude",
                                  "4. Median vs. Longitude",
                                  "5. Median vs. Mean Temperature",
                                  "6. Median vs. Mean DO")))%>%
  # plot
  ggplot(aes(x = rho, y = name))+
  stat_halfeye(.width = .95, size = 5/4) +
  #scale_x_continuous(breaks = c(-1,1)) +
  scale_y_discrete(limits=rev)+
  labs(x = expression(rho),
       y = NULL) +
  theme_cowplot()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank())+
  geom_vline(xintercept = 0,linetype = "dashed")+
  ggtitle("Community")
print(divdog)

#Interaction correlations

##DO Correlations
#standardize data
inddatado<-inddatado %>% 
  mutate(median_s = (median - mean(median)) / sd(median),
         Species_s = (Unique_Species - mean(Unique_Species)) / sd(Unique_Species),
         Years_Fish_Data_s = (Years_Fish_Data - mean(Years_Fish_Data)) / sd(Years_Fish_Data),
         Lat_s=(Lat - mean(Lat)) / sd(Lat),
         Long_s=(Long - mean(Long)) / sd(Long),
         mean_temp_s=(mean_temp - mean(mean_temp)) / sd(mean_temp),
         mean_DO_s=(mean_DO - mean(mean_DO)) / sd(mean_DO))

docor1<-brm(data = inddatado, 
            family = gaussian,
            median_s~1+Species_s,
            chains = 4, cores = 4, 
            seed = 1)
p_direction(docor1)
#should be same correlation as pearson
print(docor1)

docor2<-update(docor1,formula=median_s~1+Years_Fish_Data_s,newdata=inddatado,seed=1)
print(docor2)
cor.test(inddatado$median,inddatado$Years_Fish_Data,method="pearson")
p_direction(docor2)

docor3<-update(docor1,formula=median_s~1+Lat_s,newdata=inddatado,seed=1)
print(docor3)
p_direction(docor3)

docor4<-update(docor1,formula=median_s~1+Long_s,newdata=inddatado,seed=1)
print(docor4)
p_direction(docor4)

docor5<-update(docor1,formula=median_s~1+mean_temp,newdata=inddatado,seed=1)
print(docor5)
p_direction(docor5)

docor6<-update(docor1,formula=median_s~1+mean_DO,newdata=inddatado,seed=1)
print(docor6)
p_direction(docor6)

# collect the posteriors from the univariate models and graph
inddog<-tibble(name = str_c("docor", 1:6)) %>% 
  mutate(fit = map(name, get)) %>% 
  mutate(rho = map(fit, get_rho)) %>% 
  unnest(rho) %>% 
  mutate(predictor = rep(c("median", "median", "median","median","median","median"), each = 4000) %>% rep(., times = 1),
         criterion = rep(c("Unique_Species", "Years_Fish_Data", "Lat","Long","Mean_temp","Mean_DO"), each = 4000) %>% rep(., times = 1)) %>% 
  mutate(label = str_c(predictor, " with ", criterion)) %>%
  select(-c(predictor:criterion))%>%
  # wrangle a bit just to make the y axis easier to understand
  mutate(name = factor(name, 
                       levels = c(str_c("docor", 1:6)),
                       labels = c("1. Median vs. # Unique Species",
                                  "2. Median vs. Years of Fish Data",
                                  "3. Median vs. Latitude",
                                  "4. Median vs. Longitude",
                                  "5. Median vs. Mean Temperature",
                                  "6. Median vs. Mean DO")))%>%
  # plot
  ggplot(aes(x = rho, y = name))+
  stat_halfeye(.width = .95, size = 5/4) +
  #scale_x_continuous(breaks = c(-1,1)) +
  labs(x = expression(rho),
       y = NULL) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))+
  geom_vline(xintercept = 0,linetype = "dashed")+
  theme_cowplot()+
  scale_y_discrete(limits=rev)+
  ggtitle("Individual")
print(inddog)

#pop
ggpairs(popdatado, columns = c(2:3,7,8,13:14,16),ggplot2::aes(color=Habitat_Broad)) 
ggpairs(popdatado, columns = c(2:3,7,12:14,16),ggplot2::aes(color=Habitat_Broadest)) 
cor.test(popdatado$median,popdatado$`Unique Species`,method="pearson")
cor.test(popdatado$median,popdatado$`Years of Fish Data`,method="pearson")
cor.test(popdatado$median,popdatado$`Years of Temp Data`,method="pearson")
cor.test(popdatado$median,popdatado$Lat,method="pearson")
cor.test(popdatado$median,popdatado$Long,method="pearson")
cor.test(popdatado$median,popdatado$mean_temp,method="pearson")
cor.test(popdatado$median,popdatado$mean_DO,method="pearson")
#standardize 
popdatado<-popdatado %>% 
  mutate(median_s = (median - mean(median)) / sd(median),
         Species_s = (Unique_Species - mean(Unique_Species)) / sd(Unique_Species),
         Years_Fish_Data_s = (Years_Fish_Data - mean(Years_Fish_Data)) / sd(Years_Fish_Data),
         Lat_s=(Lat - mean(Lat)) / sd(Lat),
         Long_s=(Long - mean(Long)) / sd(Long),
         mean_temp_s=(mean_temp - mean(mean_temp)) / sd(mean_temp),
         mean_DO_s=(mean_DO - mean(mean_DO)) / sd(mean_DO))
#bayesian correlations
popdocor1<-brm(data = popdatado, 
               family = gaussian,
               median_s~1+Species_s,
               chains = 4, cores = 4, 
               seed = 1)
p_direction(popdocor1)
print(popdocor1)

popdocor2<-update(popdocor1,formula=median_s~1+Years_Fish_Data_s,newdata=popdatado,seed=1)
print(popdocor2)
p_direction(popdocor2)

popdocor3<-update(popdocor1,formula=median_s~1+Lat_s,newdata=popdatado,seed=1)
print(popdocor3)
p_direction(popdocor3)

popdocor4<-update(popdocor1,formula=median_s~1+Long_s,newdata=popdatado,seed=1)
print(popdocor4)
p_direction(popdocor4)

popdocor5<-update(popdocor1,formula=median_s~1+mean_temp,newdata=popdatado,seed=1)
print(popdocor5)
p_direction(popdocor5)

popdocor6<-update(popdocor1,formula=median_s~1+mean_DO,newdata=popdatado,seed=1)
print(popdocor6)
p_direction(popdocor6)

#graph
popdog<-tibble(name = str_c("popdocor", 1:6)) %>% 
  mutate(fit = map(name, get)) %>% 
  mutate(rho = map(fit, get_rho)) %>% 
  unnest(rho) %>% 
  mutate(predictor = rep(c("median", "median", "median","median","median","median"), each = 4000) %>% rep(., times = 1),
         criterion = rep(c("Unique_Species", "Years_Fish_Data", "Lat","Long","Mean_temp","Mean_DO"), each = 4000) %>% rep(., times = 1)) %>% 
  mutate(label = str_c(predictor, " with ", criterion)) %>%
  select(-c(predictor:criterion))%>%
  # wrangle a bit just to make the y axis easier to understand
  mutate(name = factor(name, 
                       levels = c(str_c("popdocor", 1:6)),
                       labels = c("1. Median vs. # Unique Species",
                                  "2. Median vs. Years of Fish Data",
                                  "3. Median vs. Latitude",
                                  "4. Median vs. Longitude",
                                  "5. Median vs. Mean Temperature",
                                  "6. Median vs. Mean DO")))%>%
  # plot
  ggplot(aes(x = rho, y = name))+
  stat_halfeye(.width = .95, size = 5/4) +
  #scale_x_continuous(breaks = c(-1,1)) +
  labs(x = expression(rho),
       y = NULL) +
  xlim(c(-1,1))+
  scale_y_discrete(limits=rev)+
  theme_cowplot()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank())+
  geom_vline(xintercept = 0,linetype = "dashed")+
  ggtitle("Population")
print(popdog)
#div
ggpairs(divdatado, columns = c(2:3,7,12:14,16),ggplot2::aes(color=Habitat_Broad)) 
ggpairs(divdatado, columns = c(2:3,7,12:14,16),ggplot2::aes(color=Habitat_Broadest)) 
cor.test(divdatado$median,divdatado$`Unique Species`,method="pearson")
cor.test(divdatado$median,divdatado$`Years of Fish Data`,method="pearson")
cor.test(divdatado$median,divdatado$`Years of Temp Data`,method="pearson")
cor.test(divdatado$median,divdatado$Lat,method="pearson")
cor.test(divdatado$median,divdatado$Long,method="pearson")
cor.test(divdatado$median,divdatado$mean_temp,method="pearson")
cor.test(divdatado$median,divdatado$mean_DO,method="pearson")

divdatado<-divdatado %>% 
  mutate(median_s = (median - mean(median)) / sd(median),
         Species_s = (Unique_Species - mean(Unique_Species)) / sd(Unique_Species),
         Years_Fish_Data_s = (Years_Fish_Data - mean(Years_Fish_Data)) / sd(Years_Fish_Data),
         Lat_s=(Lat - mean(Lat)) / sd(Lat),
         Long_s=(Long - mean(Long)) / sd(Long),
         mean_temp_s=(mean_temp - mean(mean_temp)) / sd(mean_temp),
         mean_DO_s=(mean_DO - mean(mean_DO)) / sd(mean_DO))
#bayesian correlations
divdocor1<-brm(data = divdatado, 
               family = gaussian,
               median_s~1+Species_s,
               chains = 4, cores = 4, 
               seed = 1)
p_direction(divdocor1)
print(divdocor1)

divdocor2<-update(divdocor1,formula=median_s~1+Years_Fish_Data_s,newdata=divdatado,seed=1)
print(divdocor2)
p_direction(divdocor2)

divdocor3<-update(divdocor1,formula=median_s~1+Lat_s,newdata=divdatado,seed=1)
print(divdocor3)
p_direction(divdocor3)

divdocor4<-update(divdocor1,formula=median_s~1+Long_s,newdata=divdatado,seed=1)
print(divdocor4)
p_direction(divdocor4)

divdocor5<-update(divdocor1,formula=median_s~1+mean_temp,newdata=divdatado,seed=1)
print(divdocor5)
p_direction(divdocor5)

divdocor6<-update(divdocor1,formula=median_s~1+mean_DO,newdata=divdatado,seed=1)
print(divdocor6)
p_direction(divdocor6)

divdog<-tibble(name = str_c("divdocor", 1:6)) %>% 
  mutate(fit = map(name, get)) %>% 
  mutate(rho = map(fit, get_rho)) %>% 
  unnest(rho) %>% 
  mutate(predictor = rep(c("median", "median", "median","median","median","median"), each = 4000) %>% rep(., times = 1),
         criterion = rep(c("Unique_Species", "Years_Fish_Data", "Lat","Long","Mean_temp","Mean_DO"), each = 4000) %>% rep(., times = 1)) %>% 
  mutate(label = str_c(predictor, " with ", criterion)) %>%
  select(-c(predictor:criterion))%>%
  # wrangle a bit just to make the y axis easier to understand
  mutate(name = factor(name, 
                       levels = c(str_c("divdocor", 1:6)),
                       labels = c("1. Median vs. # Unique Species",
                                  "2. Median vs. Years of Fish Data",
                                  "3. Median vs. Latitude",
                                  "4. Median vs. Longitude",
                                  "5. Median vs. Mean Temperature",
                                  "6. Median vs. Mean DO")))%>%
  # plot
  ggplot(aes(x = rho, y = name))+
  stat_halfeye(.width = .95, size = 5/4) +
  #scale_x_continuous(breaks = c(-1,1)) +
  scale_y_discrete(limits=rev)+
  labs(x = expression(rho),
       y = NULL) +
  theme_cowplot()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank())+
  geom_vline(xintercept = 0,linetype = "dashed")+
  ggtitle("Community")
print(divdog)

# Combine Graphs ----------------------------------------------------------

tempcorg<-ggarrange(indtempg,poptempg,divtempg,nrow=1,widths = c(1.5,1,1))
print(tempcorg)

docorg<-ggarrange(inddog,popdog,divdog,nrow=1,widths = c(1.5,1,1))
print(docorg)


###Make Graphs---------------------
sl3<-subset(sl,.variable!="beta_0sp")
#sl2<-subset(sl2,Habitat_Broad!="Estuary")
#scatterplot function

scatter_funsl = function(x, y) {
  ggplot(sl3, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_cowplot()+
    ggtitle("Individual Site Level")+
    facet_wrap(~factor(.variable,levels=c("beta_temp_si","beta_DO_si")),scales="free_y")
  }

#select explanatory and response variables
explind = names(sl3)[c(7:8,12,13,18,19)]
explind = set_names(explind)
respind = names(sl3)[c(3)]
respind = set_names(respind)

all_plots_ind = purrr::map(respind, function(respind) {
  purrr::map(explind, function(explind) {
    scatter_funsl(x = explind, y = respind)
  })
})
print(all_plots_ind)
allplotsind<-cowplot::plot_grid(nrow=3,plotlist=all_plots_ind[[1]],labels=c("A","B","C","D","E","F"))
nflplotR::ggpreview(allplotsind,height=10,width=16)

#scatterplot function pop
#cpue2b<-subset(cpue2,.variable!=c("beta_0sp"))
#cpue2b<-subset(cpue2b,Habitat_Broad!="Estuary")
scatter_funcpue = function(x, y) {
  ggplot(cpue, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_cowplot()+
    ggtitle("Population Site Level")+
    facet_wrap(~factor(.variable,levels=c("beta_temp_si","beta_DO_si")),scales="free_y")
}

#select explanatory and response variables
explpop = names(cpue)[c(2,3,7,8,13,14)]
explpop = set_names(explpop)
resppop = names(cpue)[c(16)]
resppop = set_names(resppop)

all_plots_pop = purrr::map(resppop, function(resppop) {
  purrr::map(explpop, function(explpop) {
    scatter_funcpue(x = explpop, y = resppop)
  })
})
print(all_plots_pop)
allplotspop<-cowplot::plot_grid(nrow=3,plotlist=all_plots_pop[[1]],labels=c("A","B","C","D","E","F"))
nflplotR::ggpreview(allplotspop,height=10,width=16)


#scatterplot function diversity
#div2<-subset(div,Habitat_Broad!="Estuary")
scatter_fundiv = function(x, y) {
  ggplot(div, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_cowplot()+
    ggtitle("Community Site Level")+
    facet_wrap(~factor(.variable,levels=c("beta_temp","beta_DO")),scales="free_y")
}

#select explanatory and response variables
explcom = names(div)[c(2,3,7,8,13,14)]
explcom = set_names(explcom)
respcom = names(div)[c(16)]
respcom = set_names(respcom)

all_plots_com = purrr::map(respcom, function(respcom) {
  purrr::map(explcom, function(explcom) {
    scatter_fundiv(x = explcom, y = respcom)
  })
})
print(all_plots_com)
allplotscom<-cowplot::plot_grid(nrow=3,plotlist=all_plots_com[[1]],labels=c("A","B","C","D","E","F"))
nflplotR::ggpreview(allplotscom,height=10,width=16)
