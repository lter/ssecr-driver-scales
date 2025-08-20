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
                 vegan,readxl,GGally)

source(file = file.path("scripts",
                        "functions.R"))

source(file = file.path("scripts",
                        "viz_ideas.R"))

## load Site data ---------------------

#internal note, make sure to download data from Google drive first
#load info on sites that were included in dataset
sitedata <- read_csv(file = file.path("data",
                                         "final_harmonized_summary.csv"))
#load final harmonized data to extract do and temp average for site
envdata<- read_csv(file = file.path("data",
                                    "final_harmonized.csv"))

#load coordinates for each site
coord<-read_xlsx(path = file.path("data",
                          "allsites_coords.xlsx"))

#combine site and coordinate data

sitedata<-merge(sitedata,coord, by="MIDSITE")
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
                                      "mean_div_effects_df.csv"))

cpue <- read_csv(file = file.path("data",
                                 "mean_pop_effects_df.csv"))

sl <- read_csv(file = file.path("data",
                                  "mean_ind_effects_df.csv"))

#remove prefixes from site names to be able to merge

patterns_to_remove <- c("LTER_","NEON_","IEP_")
combined_pattern <- paste(patterns_to_remove, collapse = "|")
sitedata$MIDSITE<-str_remove_all(sitedata$MIDSITE,combined_pattern)

#get names matching
sitedata <- sitedata %>% 
  rename(site = MIDSITE)

#merge to get final datasets for graphing

cpue<-merge(sitedata,cpue,by="site")
cpue<-subset(cpue,site!="MCR_Backreef"& site!="MCR_FringingReef" &site!="MCR_Forereef")

div<-merge(sitedata,div,by="site")
div<-subset(div,site!="MCR_Backreef"& site!="MCR_FringingReef" &site!="MCR_Forereef")

sl<-merge(sitedata,sl,by="site")
#remove cpue parameter estimates for graphing
sl<-subset(sl,var!="CPUE")
#try without morea since its lat is so different
sl<-subset(sl,site!="MCR_Backreef"& site!="MCR_FringingReef" &site!="MCR_Forereef")

###Make Correlelograms---------------------
inddatatemp<-subset(sl,var=="temp")
inddatado<-subset(sl,var=="DO")

popdatatemp<-subset(cpue,var=="temp")
popdatado<-subset(cpue,var=="DO")

divdatatemp<-subset(div,var=="temp")
divdatado<-subset(div,var=="DO")


#inididual
ggpairs(inddatatemp, columns = c(6:10,14)) 
ggpairs(inddatado, columns = c(6:10,14)) 

#pop
ggpairs(popdatatemp, columns = c(6:10,13)) 
ggpairs(popdatado, columns = c(6:10,13)) 

#div
ggpairs(divdatatemp, columns = c(6:10,13)) 
ggpairs(divdatado, columns = c(6:10,13)) 

###Make Graphs---------------------

#scatterplot function
scatter_funsl = function(x, y) {
  ggplot(sl, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "grey74") +
    theme_cowplot()+
    ggtitle("Individual Site Level")+
    facet_grid(~var)
}

#select explanatory and response variables
explind = names(sl)[6:10]
explind = set_names(explind)
respind = names(sl)[c(14)]
respind = set_names(respind)

all_plots_ind = purrr::map(respind, function(respind) {
  purrr::map(explind, function(explind) {
    scatter_funsl(x = explind, y = respind)
  })
})
print(all_plots_ind)