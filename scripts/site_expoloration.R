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
                                         "final_harmonized_midsites_summary.csv"))
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
                                 "pop.csv"))

sl <- read_csv(file = file.path("data",
                                  "ind.csv"))

#remove prefixes from site names to be able to merge

#patterns_to_remove <- c("LTER_","NEON_","IEP_")
#combined_pattern <- paste(patterns_to_remove, collapse = "|")
#sitedata$MIDSITE<-str_remove_all(sitedata$MIDSITE,combined_pattern)

#get names matching
sitedata <- sitedata %>% 
  rename(si = MIDSITE)


#merge to get final datasets for graphing

cpue<-merge(sitedata,cpue,by="si")
cpue<-subset(cpue,si!="MCR_Backreef"& si!="MCR_FringingReef" &si!="MCR_Forereef")

div<-merge(sitedata,div,by="si")
div<-subset(div,si!="MCR_Backreef"& si!="MCR_FringingReef" &si!="MCR_Forereef")

sl<-left_join(sl,sitedata,by="si")

#try without morea since its lat is so different
sl<-subset(sl,si!="MCR_Backreef"& si!="MCR_FringingReef" &si!="MCR_Forereef")

###Make Correlelograms---------------------
inddatatemp<-subset(sl,.variable=="beta_temp")
inddatado<-subset(sl,.variable=="beta_DO")

popdatatemp<-subset(cpue,.variable=="beta_temp")
popdatado<-subset(cpue,.variable=="beta_DO")

divdatatemp<-subset(div,.variable=="beta_temp")
divdatado<-subset(div,.variable=="beta_DO")


#indvidual
ggpairs(inddatatemp, columns = c(13,18:20,4),ggplot2::aes(color=Habitat_Broad)) 
ggpairs(inddatado, columns = c(13,18:20,4),ggplot2::aes(color=Habitat_Broad)) 

#pop
ggpairs(popdatatemp, columns = c(7,12:13,16),ggplot2::aes(color=Habitat_Broad)) 
ggpairs(popdatado, columns = c(7,12:13,16),ggplot2::aes(color=Habitat_Broad)) 

#div
ggpairs(divdatatemp, columns = c(7,12:13,15),ggplot2::aes(color=Habitat_Broad)) 
ggpairs(divdatado, columns = c(7,12:13,15),ggplot2::aes(color=Habitat_Broad)) 

###Make Graphs---------------------
sl2<-subset(sl,.variable==c("beta_temp","beta_DO"))
sl2<-subset(sl2,Habitat_Broad!="Estuary")
#scatterplot function

scatter_funsl = function(x, y) {
  ggplot(sl2, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point(aes(color=si)) +
    geom_smooth(method = "lm", se = TRUE) +
    theme_cowplot()+
    ggtitle("Individual Site Level")+
    facet_grid(~.variable+Habitat_Broad)
}

#select explanatory and response variables
explind = names(sl2)[c(13:14,18:20)]
explind = set_names(explind)
respind = names(sl2)[c(4)]
respind = set_names(respind)

all_plots_ind = purrr::map(respind, function(respind) {
  purrr::map(explind, function(explind) {
    scatter_funsl(x = explind, y = respind)
  })
})
print(all_plots_ind)


#scatterplot function pop
cpue2<-subset(cpue,.variable==c("beta_temp","beta_DO"))
cpue2<-subset(cpue2,Habitat_Broad!="Estuary")
scatter_funcpue = function(x, y) {
  ggplot(cpue2, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point(aes(color=si)) +
    geom_smooth(method = "lm", se = TRUE, color = "grey74") +
    theme_cowplot()+
    ggtitle("CPUE Site Level")+
    facet_grid(~.variable+Habitat_Broad)
}

#select explanatory and response variables
explpop = names(cpue)[c(7,12:13)]
explpop = set_names(explpop)
resppop = names(cpue)[c(16)]
resppop = set_names(resppop)

all_plots_pop = purrr::map(resppop, function(resppop) {
  purrr::map(explpop, function(explpop) {
    scatter_funcpue(x = explpop, y = resppop)
  })
})
print(all_plots_pop)



#scatterplot function diversity
div2<-subset(div,Habitat_Broad!="Estuary")
scatter_fundiv = function(x, y) {
  ggplot(div2, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point(aes(color=si)) +
    geom_smooth(method = "lm", se = TRUE, color = "grey74") +
    theme_cowplot()+
    ggtitle("Diversity Site Level")+
    facet_grid(~.variable)
}

#select explanatory and response variables
explcom = names(div)[c(7,12:13)]
explcom = set_names(explcom)
respcom = names(div)[c(15)]
respcom = set_names(respcom)

all_plots_com = purrr::map(respcom, function(respcom) {
  purrr::map(explcom, function(explcom) {
    scatter_fundiv(x = explcom, y = respcom)
  })
})
print(all_plots_com)
