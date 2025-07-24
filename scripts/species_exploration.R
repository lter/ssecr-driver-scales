#_________________________________
#LTER Species Exploration
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
                 vegan,readxl)

source(file = file.path("scripts",
                        "functions.R"))

source(file = file.path("scripts",
                        "viz_ideas.R"))

## load data ---------------------

#internal note, make sure to download data from Google drive first

#species data were collated from Fish Base between June 25th and June 27th 2025
speciesdata <- read_xlsx(path = file.path("data",
                                         "Species_attributes.xlsx"))

#individual and population data are from model outputs
inddata <- read.csv(file = file.path("data",
                                         "species_ind_effects.csv"))

popdata <- read.csv(file = file.path("data",
                                     "species_pop_effects.csv"))

## combine data---------------------
popdata <- popdata %>% 
  rename(SCI_NAME = s)
inddata <- inddata %>% 
  rename(SCI_NAME = s)


popdata<-merge(popdata,speciesdata,by="SCI_NAME")
inddata<-merge(inddata,speciesdata,by="SCI_NAME")




##Make Graphs for Individual Size DO and Temperature ---------------------

#remove one massive outlier for do (-400 median)
inddata2<-subset(inddata,SCI_NAME!="Paralichthys dentatus")
#scatterplot function
scatter_funind = function(x, y) {
  ggplot(inddata2, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "grey74") +
    theme_cowplot()+
    ggtitle("Individual Unfiltered")
}

#select explanatory and response variables
explind = names(inddata2)[14:20]
explind = set_names(explind)
respind = names(inddata2)[c(4,8)]
respind = set_names(respind)

all_plots_ind = purrr::map(respind, function(respind) {
  purrr::map(explind, function(explind) {
    scatter_funind(x = explind, y = respind)
  })
})
print(all_plots_ind)

##Make Graphs for Population Size DO and Temperature ---------------------

#scatterplot function
scatter_funpop = function(x, y) {
  ggplot(popdata, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "grey74") +
    theme_cowplot()+
    ggtitle("Population Unfiltered")
}

#select explanatory and response variables
explpop = names(popdata)[14:20]
explpop = set_names(explpop)
resppop = names(popdata)[c(4,8)]
resppop = set_names(resppop)

all_plots_pop = purrr::map(resppop, function(resppop) {
  purrr::map(explpop, function(explpop) {
    scatter_funpop(x = explpop, y = resppop)
  })
})
print(all_plots_pop)

## Same Graphs but with filtered data to PD>0.8 ---------------------

inddatafiltereddo<-subset(inddata,PD_DO>=0.8)
inddatafilteredtemp<-subset(inddata,PD_temp>=0.8)
popdatafiltereddo<-subset(popdata,PD_DO>=0.8)
popdatafilteredtemp<-subset(popdata,PD_temp>=0.8)


##Make Graphs for Individual Size DO and Temperature ---------------------
#scatterplot function
scatter_funindfiltereddo = function(x, y) {
  ggplot(inddatafiltereddo, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "lightblue") +
    theme_cowplot()+
    ggtitle("Individual Filtered")
}

#select explanatory and response variables
explind = names(inddata)[14:20]
explind = set_names(explind)
respinddo = names(inddata)[4]
respinddo = set_names(respinddo)

all_plots_ind_filtered_do = purrr::map(respinddo, function(respinddo) {
  purrr::map(explind, function(explind) {
    scatter_funindfiltereddo(x = explind, y = respinddo)
  })
})
print(all_plots_ind_filtered_do)

#scatterplot function
scatter_funindfilteredtemp = function(x, y) {
  ggplot(inddatafilteredtemp, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "lightblue") +
    theme_cowplot()+
    ggtitle("Individual Filtered")
}

#select explanatory and response variables
explind = names(inddata)[14:20]
explind = set_names(explind)
respindtemp = names(inddata)[8]
respindtemp = set_names(respindtemp)

all_plots_ind_filtered_temp = purrr::map(respindtemp, function(respindtemp) {
  purrr::map(explind, function(explind) {
    scatter_funindfilteredtemp(x = explind, y = respindtemp)
  })
})
print(all_plots_ind_filtered_temp)

##Make Graphs for Population Size DO and Temperature ---------------------
#scatterplot function
scatter_funpopfiltereddo = function(x, y) {
  ggplot(popdatafiltereddo, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "lightblue") +
    theme_cowplot()+
    ggtitle("Population Filtered")
}

#select explanatory and response variables
explpop = names(popdata)[14:20]
explpop = set_names(explpop)
resppopdo = names(popdata)[4]
resppopdo = set_names(resppopdo)

all_plots_pop_filtered_do = purrr::map(resppopdo, function(resppopdo) {
  purrr::map(explpop, function(explpop) {
    scatter_funpopfiltereddo(x = explpop, y = resppopdo)
  })
})
print(all_plots_pop_filtered_do)

#scatterplot function
scatter_funpopfilteredtemp = function(x, y) {
  ggplot(popdatafilteredtemp, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "lightblue") +
    theme_cowplot()+
    ggtitle("Population Filtered")
}

#select explanatory and response variables
explpop = names(popdata)[14:20]
explpop = set_names(explpop)
resppoptemp = names(popdata)[8]
resppoptemp = set_names(resppoptemp)

all_plots_pop_filtered_temp = purrr::map(resppoptemp, function(resppoptemp) {
  purrr::map(explpop, function(explpop) {
    scatter_funpopfilteredtemp(x = explpop, y = resppoptemp)
  })
})
print(all_plots_pop_filtered_temp)

##Compile all graphs ---------------------
#unfiltered
pdf("all_scatterplots_unfiltered.pdf")
#Length
all_plots_ind
#Population
all_plots_pop
dev.off()


#filtered
pdf("all_scatterplots_filtered.pdf")
all_plots_ind_filtered_do
all_plots_ind_filtered_temp
all_plots_pop_filtered_do
all_plots_pop_filtered_temp
dev.off()