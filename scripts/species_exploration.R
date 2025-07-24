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
#add site metadata
sitedata <- read_xlsx(path = file.path("data",
                                          "Site_metadata.xlsx"))


#individual and population data are from model outputs
inddata <- read_rds(file = file.path("data",
                                         "species_ind_effects_df.RDS"))

popdata <- read_rds(file = file.path("data",
                                     "species_pop_effects_df.RDS"))

## combine data---------------------
popdata <- popdata %>% 
  rename(SCI_NAME = s)
inddata <- inddata %>% 
  rename(SCI_NAME = s)

#get overall site name for pop data and ind data
popdata$Site<-str_sub(popdata$site,1,3)
inddata$Site<-str_sub(inddata$site,1,3)
sitedata$Site<-str_sub(sitedata$Site,1,3)

popdata<-merge(popdata,speciesdata,by="SCI_NAME")
popdata<-merge(popdata,sitedata,by="Site")
inddata<-merge(inddata,speciesdata,by="SCI_NAME")
inddata<-merge(inddata,sitedata,by="Site")




##Make Graphs for Individual Size DO and Temperature ---------------------

#scatterplot function
scatter_funind = function(x, y) {
  ggplot(inddata, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "grey74") +
    theme_cowplot()+
    ggtitle("Individual Unfiltered")+
    facet_grid(~var+Habitat)
}

#select explanatory and response variables
explind = names(inddata)[11:17]
explind = set_names(explind)
respind = names(inddata)[c(5)]
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
    ggtitle("Population Unfiltered")+
    facet_grid(~var+Habitat)
}

#select explanatory and response variables
explpop = names(popdata)[11:17]
explpop = set_names(explpop)
resppop = names(popdata)[c(5)]
resppop = set_names(resppop)

all_plots_pop = purrr::map(resppop, function(resppop) {
  purrr::map(explpop, function(explpop) {
    scatter_funpop(x = explpop, y = resppop)
  })
})
print(all_plots_pop)

## Same Graphs but with filtered data to PD>0.8 ---------------------

inddatafiltered<-subset(inddata,PD>=0.8)
popdatafiltered<-subset(popdata,PD>=0.8)



##Make Graphs for Individual Size DO and Temperature ---------------------
#scatterplot function
scatter_funindfiltered = function(x, y) {
  ggplot(inddatafiltered, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "lightblue") +
    theme_cowplot()+
    ggtitle("Individual Filtered")+
    facet_grid(~var+Habitat)
}

#select explanatory and response variables
explind = names(inddatafiltered)[11:17]
explind = set_names(explind)
respind = names(inddatafiltered)[5]
respind = set_names(respind)

all_plots_ind_filtered = purrr::map(respind, function(respind) {
  purrr::map(explind, function(explind) {
    scatter_funindfiltered(x = explind, y = respind)
  })
})
print(all_plots_ind_filtered)



##Make Graphs for Population Size DO and Temperature ---------------------
#scatterplot function
scatter_funpopfiltered = function(x, y) {
  ggplot(popdatafiltered, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "lightblue") +
    theme_cowplot()+
    ggtitle("Population Filtered")+
    facet_grid(~var+Habitat)
}

#select explanatory and response variables
explpop = names(popdata)[11:17]
explpop = set_names(explpop)
resppop = names(popdata)[5]
resppop = set_names(resppop)

all_plots_pop_filtered = purrr::map(resppop, function(resppop) {
  purrr::map(explpop, function(explpop) {
    scatter_funpopfiltered(x = explpop, y = resppop)
  })
})
print(all_plots_pop_filtered)


##Compile all graphs ---------------------
#unfiltered
pdf("all_scatterplots_unfiltered_habitat.pdf")
#Length
all_plots_ind
#Population
all_plots_pop
dev.off()


#filtered
pdf("all_scatterplots_filtered_habitat.pdf")
all_plots_ind_filtered
all_plots_pop_filtered
dev.off()