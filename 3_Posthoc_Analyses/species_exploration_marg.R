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
                 vegan,readxl,GGally,brms,bayestestR)

source(file = file.path("scripts",
                        "functions.R"))

source(file = file.path("scripts",
                        "viz_ideas.R"))

## load data ---------------------

#internal note, make sure to download data from Google drive first

#species data were collated from Fish Base between June 25th and June 27th and again on September 22nd 2025
speciesdata <- read_xlsx(path = file.path("data",
                                   "Species_attributes_V2.xlsx"))


#individual and population data are from model outputs


inddata1<-read_csv(file=file.path("data",
                             "marg_sp_ind.csv"))


popdata1 <- read_csv(file = file.path("data",
                                    "marg_sp_pop.csv"))

#nonmarginalized to get habitat data
inddatahabitat <- read_csv(file = file.path("data",
                                      "ind.csv"))
#coords
coord<-read_xlsx(path = file.path("data",
                                  "allsites_coords.xlsx"))
coord$...8<-NULL


#site data
sitedata <- read_csv(file = file.path("data",
                                      "final_harmonized_midsites_summary.csv"))

#combine site and coordinate data

sitedata<-merge(sitedata,coord, by="MIDSITE")

#merge habitat and individual data to get fish and habitat togther

inddatahabitat <- inddatahabitat %>% 
  rename(MIDSITE = si,
         SCI_NAME = sp)

fishhabitat<-merge(sitedata,inddatahabitat,by="MIDSITE")
fishhabitat<- fishhabitat%>%select(c("Habitat_Broad","SCI_NAME","MIDSITE","Habitat","Habitat_Broadest"))

fishhabitat2<- fishhabitat%>%select(c("Habitat_Broadest","SCI_NAME"))
fishhabitat2<-unique(fishhabitat2)

numbermult<-fishhabitat2%>%group_by(SCI_NAME)%>% filter(n()>1)

#8 species in multiple habitats
length(unique(numbermult$SCI_NAME))
aggregate(Habitat_Broadest~SCI_NAME,data=numbermult,FUN=length)

fishhabitat<-fishhabitat%>%select(c("SCI_NAME","Habitat_Broadest"))
fishhabitat<-unique(fishhabitat)

#double checked classification based on fish base and occurrence data on gbif
# if possible to occur in both, listed the habitat it primarily occurred in
#ameirus melas= freshwater
#citharicthys stigmaeus=marine
#cyprinus carpio, tolerant of brackish water, but freshwater primarily
#ictalurus punctatus, found in brackish water, but freshwater primarily
#lepomis cyanellus: freshwater
#lepomis macrochirus: freshwater
#pimephales promelas: freshwater
#pomoxis nigromaculatus: freshwater

fishhabitatfinal<-fishhabitat%>%mutate(Habitat_Broadest=
                                  case_when(SCI_NAME=="Ameiurus melas"~"Freshwater",
                                            SCI_NAME=="Citharichthys stigmaeus"~"Marine",
                                            SCI_NAME=="Cyprinus carpio"~"Freshwater",
                                            SCI_NAME=="Ictalurus punctatus"~"Freshwater",
                                            SCI_NAME=="Lepomis cyanellus"~"Freshwater",
                                            SCI_NAME=="Lepomis macrochirus"~"Freshwater",
                                            SCI_NAME=="Pimephales promelas"~"Freshwater",
                                            SCI_NAME=="Pomoxis nigromaculatus"~"Freshwater",
                                            TRUE~Habitat_Broadest))
fishhabitatfinal<-unique(fishhabitatfinal)
length(unique(fishhabitatfinal$SCI_NAME))
## combine data---------------------
popdata1 <- popdata1 %>% 
  rename(SCI_NAME = sp)
inddata1 <- inddata1 %>% 
  rename(SCI_NAME = sp)



#check that species are structured the same
popdata1 <- popdata1 %>% 
  mutate(SCI_NAME = str_to_sentence(SCI_NAME))
inddata1 <- inddata1 %>% 
  mutate(SCI_NAME = str_to_sentence(SCI_NAME))
speciesdata <- speciesdata %>% 
  mutate(SCI_NAME = str_to_sentence(SCI_NAME))

speciesdata<-speciesdata%>% distinct(SCI_NAME,.keep_all=TRUE)
speciesdata<-merge(fishhabitatfinal,speciesdata,by="SCI_NAME")



#thermdata from Verberk 2026 paper
#freshwater and marine
thermdata0<-read_csv(file = file.path("data",
                                      "data_fish_thermal_tolerance.csv"))
thermdata0<-thermdata0%>%select(species_fb,tolerance_value_corr)
thermdata0 <- thermdata0 %>% 
  rename(SCI_NAME = species_fb,
         tol = tolerance_value_corr)



#thermal tolerance data
#freshwater thermal tolerance
#bayat et al 2025
thermdata1 <- read_csv(file = file.path("data",
                                       "thermtol_comb_final.csv"))
thermdata1<-thermdata1%>%select(taxon, tol,metric,endpoint)
thermdata1<-subset(thermdata1,metric=="ctmax")
thermdata1<-subset(thermdata1,endpoint=="loe")
thermdata1 <- thermdata1 %>% 
  rename(SCI_NAME = taxon)
thermdata1 <- thermdata1 %>% 
  select(SCI_NAME,tol)



#merge both
thermdata<-rbind(thermdata0,thermdata1)

#get averages for species with multiple values
thermdata <- thermdata %>%
  group_by(SCI_NAME) %>%
  summarise(tol = mean(tol, na.rm = TRUE))


#add to datasets

popdata1<-left_join(popdata1,thermdata,by="SCI_NAME")
inddata1<-left_join(inddata1,thermdata,by="SCI_NAME")


#Combine Datasets
popdata<-merge(popdata1,speciesdata,by="SCI_NAME")
popdata<-popdata%>% rename(Ctmax=tol)
#remove nas
#popdata<-subset(popdata,!is.na(popdata$Max_TL_cm))

inddata<-merge(inddata1,speciesdata,by="SCI_NAME")
inddata<-inddata%>% rename(Ctmax=tol)
#inddata<-subset(inddata,!is.na(inddata$Max_TL_cm))


##Correlogram---------------------
inddatatemp<-subset(inddata,.variable=="beta_temp_sp")
inddatado<-subset(inddata,.variable=="beta_DO_sp")


popdatatemp<-subset(popdata,.variable=="beta_temp_sp")
popdatado<-subset(popdata,.variable=="beta_DO_sp")

#individual all data 

#temp
tempsize<-ggpairs(inddatatemp, columns = c(7,10:15,3),aes(color=Habitat_Broadest))
print(tempsize)


#no significant effects
#do
dosize<-ggpairs(inddatado, columns = c(7,10:15,3),aes(color=Habitat_Broadest)) 
print(dosize)

#population all data 
#strong positive temp preference effects on population density, no effects of do
#temp
tempop<-ggpairs(popdatatemp, columns = c(7,10:15,3),aes(color=Habitat_Broadest)) 
print(tempop)
#do
dopop<-ggpairs(popdatado, columns = c(7,10:15,3),aes(color=Habitat_Broadest)) 
print(dopop)


##Make Graphs for Individual Size DO and Temperature ---------------------

#scatterplot function
scatter_funind = function(x, y) {
  ggplot(inddata, aes(x = .data[[x]], y = .data[[y]],color=Habitat_Broadest) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_cowplot()+
    ggtitle("Individual Species Level")+
    facet_wrap(~factor(.variable,levels=c("beta_temp_sp","beta_DO_sp")),scales="free_y")
}

#select explanatory and response variables
explind = names(inddata)[c(7,11,15)]
explind = set_names(explind)
respind = names(inddata)[c(3)]
respind = set_names(respind)

all_plots_ind = purrr::map(respind, function(respind) {
  purrr::map(explind, function(explind) {
    scatter_funind(x = explind, y = respind)
  })
})
print(all_plots_ind)
allplotsind<-patchwork::wrap_plots(nrow=3,plotlist=all_plots_ind[[1]])+
  patchwork::plot_layout(guides="collect",)+ 
  patchwork::plot_annotation(tag_levels = 'A')&
  theme(legend.position="bottom")
nflplotR::ggpreview(allplotsind,height=10,width=8)



##Make Graphs for Population Size DO and Temperature ---------------------

#scatterplot function
scatter_funpop = function(x, y) {
  ggplot(popdata, aes(x = .data[[x]], y = .data[[y]],color=Habitat_Broadest)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_cowplot()+
    ggtitle("Population Species Level")+
    facet_wrap(~factor(.variable,levels=c("beta_temp_sp","beta_DO_sp")),scales="free_y")
  
}

#select explanatory and response variables
explpop = names(popdata)[c(7,11,15)]
explpop = set_names(explpop)
resppop = names(popdata)[c(3)]
resppop = set_names(resppop)

all_plots_pop = purrr::map(resppop, function(resppop) {
  purrr::map(explpop, function(explpop) {
    scatter_funpop(x = explpop, y = resppop)
  })
})
print(all_plots_pop)
allplotspop<-patchwork::wrap_plots(nrow=3,plotlist=all_plots_pop[[1]])+
  patchwork::plot_layout(guides="collect",)+ 
  patchwork::plot_annotation(tag_levels = 'A')&
  theme(legend.position="bottom")
nflplotR::ggpreview(allplotspop,height=10,width=8)

# Bayesian Correlations ---------------------------------------------------

##Temperature Correlations
#brms correlation following this set up https://solomonkurz.netlify.app/blog/2019-02-16-bayesian-correlations-let-s-talk-options/
#temperature size
#first standardize data
inddatatemp<-inddatatemp %>% 
  mutate(median_s = (median - mean(median)) / sd(median),
         Common_Length_cm_s = (Common_Length_cm - mean(Common_Length_cm,na.rm=TRUE)) / sd(Common_Length_cm,na.rm=TRUE),
         tol_s=(tol-mean(tol,na.rm=TRUE))/sd(tol,na.rm=TRUE),
         Trophic_Level_s=(Trophic_Level - mean(Trophic_Level,na.rm=TRUE)) / sd(Trophic_Level,na.rm=TRUE))

#then separate for marine, estuary, and freshwater
inddatatempmarine<-subset(inddatatemp,Habitat_Broadest=="Marine")
inddatatempestuary<-subset(inddatatemp,Habitat_Broadest=="Estuary")
inddatatempfreshwater<-subset(inddatatemp,Habitat_Broadest=="Freshwater")


#Marine
tempcormarine1<-brm(data = inddatatempmarine, 
              family = gaussian,
              median_s~1+Common_Length_cm_s,
              chains = 4, cores = 4, 
              seed = 1)
p_direction(tempcormarine1)
#should be same correlation as pearson
cor.test(inddatatemp$median,inddatatemp$Common_Length_cm_s,method="pearson")
print(tempcormarine1)

tempcormarine2<-update(tempcormarine1,formula=median_s~1+tol_s,newdata=inddatatempmarine,seed=1)
print(tempcormarine2)
p_direction(tempcormarine2)

tempcormarine3<-update(tempcormarine1,formula=median_s~1+Trophic_Level_s,newdata=inddatatempmarine,seed=1)
print(tempcormarine3)
p_direction(tempcormarine3)

#Estuary
tempcorestuary1<-brm(data = inddatatempestuary, 
                    family = gaussian,
                    median_s~1+Common_Length_cm_s,
                    chains = 4, cores = 4, 
                    seed = 1)
p_direction(tempcorestuary1)
#should be same correlation as pearson
cor.test(inddatatemp$median,inddatatemp$Common_Length_cm_s,method="pearson")
print(tempcorestuary1)

tempcorestuary2<-update(tempcorestuary1,formula=median_s~1+tol_s,newdata=inddatatempestuary,seed=1)
print(tempcorestuary2)
p_direction(tempcorestuary2)

tempcorestuary3<-update(tempcorestuary1,formula=median_s~1+Trophic_Level_s,newdata=inddatatempestuary,seed=1)
print(tempcorestuary3)
p_direction(tempcorestuary3)

#freshwater
tempcorfreshwater1<-brm(data = inddatatempfreshwater, 
                    family = gaussian,
                    median_s~1+Common_Length_cm_s,
                    chains = 4, cores = 4, 
                    seed = 1)
p_direction(tempcorfreshwater1)
#should be same correlation as pearson
cor.test(inddatatempfreshwater$median,inddatatempfreshwater$Common_Length_cm_s,method="pearson")
print(tempcorfreshwater1)

tempcorfreshwater2<-update(tempcorfreshwater1,formula=median_s~1+tol_s,newdata=inddatatempfreshwater,seed=1)
print(tempcorfreshwater2)
p_direction(tempcorfreshwater2)

tempcorfreshwater3<-update(tempcorfreshwater1,formula=median_s~1+Trophic_Level_s,newdata=inddatatempfreshwater,seed=1)
print(tempcorfreshwater3)
p_direction(tempcorfreshwater3)


#Pop correlations
popdatatemp<-popdatatemp %>% 
mutate(median_s = (median - mean(median)) / sd(median),
       Common_Length_cm_s = (Common_Length_cm - mean(Common_Length_cm,na.rm=TRUE)) / sd(Common_Length_cm,na.rm=TRUE),
       Trophic_Level_s=(Trophic_Level - mean(Trophic_Level,na.rm=TRUE)) / sd(Trophic_Level,na.rm=TRUE),
       tol_s=(tol-mean(tol,na.rm=TRUE))/sd(tol,na.rm=TRUE))
#then separate for marine, estuary, and freshwater
popdatatempmarine<-subset(popdatatemp,Habitat_Broadest=="Marine")
popdatatempestuary<-subset(popdatatemp,Habitat_Broadest=="Estuary")
popdatatempfreshwater<-subset(popdatatemp,Habitat_Broadest=="Freshwater")

#marine
poptempcormarine1<-brm(data = popdatatempmarine, 
                    family = gaussian,
                    median_s~1+Common_Length_cm_s,
                    chains = 4, cores = 4, 
                    seed = 1)
p_direction(poptempcormarine1)
#should be same correlation as pearson
cor.test(popdatatempmarine$median,popdatatempmarine$Common_Length_cm_s,method="pearson")
print(poptempcormarine1)

poptempcormarine2<-update(poptempcormarine1,formula=median_s~1+tol_s,newdata=popdatatempmarine,seed=1)
print(poptempcormarine2)
p_direction(poptempcormarine2)

poptempcormarine3<-update(poptempcormarine1,formula=median_s~1+Trophic_Level_s,newdata=popdatatempmarine,seed=1)
print(poptempcormarine3)
p_direction(poptempcormarine3)

#Estuary
poptempcorestuary1<-brm(data = popdatatempestuary, 
                     family = gaussian,
                     median_s~1+Common_Length_cm_s,
                     chains = 4, cores = 4, 
                     seed = 1)
p_direction(poptempcorestuary1)
#should be same correlation as pearson
cor.test(popdatatemp$median,popdatatemp$Common_Length_cm_s,method="pearson")
print(poptempcorestuary1)

poptempcorestuary2<-update(poptempcorestuary1,formula=median_s~1+tol_s,newdata=popdatatempestuary,seed=1)
print(poptempcorestuary2)
p_direction(poptempcorestuary2)

poptempcorestuary3<-update(poptempcorestuary1,formula=median_s~1+Trophic_Level_s,newdata=popdatatempestuary,seed=1)
print(poptempcorestuary3)
p_direction(poptempcorestuary3)

#freshwater
poptempcorfreshwater1<-brm(data = popdatatempfreshwater, 
                        family = gaussian,
                        median_s~1+Common_Length_cm_s,
                        chains = 4, cores = 4, 
                        seed = 1)
p_direction(poptempcorfreshwater1)
#should be same correlation as pearson
cor.test(popdatatempfreshwater$median_s,popdatatempfreshwater$Common_Length_cm_s,method="pearson")
print(poptempcorfreshwater1)

poptempcorfreshwater2<-update(poptempcorfreshwater1,formula=median_s~1+tol_s,newdata=popdatatempfreshwater,seed=1)
print(poptempcorfreshwater2)
p_direction(poptempcorfreshwater2)

poptempcorfreshwater3<-update(poptempcorfreshwater1,formula=median_s~1+Trophic_Level_s,newdata=popdatatempfreshwater,seed=1)
print(poptempcorfreshwater3)
p_direction(poptempcorfreshwater3)


#DO
#first standardize data
inddatado<-inddatado %>% 
  mutate(median_s = (median - mean(median)) / sd(median),
         Common_Length_cm_s = (Common_Length_cm - mean(Common_Length_cm,na.rm=TRUE)) / sd(Common_Length_cm,na.rm=TRUE),
         Trophic_Level_s=(Trophic_Level - mean(Trophic_Level,na.rm=TRUE)) / sd(Trophic_Level,na.rm=TRUE),
         tol_s=(tol-mean(tol,na.rm=TRUE))/sd(tol,na.rm=TRUE))

#then separate for marine, estuary, and freshwater
inddatadomarine<-subset(inddatado,Habitat_Broadest=="Marine")
inddatadoestuary<-subset(inddatado,Habitat_Broadest=="Estuary")
inddatadofreshwater<-subset(inddatado,Habitat_Broadest=="Freshwater")

#marine
docor1indmarine<-brm(data = inddatadomarine, 
              family = gaussian,
              median_s~1+Common_Length_cm_s,
              chains = 4, cores = 4, 
              seed = 1)
p_direction(docor1indmarine)
print(docor1indmarine)

docor2indmarine<-update(docor1indmarine,formula=median_s~1+Trophic_Level_s,newdata=inddatadomarine,seed=1)
print(docor2indmarine)
p_direction(docor2indmarine)

docor3indmarine<-update(docor1indmarine,formula=median_s~1+tol_s,newdata=inddatadomarine,seed=1)
print(docor3indmarine)
p_direction(docor3indmarine)

#estuary
docor1indestuary<-brm(data = inddatadoestuary, 
                     family = gaussian,
                     median_s~1+Common_Length_cm_s,
                     chains = 4, cores = 4, 
                     seed = 1)
p_direction(docor1indestuary)
print(docor1indestuary)

docor2indestuary<-update(docor1indestuary,formula=median_s~1+Trophic_Level_s,newdata=inddatadoestuary,seed=1)
print(docor2indestuary)
p_direction(docor2indestuary)

docor3indestuary<-update(docor1indestuary,formula=median_s~1+tol_s,newdata=inddatadoestuary,seed=1)
print(docor3indestuary)
p_direction(docor3indestuary)

#freshwater

docor1indfreshwater<-brm(data = inddatadofreshwater, 
                      family = gaussian,
                      median_s~1+Common_Length_cm_s,
                      chains = 4, cores = 4, 
                      seed = 1)
p_direction(docor1indfreshwater)
print(docor1indfreshwater)

docor2indfreshwater<-update(docor1indfreshwater,formula=median_s~1+Trophic_Level_s,newdata=inddatadofreshwater,seed=1)
print(docor2indfreshwater)
p_direction(docor2indfreshwater)

docor3indfreshwater<-update(docor1indfreshwater,formula=median_s~1+tol_s,newdata=inddatadofreshwater,seed=1)
print(docor3indfreshwater)
p_direction(docor3indfreshwater)

#Pop correlations
popdatado<-popdatado %>% 
  mutate(median_s = (median - mean(median)) / sd(median),
         Common_Length_cm_s = (Common_Length_cm - mean(Common_Length_cm,na.rm=TRUE)) / sd(Common_Length_cm,na.rm=TRUE),
         Trophic_Level_s=(Trophic_Level - mean(Trophic_Level,na.rm=TRUE)) / sd(Trophic_Level,na.rm=TRUE),
         tol_s=(tol-mean(tol,na.rm=TRUE))/sd(tol,na.rm=TRUE))

#then separate by habitat
popdatadomarine<-subset(popdatado,Habitat_Broadest=="Marine")
popdatadoestuary<-subset(popdatado,Habitat_Broadest=="Estuary")
popdatadofreshwater<-subset(popdatado,Habitat_Broadest=="Freshwater")

#marine
docor1popmarine<-brm(data = popdatadomarine, 
                     family = gaussian,
                     median_s~1+Common_Length_cm_s,
                     chains = 4, cores = 4, 
                     seed = 1)
p_direction(docor1popmarine)
print(docor1popmarine)

docor2popmarine<-update(docor1popmarine,formula=median_s~1+Trophic_Level_s,newdata=popdatadomarine,seed=1)
print(docor2popmarine)
p_direction(docor2popmarine)

docor3popmarine<-update(docor1popmarine,formula=median_s~1+tol_s,newdata=popdatadomarine,seed=1)
print(docor3popmarine)
p_direction(docor3popmarine)

#estuary
docor1popestuary<-brm(data = popdatadoestuary, 
                      family = gaussian,
                      median_s~1+Common_Length_cm_s,
                      chains = 4, cores = 4, 
                      seed = 1)
p_direction(docor1popestuary)
print(docor1popestuary)

docor2popestuary<-update(docor1popestuary,formula=median_s~1+Trophic_Level_s,newdata=popdatadoestuary,seed=1)
print(docor2popestuary)
p_direction(docor2popestuary)

docor3popestuary<-update(docor1popestuary,formula=median_s~1+tol_s,newdata=popdatadoestuary,seed=1)
print(docor3popestuary)
p_direction(docor3popestuary)

#freshwater

docor1popfreshwater<-brm(data = popdatadofreshwater, 
                         family = gaussian,
                         median_s~1+Common_Length_cm_s,
                         chains = 4, cores = 4, 
                         seed = 1)
p_direction(docor1popfreshwater)
print(docor1popfreshwater)

docor2popfreshwater<-update(docor1popfreshwater,formula=median_s~1+Trophic_Level_s,newdata=popdatadofreshwater,seed=1)
print(docor2popfreshwater)
p_direction(docor2popfreshwater)

docor3popfreshwater<-update(docor1popfreshwater,formula=median_s~1+tol_s,newdata=popdatadofreshwater,seed=1)
print(docor3popfreshwater)
p_direction(docor3popfreshwater)


