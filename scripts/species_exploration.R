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
                 vegan,readxl,GGally)

source(file = file.path("scripts",
                        "functions.R"))

source(file = file.path("scripts",
                        "viz_ideas.R"))

## load data ---------------------

#internal note, make sure to download data from Google drive first

#species data were collated from Fish Base between June 25th and June 27th and again on September 22nd 2025
speciesdata <- read_xlsx(path = file.path("data",
                                   "Species_attributes_V2.xlsx"))

#add site metadata
sitedata <- read_csv(file = file.path("data",
                                      "final_harmonized_midsites_summary.csv"))
#add more site info

coord<-read_xlsx(path = file.path("data",
                                  "allsites_coords.xlsx"))


sitedata<-merge(sitedata,coord, by="MIDSITE")
sitedata$...7<-NULL
sitedata$...1<-NULL

#individual and population data are from model outputs
inddata1 <- read_csv(file = file.path("data",
                                         "ind.csv"))

popdata1 <- read_csv(file = file.path("data",
                                     "pop.csv"))

## combine data---------------------
popdata1 <- popdata1 %>% 
  rename(SCI_NAME = sp)
inddata1 <- inddata1 %>% 
  rename(SCI_NAME = sp)


#get overall site name for pop data and ind data
#popdata1$Site<-str_sub(popdata1$si,1,8)
#inddata1$Site<-str_sub(inddata1$si,1,8)
#sitedata$Site<-paste0(sitedata$Network,"_",sitedata$Site)
#sitedata$Site<-str_sub(sitedata$Site,1,8)

#check that species are structured the same
popdata1 <- popdata1 %>% 
  mutate(SCI_NAME = str_to_sentence(SCI_NAME))
inddata1 <- inddata1 %>% 
  mutate(SCI_NAME = str_to_sentence(SCI_NAME))
speciesdata <- speciesdata %>% 
  mutate(SCI_NAME = str_to_sentence(SCI_NAME))

speciesdata<-speciesdata%>% distinct(SCI_NAME,.keep_all=TRUE)

#checking what Ftp correlates with for marine fish
plot(speciesdata$T_pref_mean,speciesdata$Ftp)
plot(speciesdata$T_pref_low,speciesdata$Ftp)
plot(speciesdata$T_pref_high,speciesdata$Ftp)


#thermal tolerance data
#freshwater thermal tolerance
thermdata1 <- read_csv(file = file.path("data",
                                       "thermtol_comb_final.csv"))
thermdata1<-thermdata1%>%select(taxon, tol,metric,endpoint)
thermdata1<-subset(thermdata1,metric=="ctmax")
thermdata1<-subset(thermdata1,endpoint=="loe")
thermdata1 <- thermdata1 %>% 
  rename(SCI_NAME = taxon)
thermdata1 <- thermdata1 %>% 
  select(SCI_NAME,tol)


#globtherm
thermdata2 <- read_excel(path = file.path("data",
                                          "GlobalTherm_upload_10_11_17.xlsx"))  
thermdata2<-thermdata2%>%select(Genus,Species,Tmax,max_metric)
thermdata2$SCI_NAME=paste(thermdata2$Genus,thermdata2$Species)
thermdata2<-subset(thermdata2,max_metric=="ctmax")
thermdata2 <- thermdata2 %>% 
  rename(metric = max_metric,
         tol= Tmax)
thermdata2<-thermdata2%>%select(SCI_NAME,tol)
#merge two

thermdata<-rbind(thermdata1,thermdata2)

#get averages for species with multiple values
thermdata <- thermdata %>%
  group_by(SCI_NAME) %>%
  summarise(tol = mean(tol, na.rm = TRUE))



#add to datasets

popdata1<-left_join(popdata1,thermdata,by="SCI_NAME")
inddata1<-left_join(inddata1,thermdata,by="SCI_NAME")
speciesdata<-left_join(speciesdata,thermdata,by="SCI_NAME")

#Combine Datasets

sitedata <- sitedata %>% 
  rename(si = MIDSITE)

popdata<-merge(popdata1,speciesdata,by="SCI_NAME")
popdata<-merge(popdata,sitedata,by="si")
#remove species with common lengths >100
popdata<-subset(popdata,Max_TL_cm<400|is.na(popdata$Max_TL_cm))
inddata<-merge(inddata1,speciesdata,by="SCI_NAME")
inddata<-subset(inddata,Max_TL_cm<400|is.na(inddata$Max_TL_cm))
inddata<-merge(inddata,sitedata,by="si")

#marine data based on 
#Protein, lipid and energy requirements of cultured marine fish in cold, temperate and warm water
#freshwater based on
#Asymmetric impacts of climate change on thermal habitat suitability for inland lake fishes

#assign fish to categories based on temperature preference
popdata<-popdata%>% mutate(
  Therm_cat = case_when(
    Habitat_Broadest=="Marine" & T_pref_mean >= 25 |
      Habitat_Broadest=="Freshwater" & Ftp >=23 ~"Warm",
    Habitat_Broadest=="Marine" & between(T_pref_mean,20,25)|
      Habitat_Broadest=="Freshwater" & between(Ftp,19,23) ~"Cool",
    Habitat_Broadest=="Marine" & T_pref_mean<=20 |
    Habitat_Broadest=="Freshwater" & Ftp <=19 ~"Cold"
  ))

inddata<-inddata%>% mutate(
  Therm_cat = case_when(
    Habitat_Broadest=="Marine" & T_pref_mean >= 25 |
      Habitat_Broadest=="Freshwater" & Ftp >=23 ~"Warm",
    Habitat_Broadest=="Marine" & between(T_pref_mean,20,25)|
      Habitat_Broadest=="Freshwater" & between(Ftp,19,23) ~"Cool",
    Habitat_Broadest=="Marine" & T_pref_mean<=20 |
      Habitat_Broadest=="Freshwater" & Ftp <=19 ~"Cold"
  ))

hist(inddata$T_pref_mean)

#Checking for Overall Differences Between Marine and Freshwater

ggplot(data=inddata,aes(x=Habitat_Broadest,y=median))+
  geom_boxplot()+
  facet_grid(~.variable)+
  theme_cowplot()

ggplot(data=popdata,aes(x=Habitat_Broadest,y=median))+
  geom_boxplot()+
  facet_grid(~.variable)+
  theme_cowplot()


##Correlogram---------------------
inddatatemp<-subset(inddata,.variable=="beta_temp")
inddatado<-subset(inddata,.variable=="beta_DO")
inddatafilteredtemp<-subset(inddatatemp,PDpos>=0.80)
inddatafiltereddo<-subset(inddatado,PDpos>=0.80)

#remove outliers
inddatado<-subset(inddatado,median>-0.5)

popdatatemp<-subset(popdata,.variable=="beta_temp")
popdatado<-subset(popdata,.variable=="beta_DO")
popdatafilteredtemp<-subset(popdatatemp,PDpos>=0.80)
popdatafiltereddo<-subset(popdatado,PDpos>=0.80)

#thermal preference data # of values
length(unique(popdata$SCI_NAME))
inddatatemp%>% group_by(Habitat)%>%distinct(SCI_NAME, .keep_all = TRUE)%>% summarise(na_count = sum(!is.na(T_pref_high)))
#59 species
#five freshwater
#53 marine
inddatatemp%>%group_by(Habitat)%>% distinct(SCI_NAME, .keep_all = TRUE)%>% summarise(na_count = sum(!is.na(tol.x)))
#61 species
#52 freshwater
#14 marine

#thermal preference
inddatatemp%>% group_by(Habitat_Broad)%>%distinct(SCI_NAME, .keep_all = TRUE)%>% summarise(na_count = sum(!is.na(T_pref_mean)))
r
#53 marine
inddatatemp%>%group_by(Habitat_Broadest)%>% distinct(SCI_NAME, .keep_all = TRUE)%>% summarise(na_count = sum(!is.na(Ftp)))
#51

#all the thermal variables are pretty strongly correlated
ggpairs(inddatatemp, columns = c(8,12:14,19:20),ggplot2::aes(color=Habitat_Broadest)) 

#individual all data 

# positive effect of thermal preference high temp preference for temp effect on body size, warmer fish less negatively affected
#temp
tempsize<-ggpairs(inddatatemp, columns = c(8,11,14:15,19,32,4),ggplot2::aes(color=Habitat_Broad)) 
print(tempsize)


#no significant effects
#do
dosize<-ggpairs(inddatado, columns = c(8,10:15,19,4),ggplot2::aes(color=Habitat_Broad)) 
print(dosize)
#individual filtered
#filtering generally led to reduced significance and removed many samples
#temp
ggpairs(inddatafilteredtemp, columns = c(8,11,14:15,19,32,4),ggplot2::aes(color=Habitat_Broad)) 

#do
ggpairs(inddatafiltereddo,columns = c(8,10:15,19,4),ggplot2::aes(color=Habitat_Broad)) 


#population all data 
#strong positive temp preference effects on population density, no effects of do
#temp
tempop<-ggpairs(popdatatemp, columns = c(8,10:15,19,32,4),ggplot2::aes(color=Habitat_Broad)) 
print(tempop)
#do
dopop<-ggpairs(popdatado, columns = c(9,12:17,5),ggplot2::aes(color=Habitat)) 
print(dopop)
#population filtered
#not enough points to look at habitat
#temp
ggpairs(popdatafilteredtemp, columns = c(9,12:17,5),ggplot2::aes(color=Habitat)) 

#do
ggpairs(popdatafiltereddo, columns = c(9,12:17,5),ggplot2::aes(color=Habitat)) 


##Make Graphs for Individual Size DO and Temperature ---------------------
inddata<-subset(inddata,.variable==c("beta_temp","beta_DO"))
#scatterplot function
scatter_funind = function(x, y) {
  ggplot(inddata, aes(x = .data[[x]], y = .data[[y]],color=Habitat_Broad) ) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_cowplot()+
    ggtitle("Individual Unfiltered")+
    facet_grid(~.variable)
}

#select explanatory and response variables
explind = names(inddata)[c(8,11,14:15,19)]
explind = set_names(explind)
respind = names(inddata)[c(4)]
respind = set_names(respind)

all_plots_ind = purrr::map(respind, function(respind) {
  purrr::map(explind, function(explind) {
    scatter_funind(x = explind, y = respind)
  })
})
print(all_plots_ind)



##Make Graphs for Population Size DO and Temperature ---------------------
popdata<-subset(popdata,.variable==c("beta_temp","beta_DO"))
#scatterplot function
scatter_funpop = function(x, y) {
  ggplot(popdata, aes(x = .data[[x]], y = .data[[y]],color=Habitat_Broad)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    theme_cowplot()+
    ggtitle("Population Unfiltered")+
    facet_grid(~.variable)
  
}

#select explanatory and response variables
explpop = names(popdata)[c(8,11,14:15,19)]
explpop = set_names(explpop)
resppop = names(popdata)[c(4)]
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
    facet_grid(~var)
}

#select explanatory and response variables
explind = names(inddatafiltered)[13:18]
explind = set_names(explind)
respind = names(inddatafiltered)[7]
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
explpop = names(popdata)[13:18]
explpop = set_names(explpop)
resppop = names(popdata)[7]
resppop = set_names(resppop)

all_plots_pop_filtered = purrr::map(resppop, function(resppop) {
  purrr::map(explpop, function(explpop) {
    scatter_funpopfiltered(x = explpop, y = resppop)
  })
})
print(all_plots_pop_filtered)


#Graph by Thermal Category
inddatatemp2<-subset(inddatatemp,Therm_cat!="NA")

#inddatatemp2<-subset(inddatatemp2,Habitat_Broad!="Estuary")
ggplot(inddatatemp2,aes(x=Therm_cat,y=median,color=Habitat_Broad))+
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1)+
geom_boxplot()  +
  theme_cowplot()+
  xlab("Thermal Category")+
  facet_grid(~Habitat_Broad)

ggplot(inddatatemp2,aes(x=Therm_cat,y=median,color=Habitat_Broadest))+
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1)+
  geom_boxplot()  +
  theme_cowplot()+
  xlab("Thermal Category")+
  facet_grid(~Habitat_Broadest)

popdatatemp2<-subset(popdatatemp,Therm_cat!="NA")
#popdatatemp2<-subset(popdatatemp2,Habitat_Broad!="Estuary")

#pca
poppca<-popdatatemp2[,c(27,10,34,15)]
poppca<-poppca%>%drop_na()
popdatatemp2<-popdatatemp2%>%drop_na(Max_TL_cm)

poppca$Therm_cat[(poppca$Therm_cat) == "Cold"] <- "1"
poppca$Therm_cat[(poppca$Therm_cat) == "Cool"] <- "2"
poppca$Therm_cat[(poppca$Therm_cat) == "Warm"] <- "3"
poppca$Therm_cat<-as.numeric(poppca$Therm_cat)
pca_result<-prcomp(poppca)
pc.scores <- pca_result$x
loadings_matrix <- pca_result$rotation
poppca2<-cbind(pc.scores,popdatatemp2)
ggplot(poppca2, aes(x=PC1, y=PC2,shape=Habitat_Broadest, color=Therm_cat))+ 
  geom_point()+
  theme_classic()

poppca2$Therm_cat[(poppca2$Therm_cat) == "Cold"] <- "1"
poppca2$Therm_cat[(poppca2$Therm_cat) == "Cool"] <- "2"
poppca2$Therm_cat[(poppca2$Therm_cat) == "Warm"] <- "3"
poppca2$Therm_cat<-as.numeric(poppca2$Therm_cat)

averagetemp<-poppca2%>%group_by(si)%>%summarize(mean=mean(Therm_cat),sd=sd(Therm_cat))

averagetemp<-merge(averagetemp,poppca2,by="si")

ggplot(averagetemp,aes(x=mean,y=median,color=Habitat_Broadest))+
  geom_point()+
  geom_smooth(method="lm",SE=TRUE)+
  theme_cowplot()

ggplot(popdatatemp2,aes(x=Therm_cat,y=median,color=Habitat_Broadest))+
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1)+
  geom_boxplot()  +
  theme_cowplot()+
  xlab("Thermal Category")+
  facet_grid(~Habitat_Broad)

ggplot(popdatatemp2,aes(x=Therm_cat,y=median,color=Habitat_Broadest))+
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1)+
  geom_boxplot()  +
  theme_cowplot()+
  xlab("Thermal Category")+
  facet_grid(~Habitat_Broadest)

ggplot(popdatatemp2,aes(x=T_pref_mean,y=median))+
  geom_point()+
  geom_smooth(method="lm", se= TRUE)

#just looking at ftp
ggplot(inddatatemp2,aes(x=Ftp,y=median,color=Therm_cat))+
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1)+
  geom_point()+
  geom_smooth(method="lm", se= TRUE)+
  theme_cowplot()
ggplot(popdatatemp2,aes(x=Ftp,y=median,color=Therm_cat))+
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1)+
  geom_point()+
  geom_smooth(method="lm", se= TRUE)+
  theme_cowplot()

hist(inddata$T_pref_mean)
hist(inddata$Ftp)
#size based within categories?
ggplot(popdatatemp2,aes(x=Max_TL_cm,y=median,color=Habitat_Broadest))+
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1)+
  geom_point()  +
  theme_cowplot()+
  geom_smooth(method = "lm", se = TRUE)+
  facet_grid(~Therm_cat)

ggplot(inddatatemp2,aes(x=Max_TL_cm,y=median,color=Habitat_Broad))+
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1)+
  geom_point()  +
  theme_cowplot()+
  geom_smooth(method = "lm", se = TRUE)+
  facet_grid(~Therm_cat)

#Do by thermal category

inddatado2<-subset(inddatado,Therm_cat!="NA")

inddatado2<-subset(inddatado2,Habitat_Broad!="Estuary")
ggplot(inddatado2,aes(x=Therm_cat,y=median,color=Habitat_Broad))+
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1)+
  geom_boxplot()  +
  theme_cowplot()+
  xlab("Thermal Category")+
  facet_grid(~Habitat_Broad)

ggplot(inddatado2,aes(x=Therm_cat,y=median,color=Habitat_Broadest))+
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1)+
  geom_boxplot()  +
  theme_cowplot()+
  xlab("Thermal Category")+
  facet_grid(~Habitat_Broadest)

popdatado2<-subset(popdatado,Therm_cat!="NA")
popdatado2<-subset(popdatado2,Habitat_Broad!="Estuary")
ggplot(popdatado2,aes(x=Therm_cat,y=median,color=Habitat_Broadest))+
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1)+
  geom_boxplot()  +
  theme_cowplot()+
  xlab("Thermal Category")+
  facet_grid(~Habitat_Broad)

ggplot(popdatado2,aes(x=Therm_cat,y=median,color=Habitat_Broad))+
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1)+
  geom_boxplot()  +
  theme_cowplot()+
  xlab("Thermal Category")+
  facet_grid(~Habitat_Broad)

#how many in each group?
ggplot(popdatatemp2,aes(x=Habitat_Broadest,y=T_pref_mean,color=Habitat_Broadest))+
  geom_point()  +
  theme_cowplot()+
  geom_smooth(method = "lm", se = TRUE)+
  facet_grid(~Therm_cat,scales="free")


ggplot(popdatatemp2,aes(x=Habitat_Broad,y=Ftp,color=Habitat_Broad))+
  geom_point()  +
  theme_cowplot()+
  geom_smooth(method = "lm", se = TRUE)+
  facet_grid(~Therm_cat)

ggplot(popdatatemp2,aes(x=Therm_cat,fill=Habitat_Broad))+
  geom_bar(position=position_dodge())+
  theme_cowplot()


##
inddatatemp



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