#_________________________________
#LTER SBC - Annual fish surveys  
# SCALES/ SSECR                  
# Allie Case / Bethany Williams  
# R Version: 4.4.2 (2024-10-31) -- "Pile of Leaves"
#_________________________________

# setup ---------------------

rm(list = ls())

#create directory for LTER summary output and basic graphs

ifelse(!dir.exists(file.path("data", "metadata")), 
       dir.create(file.path("data", "metadata")), 
       FALSE)

#set "site name" based on the name of your raw data folder and what the output should look like for naming convention, This step is critical!  

dataset <- "LTER_SBC"

## load packages and function --------------------

#install.packages("librarian")

librarian::shelf(supportR, tidyverse, summarytools, 
                 datacleanr, lterdatasampler,
                 cowplot, gt,
                 vegan)

source(file = file.path("scripts",
                        "functions.R"))

source(file = file.path("scripts",
                        "viz_ideas.R"))

## load data ---------------------

#Downloading the data has already been done for you by BW. To download the "raw" data from Google Drive, use the code provided below all the comments. 

# Package ID: knb-lter-sbc.17.40 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Reef: Kelp Forest Community Dynamics: Fish abundance.
# Data set creator:  Daniel C Reed -
# Data set creator:  Robert J Miller -
# Contact:    -  Santa Barbara Coastal LTER  - sbclter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu

# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/17/40/a7899f2e57ea29a240be2c00cce7a0d4"
# infile1 <- tempfile()
# try(download.file(inUrl1,infile1,method="curl"))
# if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
# dt1 <-read.csv(infile1,header=F
#                ,skip=1
#                ,sep=","
#                ,quot='"'
#                , col.names=c(
#                  "YEAR",
#                  "MONTH",
#                  "DATE",
#                  "SITE",
#                  "TRANSECT",
#                  "QUAD",
#                "SIDE",
#                 "VIS",
#                 "SP_CODE",
#                 "SIZE",
#                 "COUNT",
#                 "AREA",
#                 "SCIENTIFIC_NAME",
#                  "COMMON_NAME",
#                   "TAXON_KINGDOM",
#                   "TAXON_PHYLUM",
#                   "TAXON_CLASS",
#                   "TAXON_ORDER",
#                   "TAXON_FAMILY",
#                   "TAXON_GENUS",
#                   "GROUP",
#                   "SURVEY",
#                   "MOBILITY",
#                 "GROWTH_MORPH"), check.names=TRUE)
# 
# unlink(infile1)
# write.csv(dt1,
#            file = file.path("data",
#                             "raw_data",
#                             "LTER_SBC",
#                             "raw_LTER_SBC_fish.csv"),
#            row.names = F)
#  rm(dt1)

#read in data - make sure to download from Google Drive

data <- read.csv(file = file.path("data",
                                  "raw_data",
                                  "LTER_SBC",
                                  "raw_LTER_SBC_fish.csv"))

#PART 1: FISH -----

# checks --------------------

#get generic output on data structures and unique values for each - this you can keep! It's set up so that it will name it based on your unique site value you named earlier. 

summarytools::view(summarytools::dfSummary(data),
                   file = file.path("data",
                                    "metadata",
                                    paste0(dataset, 
                                           "_datasummary.html")))

#here is where I also throw in questions about things I don't know AND also don't fit an obvious "block" of work like dates, species, spatial. What are the different survey methods? I made a note in the metadata that there are technically two different ones.

## duplicates -----

#no duplicate rows found (summary output). If there were, definitely remove them here first before expanding any data for counts/abundance.

## counts/abundance -----

#This is the most important part of data to have and understand so it makes sense to start with it here. 

#-99999 for COUNT means the value was not recorded or not available. 0 is defined in the metadata as "The number of individuals of the same size counted", but in reality these counts were likely done with checklists and it means that these fish weren't counted, so they should be dropped. The final structure of our data should be EACH row is one observation. 

data %>% 
  filter(COUNT == "-99999" |
           COUNT == 0) %>% 
  dplyr::count()

#this helps to show that our data looks pretty inflated row-wise, when in actuality we don't have that many observations. Almost 90% of the rows are 0 or missing. 

data <- data %>% 
  filter(COUNT != "-99999" &
           COUNT != 0)
  
#final step for counts is that we want ONE ROW to equal ONE FISH. We can do this relatively easily using some nice dplyr tools. 

data <- data %>% 
  uncount(COUNT,
          .remove = T) #the default is to remove the count column after it "expands" since the count won't necessarily represent anything anymore

## dates ------

#here is where I would create a YMD column if necessary if the date was in any other format. 

#Years : 25 (2000-2024)

#Month: ~98% of data comes from July or August

data <- data %>% 
  mutate(DATE = ymd(DATE))

## sites and spatial info --------

#there is site, transect and quad. Do we need to keep each transect and quad? Is everything getting lumped together? 

#there are 11 unique "sites" (these are just subsites!)

#Transect: unique numbers for each site? Dig into further:

unique(data$QUAD)
data %>% 
  distinct(SITE,TRANSECT) %>% 
  arrange(SITE, TRANSECT) #so not all sites have all transects. Is there a better map we need to reference or is this important? 

#Quadrat: somewhat same thing as transect. Do we need this detailed spatial information or can we disregard? Used for benthic survey method only. Describes location along transect (from data metadata)

#do we want to include lat and long? 

#final decision for now: drop transect and quadrat because they are basically just subsite "replicates". The only thing we'll need to consider is how/if that affects our sampling effort. 

data <- data %>% 
  select(!c(TRANSECT, QUAD))

##sample effort ------------------------------------

#across years lumped (no transects)

#this is back to base R, but the output is really appealing 

timeseries <- function(x){length(unique(x))} #function can can count unique years for each station

tapply(data$YEAR, list(data$SITE), timeseries)

#we can look at the same thing visually 

data %>% 
  distinct(YEAR, SITE) %>% 
  ggplot(aes(x = as.factor(YEAR),
             y = SITE,
             color = as.factor(SITE))) +
  geom_point(show.legend = F) +
  labs(x = "",
       y = "Site")

fish <- data %>% 
  select(DATE, SITE, SP_CODE, SIZE, SCIENTIFIC_NAME, COMMON_NAME,AREA) %>% 
  rename(SUBSITE = SITE,
         EFFORT=AREA) %>% 
  mutate(YEAR = year(DATE))

rm(data)

#quick look at distribution of counts
count_check <- data.frame(fish %>% group_by(DATE, SCIENTIFIC_NAME) %>% 
                            reframe(count=n())) # count for each species for each sampling event 
hist(count_check$count, breaks = 20) 
summary(count_check)

#at the end of this data you should have DATE, SITE, SP_CODE, SIZE, SCIENTIFIC_NAME, COMMON_NAME

#PART 2: TEMP & DO ----

#same thing here - only un-comment and run this code if you don't have access to the Google Drive where this was uploaded to 
## download data from EDI ---------------------
# 
# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/6006/7/299a4b5019b3d1ceb950614b0955cfd9" 
# infile1 <- tempfile()
# try(download.file(inUrl1,infile1,method="curl"))
# if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
# 
# 
# dt1 <-read.csv(infile1,header=F 
#                ,skip=1
#                ,sep=","  
#                ,quot='"' 
#                , col.names=c(
#                  "site",     
#                  "datetime_UTC",     
#                  "deployment_depth_m",     
#                  "temperature_C",     
#                  "DO_percent_saturation",     
#                  "DO_mgl"    ), check.names=TRUE)
# 
# unlink(infile1)
# 
# # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
# 
# if (class(dt1$site)!="factor") dt1$site<- as.factor(dt1$site)                                   
# if (class(dt1$deployment_depth_m)=="factor") dt1$deployment_depth_m <-as.numeric(levels(dt1$deployment_depth_m))[as.integer(dt1$deployment_depth_m) ]               
# if (class(dt1$deployment_depth_m)=="character") dt1$deployment_depth_m <-as.numeric(dt1$deployment_depth_m)
# if (class(dt1$temperature_C)=="factor") dt1$temperature_C <-as.numeric(levels(dt1$temperature_C))[as.integer(dt1$temperature_C) ]               
# if (class(dt1$temperature_C)=="character") dt1$temperature_C <-as.numeric(dt1$temperature_C)
# if (class(dt1$DO_percent_saturation)=="factor") dt1$DO_percent_saturation <-as.numeric(levels(dt1$DO_percent_saturation))[as.integer(dt1$DO_percent_saturation) ]               
# if (class(dt1$DO_percent_saturation)=="character") dt1$DO_percent_saturation <-as.numeric(dt1$DO_percent_saturation)
# if (class(dt1$DO_mgl)=="factor") dt1$DO_mgl <-as.numeric(levels(dt1$DO_mgl))[as.integer(dt1$DO_mgl) ]               
# if (class(dt1$DO_mgl)=="character") dt1$DO_mgl <-as.numeric(dt1$DO_mgl)
# 
# # Convert Missing Values to NA for non-dates
# 
# dt1$deployment_depth_m <- ifelse((trimws(as.character(dt1$deployment_depth_m))==trimws("NaN")),NA,dt1$deployment_depth_m)               
# suppressWarnings(dt1$deployment_depth_m <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$deployment_depth_m))==as.character(as.numeric("NaN"))),NA,dt1$deployment_depth_m))
# dt1$temperature_C <- ifelse((trimws(as.character(dt1$temperature_C))==trimws("NaN")),NA,dt1$temperature_C)               
# suppressWarnings(dt1$temperature_C <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$temperature_C))==as.character(as.numeric("NaN"))),NA,dt1$temperature_C))
# dt1$DO_percent_saturation <- ifelse((trimws(as.character(dt1$DO_percent_saturation))==trimws("NaN")),NA,dt1$DO_percent_saturation)               
# suppressWarnings(dt1$DO_percent_saturation <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$DO_percent_saturation))==as.character(as.numeric("NaN"))),NA,dt1$DO_percent_saturation))
# dt1$DO_mgl <- ifelse((trimws(as.character(dt1$DO_mgl))==trimws("NaN")),NA,dt1$DO_mgl)               
# suppressWarnings(dt1$DO_mgl <- ifelse(!is.na(as.numeric("NaN")) & (trimws(as.character(dt1$DO_mgl))==as.character(as.numeric("NaN"))),NA,dt1$DO_mgl))
# 
# data<-dt1
# rm(dt1)
# #select just important variables from env data
# data <- data %>% 
#   select(c(site,datetime_UTC,temperature_C,DO_mgl))
# 
# #write file for temperature and do
# write.csv(data,
#           file = file.path("data", 
#                            "raw_data", 
#                            "LTER_SBC", 
#                            "raw_LTER_SBC_do_temp.csv"),
#           row.names = F)

##start here for temp & do if files already download-------


envdata <- read.csv(file = file.path("data",
                                  "raw_data",
                                  "LTER_SBC",
                                  "raw_LTER_SBC_do_temp.csv"))

#match column names and date format with fish

envdata <- envdata %>% 
  rename(SUBSITE = site,
         DATE = datetime_UTC,
         TEMP = temperature_C,
         DO = DO_mgl) %>% 
  mutate(DATE = ymd_hms(DATE),
         DATE = as.Date(DATE),
         YEAR = year(DATE))

setdiff(unique(fish$SUBSITE), unique(envdata$SUBSITE))
sort(unique(fish$SUBSITE))
sort(unique(envdata$SUBSITE))

#I did not subsite by year because there was not a perfect match between site and surface temperature locations in this dataset
#could add to group by and merge if needed though
#for each year obtain:

# VARIABLES
# A) annual average temperature (rational: growing season avg would require identifying growing season at each site)
# B) annual mean daily max temperature
# C) annual mean daily min temperature


annual_TEMP <- envdata %>% 
  group_by(YEAR) %>%
  reframe(mean_daily_temp = mean(TEMP, na.rm = T))  #variable A

daily_TEMP <- drop_na(envdata) %>% 
  group_by(YEAR, DATE) %>% 
  reframe(mean_max_temp = max(TEMP, na.rm = T),
          mean_min_temp = min(TEMP, na.rm = T)) # get daily max & min

daily_TEMP <- daily_TEMP %>% 
  group_by(YEAR) %>% 
  reframe(mean_max_temp = mean(mean_max_temp, na.rm = T),
          mean_min_temp = mean(mean_min_temp, na.rm = T))   # variables B & C 


temp_final <- left_join(daily_TEMP, annual_TEMP)
temp_final$YEAR <- temp_final$YEAR + 1 # offset year before joining to fish data

# VARIABLES
# D) annual average DO (might scratch?)
# E) annual mean daily DO
# F) annual mean min DO


annual_DO <- envdata %>% 
  group_by(YEAR) %>%
  reframe(annual_avg_DO = mean(DO, na.rm = T))  #variable D

#drop_na needed here because some Na's in do effect minimum calculation
daily_DO <- drop_na(envdata) %>% 
  group_by(YEAR, DATE) %>% 
  reframe(mean_daily_DO = mean(DO, na.rm = T),
          mean_min_DO = min(DO, na.rm = T)) # get daily mean & min


daily_DO <- daily_DO %>% 
  group_by(YEAR) %>% 
  reframe(mean_daily_DO = mean(mean_daily_DO, na.rm = T),
          mean_min_DO = mean(mean_min_DO, na.rm = T))   # variables E & F 

#finalize DO
DO_final <- left_join(daily_DO, annual_DO)
DO_final$YEAR <- DO_final$YEAR + 1 # offset year before joining to fish data

#finalize environmental data 

enviro_final <- temp_final %>%
  merge(DO_final, by=c("YEAR"), all = T) # use merge not join--join drops years if temp or DO missing for year

#finalize intermediate data -----


#add name of overall site for when its added to rest of datasite
fish$SITE<-"LTER_SBC"


#remove any rows where the date or subsite is missing
fish<-fish%>%drop_na(c("DATE","SUBSITE"))



#merge fish and environmental data by year 

intermediate <- left_join(fish, enviro_final, by = c("YEAR"))


#Here, you'll want to rename any columns you already fit to have our required column naming conventions

intermediate.names()

intermediate <- intermediate %>% 
  rename(SCI_NAME = SCIENTIFIC_NAME)
 

#this custom function should do the rest 

intermediate.prep(intermediate)

#OPTIONAL VIZ: Play around with subsites by changing to True! The default is false. 

plot.top5(intermediate, species_col = "SCI_NAME", subsite = F)

plot.presence(intermediate, 
              species_col = "COMMON_NAME",
              subsite = F)

plot.speciesaccum(intermediate,
                  species_col = "SP_CODE",
                  subsite = F)



  
  
