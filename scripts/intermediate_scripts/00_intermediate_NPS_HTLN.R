#_________________________________
# Fish Community and Associated Habitat Data From the Heartland Inventory and Monitoring Network: 2001-2023 
# SCALES/ SSECR                  
# Bethany Williams
# R version 4.4.1 (2024-06-14 ucrt)"
# https://doi.org/10.57830/2303237
#_________________________________

# SETUP ---------------

rm(list = ls())

## LOAD PACKAGES AND FUNCTIONS --------------

#install.packages("librarian")

librarian::shelf(supportR, tidyverse, summarytools, datacleanr, lterdatasampler,
                 cowplot, gt, vegan, neonUtilities, splitstackshape)

source(file = file.path("scripts",
                        "functions.R"))

source(file = file.path("scripts",
                        "viz_ideas.R"))


#create directories for project if they don't already exist

intermediate.directories()


# set "site name" based on the name of your raw data folder and what the output should look like for naming convention, This step is critical!  

dataset <- "NPS_HTLN"

# PART #1: FISH  ---------------------

## LOAD FISH DATA ---------------------
#note that this data is not on EDI, so predownload from nps site

data <- read_csv(file = file.path("data", "raw_data","NPS_HTLN",
                                      "HTLN_FishCommunities_FishCountsThru_2023_Cleaned.csv"))

# CHECKS --------------------

#get generic output on data structures and unique values for each - this you can keep! 
# It's set up so that it will name it based on your unique site value you named earlier. 

summarytools::view(summarytools::dfSummary(data),
                   file = file.path("data",
                                    "metadata",
                                    paste0(dataset, 
                                           "_datasummary.html")))

#Period ID weird format (name+date combined)
#total length in mm
#-999 or NA= not collected

## DUPLICATES --------------------

data[duplicated(data),]
data <- data[!duplicated(data),] 

## DROP COLUMNS --------------------

#filter out rows that weren't id'ed to species level
data <- data %>% 
  filter(!TaxonRank=="family"&!TaxonRank=="genus") # drop samples not ID'd to species level 
#filter out unneeded columns
data <- data %>% 
  select(!c(SiteNumber, TaxaFishNumber, Weight_Grams, FishAnomaly, Vouchered,BatchID,BatchWT_Grams,
            Comments,TaxonCode,FamilyName,TSN,VerbatimIdentification,GBIFTaxonID,ReproductiveClassification,
            ScientificNameDataSource,Tolerance,TrophicClassification,dwcType,dwcBasisOfRecord,PeriodID,
            TaxonRank,ScientificName_flag))



## LENGTH --------------------

data$TotalLength_Millimeters[which(data$TotalLength_Millimeters == -999)] <- NA # unmeasured fish recorded as -999 -> change to NA
data %>% 
  filter(is.na(TotalLength_Millimeters)) %>% dplyr::count() # 5007 NA's

## BULK COUNTS ----------------

# From Methods https://irma.nps.gov/DataStore/Reference/Profile/2285001 and 
#https://irma.nps.gov/DataStore/DownloadFile/657920:
# Only the first 30 individuals of a species is measured for total length 

# all fish in a row (i.e., # in numObserved column) have the same length, species, etc

data <- data %>% 
  uncount(NumObs,
          .remove = T) #the default is to remove the count column after it "expands" since the count won't necessarily represent anything anymore


#capitalization not consistent for gear type
data <- data %>% 
  mutate(SamplingGear = str_to_sentence(SamplingGear))
data$SamplingGear<-as.factor(data$SamplingGear)
summary(data$SamplingGear)

#lots of different sampling types
#backpack=102914
#backpack+boat=856
#barge=50177
#boat=46009
#seine=64381
#is barge and boat different?
#looks like from methods backpack, barge, and boat refer to the type of electrofishing versus seining

gearsummary<-data%>% group_by(ParkCode)%>%reframe(SamplingGear=unique(SamplingGear))
aggregate(SamplingGear~ParkCode,data=gearsummary,FUN=length)
#most sites just use one or multiple types of electrofishing
#OZAR uses electrofishing and seining, removed seining from this site only so that each site
#only uses either electrofishing and seining (consistent within each site)

data <- data%>% 
  filter(!(ParkCode=="OZAR"& SamplingGear=="Seine"))


## Effort Data --------------------
effortdata <- read_csv(file = file.path("data", "raw_data","NPS_HTLN",
                                     "HTLN_FishCommunities_TransectSpacingInterval_Cleaned.csv"))
#effort not recorded for a few sampling events
effortdata$ChannelTypeSampledLength_Meters[which(effortdata$ChannelTypeSampledLength_Meters == -999)] <- NA 

#create effort column (# of transects x length of transects)
effortdata$EFFORT<-effortdata$ChannelTypeSampledLength_Meters*effortdata$NumberOfTransects

effortdata <- effortdata %>% 
  select(c(EFFORT,EventID,LocationID))
#note that a few sites didn't have effort data listed so they get removed when this is merged

data<-merge(
  x=data,y=effortdata,by=c("EventID","LocationID"))


## Finish Fish Data --------------------
# at the end of the fish section, we should have:
# DATE, SITE/SUBSITE, SP_CODE, SIZE, SCIENTIFIC_NAME, COMMON_NAME (not for NEON stuff), YEAR, EFFORT

data$EventID<-sub(".*?2","2",data$EventID)

data$EventID <- as_date(data$EventID) # convert to date using lubridate




fish <- data %>% 
  dplyr::rename(DATE = EventID,
                SCI_NAME = ScientificName,
                SIZE = TotalLength_Millimeters,
                SUBSITE = ParkCode,
                COMMON_NAME=CommonName) %>% 
  mutate(YEAR = year(DATE)) %>% 
  select(DATE, SUBSITE, SIZE, SCI_NAME, YEAR, EFFORT,COMMON_NAME)

ggplot(fish, aes(x=DATE, y = SIZE,color=SUBSITE)) +
  geom_point()
fishsummary<-fish%>% group_by(SUBSITE)%>%reframe(YEAR=unique(YEAR))
aggregate(YEAR~SUBSITE,data=fishsummary,FUN=length)
#remove EFMO, and HOSP which have less than 5 years of data

fish <- fish%>% 
  filter(!(SUBSITE=="EFMO"|SUBSITE=="HOSP"))

## ENVIRO Data --------------

envdata <- read_csv(file = file.path("data", "raw_data","NPS_HTLN",
                                  "HTLN_FishCommunities_ReachMeasurements_Cleaned.csv"))


envdata$EventID<-sub(".*?2","2",envdata$EventID)

envdata$EventID <- as_date(envdata$EventID) # convert to date using lubridate
#replace missing values (-999) with NA
envdata$DissolvedOxygen_MilligramsPerLiter[which(envdata$DissolvedOxygen_MilligramsPerLiter == -999)] <- NA
envdata$WaterTemp_Celcius[which(envdata$WaterTemp_Celcius == -999)] <- NA
envdata <- envdata %>% 
  rename(DATE=EventID,
         SUBSITE=ParkCode)%>% 
  mutate(YEAR = year(DATE))

#checking amount of data for each site
ggplot(envdata, aes(x=DATE, y = WaterTemp_Celcius,color=SUBSITE)) +
  geom_point()
ggplot(envdata, aes(x=DATE, y = DissolvedOxygen_MilligramsPerLiter,color=SUBSITE)) +
  geom_point()

tempdata<-envdata%>% 
  select(c(WaterTemp_Celcius,YEAR,SUBSITE))
tempsummary<-tempdata%>% group_by(SUBSITE)%>%reframe(YEAR=unique(YEAR))
aggregate(YEAR~SUBSITE,data=tempsummary,FUN=length)

dodata<-envdata%>% 
  select(c(DissolvedOxygen_MilligramsPerLiter,YEAR,SUBSITE))
dosummary<-dodata%>% group_by(SUBSITE)%>%reframe(YEAR=unique(YEAR))
aggregate(YEAR~SUBSITE,data=dosummary,FUN=length)

# remove EFMO, GWCA, HEHO,HOME,HOSP, and WICR which have less than 5 years of temp and do data

envdata <- envdata%>% 
  filter(!(SUBSITE=="EFMO"|SUBSITE=="HOSP"| SUBSITE=="GWCA"|SUBSITE=="HEHO"|SUBSITE=="HOME"|SUBSITE=="WICR"))

# PART #2: TEMP ------


# VARIABLES
# A) annual average temperature (rational: growing season avg would require identifying growing season at each site)
# B) annual mean daily max temperature
# C) annual mean daily min temperature

tempdata<-envdata%>% 
  select(c(WaterTemp_Celcius,YEAR,SUBSITE,DATE))

temp_final <- drop_na(tempdata) %>% 
  group_by(SUBSITE,YEAR,DATE) %>% 
  reframe(mean_daily_temp = mean(WaterTemp_Celcius, na.rm = T),
          mean_max_temp = max(WaterTemp_Celcius, na.rm = T),
          mean_min_temp = min(WaterTemp_Celcius, na.rm = T))  %>% 
  filter(!mean_daily_temp == "NaN") %>%
  ungroup() %>%
  group_by(SUBSITE,YEAR) %>%
  reframe(mean_daily_temp = mean(mean_daily_temp, na.rm = T),
          mean_max_temp = mean(mean_max_temp, na.rm = T),
          mean_min_temp = mean(mean_min_temp,na.rm = T)) # get variables A, B, C


temp_final$YEAR <- as.numeric(temp_final$YEAR)
temp_final$YEAR <- temp_final$YEAR + 1 # offset year before joining to fish data

## CALCULATE DO VARIABLES --------------------

# VARIABLES
# D) annual average DO 
# E) annual mean daily DO
# F) annual mean min DO


dodata<-envdata%>% 
  select(c(DissolvedOxygen_MilligramsPerLiter,YEAR,SUBSITE,DATE))

daily_DO <- drop_na(dodata) %>% 
  group_by(SUBSITE,YEAR, DATE) %>% 
  reframe(mean_daily_DO = mean(DissolvedOxygen_MilligramsPerLiter, na.rm = T),
          mean_min_DO = min(DissolvedOxygen_MilligramsPerLiter, na.rm = T)) # get daily mean & min

daily_DO <- daily_DO %>% 
  filter(!mean_daily_DO == "NaN") %>%
  filter(!mean_min_DO == "Inf") %>%
  group_by(YEAR,SUBSITE) %>% 
  reframe(mean_daily_DO = mean(mean_daily_DO, na.rm = T),
          mean_min_DO = mean(mean_min_DO, na.rm = T))  # variables E & F 

annual_DO <- drop_na(dodata) %>% 
  group_by(YEAR,SUBSITE) %>%
  reframe(annual_avg_DO = mean(DissolvedOxygen_MilligramsPerLiter, na.rm = T))

DO_final <- merge(daily_DO, annual_DO, by = c("YEAR","SUBSITE"), all = T)


DO_final$YEAR <- as.numeric(DO_final$YEAR)
DO_final$YEAR <- DO_final$YEAR + 1 # offset year before joining to fish data


#finalize environmental data 

enviro_final <- temp_final %>%
  merge(DO_final, by=c("YEAR","SUBSITE"), all = T) # use merge not join--join drops years if temp or DO missing for year


#PART #4: HARMONIZE TEMP & DO with FISH
#use full join because of the offsetting we have some years with just fish data 
#and some with just environmental data
intermediate <- full_join(fish, enviro_final, by = c("YEAR","SUBSITE"))

#fish and environmental data were not collected every year at every site,
#PERI doesn't have environmental data, #PIPE and #TAPR only have fish
#so final dataset only has BUFF and OZAR
intermediate <- intermediate%>% 
  filter(!(SUBSITE=="EFMO"|SUBSITE=="HOSP"| SUBSITE=="GWCA"|SUBSITE=="HEHO"|SUBSITE=="HOME"|SUBSITE=="WICR"|
             SUBSITE== "PERI"|SUBSITE=="PIPE"|SUBSITE=="TAPR"))

ggplot(intermediate, aes(y=SIZE, x = mean_daily_temp,color=SUBSITE)) +
  geom_point()+
  facet_grid(~SUBSITE,scales = "free")

ggplot(intermediate, aes(x=YEAR, y = mean_daily_temp,color=SUBSITE)) +
  geom_point()
ggplot(intermediate, aes(x=YEAR, y = mean_daily_DO,color=SUBSITE)) +
  geom_point()



#Here, you'll want to rename any columns you already fit to have our required column naming conventions

intermediate.names()

intermediate.prep(intermediate)

#OPTIONAL VIZ: Play around with subsites by changing to True! The default is false. 

plot.top5(intermediate, 
          species_col = "COMMON_NAME",
          subsite=F)

plot.presence(intermediate, 
              species_col = "COMMON_NAME",
              subsite = F)

plot.speciesaccum(intermediate,
                  species_col = "COMMON_NAME",
                  subsite = F)
