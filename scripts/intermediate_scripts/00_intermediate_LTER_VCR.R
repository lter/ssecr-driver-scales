#_________________________________
#LTER VCR
# SCALES/ SSECR                  
# Bethany Williams  
# R Version: 4.4.2 (2024-10-31) -- "Pile of Leaves"
#_________________________________

# setup ---------------------

rm(list = ls())

#create directory for LTER summary output and basic graphs

ifelse(!dir.exists(file.path("data", "metadata")), 
       dir.create(file.path("data", "metadata")), 
       FALSE)

#set "site name" based on the name of your raw data folder and what the output should look like for naming convention, This step is critical!  

dataset <- "LTER_VCR"

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

## load fish data ---------------------

#Downloading the data from EDI, note VCR has the fish data in two datasets from 2012-2018 and 2019 
#onwards because of changes in locations and methods
#skip if data already downlaoded
# Package ID: knb-lter-vcr.236.18 Cataloging System:https://pasta.edirepository.org.
# Data set title: Fish Counts and Lengths in South Bay and Hog Island Bay, Virginia 2012-2018.
# Data set creator: Dr. Karen McGlathery -  
# Data set creator:  Max Castorani -  
# Metadata Provider:    - Virginia Coast Reserve Long-Term Ecological Research Project 
# Contact: Dr. Karen McGlathery -    - kjm4k@virginia.edu
# Contact:  Max Castorani -    - castorani@virginia.edu
# Contact:    - Information manager - Virginia Coast Reserve Long-Term Ecological Research Project   - jhp7e@virginia.edu
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



#options(HTTPUserAgent="EDI_CodeGen")
#
#
#inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-vcr/236/18/27549758f3eeecd9be5344562e0340fb" 
#infile1 <- tempfile()
#try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
#if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
#
#
#dt1 <-read.csv(infile1,header=F 
#               ,skip=23
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "SPECIESNAME",     
#                 "SAMPLEDATE",     
#                 "SITE",     
#                 "SAMPLETIME",     
#                 "DISSOLVEDOXYGEN",     
#                 "TEMPERATURE",     
#                 "SALINITY",     
#                 "CONDUCTIVITY",     
#                 "LENGTH",     
#                 "COUNT",     
#                 "SITECOMMENT",     
#                 "OBSERVERS",     
#                 "DATEID",     
#                 "OBSID",     
#                 "LENGTHID",     
#                 "SPECOBSID",     
#                 "MODDATE",     
#                 "SAMPLEDATETIME",     
#                 "scientific_name"    ), check.names=TRUE)
#
#unlink(infile1)
#
## Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#
#if (class(dt1$SPECIESNAME)!="factor") dt1$SPECIESNAME<- as.factor(dt1$SPECIESNAME)                                   
## attempting to convert dt1$SAMPLEDATE dateTime string to R date structure (date or POSIXct)                                
#tmpDateFormat<-"%Y-%m-%d"
#tmp1SAMPLEDATE<-as.Date(dt1$SAMPLEDATE,format=tmpDateFormat)
## Keep the new dates only if they all converted correctly
#if(nrow(dt1[dt1$SAMPLEDATE != "",]) == length(tmp1SAMPLEDATE[!is.na(tmp1SAMPLEDATE)])){dt1$SAMPLEDATE <- tmp1SAMPLEDATE } else {print("Date conversion failed for dt1$SAMPLEDATE. Please inspect the data and do the date conversion yourself.")}                                                                    
#
#if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
#if (class(dt1$SAMPLETIME)!="factor") dt1$SAMPLETIME<- as.factor(dt1$SAMPLETIME)
#if (class(dt1$DISSOLVEDOXYGEN)=="factor") dt1$DISSOLVEDOXYGEN <-as.numeric(levels(dt1$DISSOLVEDOXYGEN))[as.integer(dt1$DISSOLVEDOXYGEN) ]               
#if (class(dt1$DISSOLVEDOXYGEN)=="character") dt1$DISSOLVEDOXYGEN <-as.numeric(dt1$DISSOLVEDOXYGEN)
#if (class(dt1$TEMPERATURE)=="factor") dt1$TEMPERATURE <-as.numeric(levels(dt1$TEMPERATURE))[as.integer(dt1$TEMPERATURE) ]               
#if (class(dt1$TEMPERATURE)=="character") dt1$TEMPERATURE <-as.numeric(dt1$TEMPERATURE)
#if (class(dt1$SALINITY)=="factor") dt1$SALINITY <-as.numeric(levels(dt1$SALINITY))[as.integer(dt1$SALINITY) ]               
#if (class(dt1$SALINITY)=="character") dt1$SALINITY <-as.numeric(dt1$SALINITY)
#if (class(dt1$CONDUCTIVITY)=="factor") dt1$CONDUCTIVITY <-as.numeric(levels(dt1$CONDUCTIVITY))[as.integer(dt1$CONDUCTIVITY) ]               
#if (class(dt1$CONDUCTIVITY)=="character") dt1$CONDUCTIVITY <-as.numeric(dt1$CONDUCTIVITY)
#if (class(dt1$LENGTH)=="factor") dt1$LENGTH <-as.numeric(levels(dt1$LENGTH))[as.integer(dt1$LENGTH) ]               
#if (class(dt1$LENGTH)=="character") dt1$LENGTH <-as.numeric(dt1$LENGTH)
#if (class(dt1$COUNT)=="factor") dt1$COUNT <-as.numeric(levels(dt1$COUNT))[as.integer(dt1$COUNT) ]               
#if (class(dt1$COUNT)=="character") dt1$COUNT <-as.numeric(dt1$COUNT)
#if (class(dt1$SITECOMMENT)!="factor") dt1$SITECOMMENT<- as.factor(dt1$SITECOMMENT)
#if (class(dt1$OBSERVERS)!="factor") dt1$OBSERVERS<- as.factor(dt1$OBSERVERS)
#if (class(dt1$DATEID)=="factor") dt1$DATEID <-as.numeric(levels(dt1$DATEID))[as.integer(dt1$DATEID) ]               
#if (class(dt1$DATEID)=="character") dt1$DATEID <-as.numeric(dt1$DATEID)
#if (class(dt1$OBSID)=="factor") dt1$OBSID <-as.numeric(levels(dt1$OBSID))[as.integer(dt1$OBSID) ]               
#if (class(dt1$OBSID)=="character") dt1$OBSID <-as.numeric(dt1$OBSID)
#if (class(dt1$LENGTHID)=="factor") dt1$LENGTHID <-as.numeric(levels(dt1$LENGTHID))[as.integer(dt1$LENGTHID) ]               
#if (class(dt1$LENGTHID)=="character") dt1$LENGTHID <-as.numeric(dt1$LENGTHID)
#if (class(dt1$SPECOBSID)=="factor") dt1$SPECOBSID <-as.numeric(levels(dt1$SPECOBSID))[as.integer(dt1$SPECOBSID) ]               
#if (class(dt1$SPECOBSID)=="character") dt1$SPECOBSID <-as.numeric(dt1$SPECOBSID)                                   
## attempting to convert dt1$MODDATE dateTime string to R date structure (date or POSIXct)                                
#tmpDateFormat<-"%Y-%m-%d"
#tmp1MODDATE<-as.Date(dt1$MODDATE,format=tmpDateFormat)
## Keep the new dates only if they all converted correctly
#if(nrow(dt1[dt1$MODDATE != "",]) == length(tmp1MODDATE[!is.na(tmp1MODDATE)])){dt1$MODDATE <- tmp1MODDATE } else {print("Date conversion failed for dt1$MODDATE. Please inspect the data and do the date conversion yourself.")}                                                                    
#
## attempting to convert dt1$SAMPLEDATETIME dateTime string to R date structure (date or POSIXct)                                
#tmpDateFormat<-"%Y-%m-%d %H:%M" 
#tmp1SAMPLEDATETIME<-as.POSIXct(dt1$SAMPLEDATETIME,format=tmpDateFormat)
## Keep the new dates only if they all converted correctly
#if(nrow(dt1[dt1$SAMPLEDATETIME != "",]) == length(tmp1SAMPLEDATETIME[!is.na(tmp1SAMPLEDATETIME)])){dt1$SAMPLEDATETIME <- tmp1SAMPLEDATETIME } else {print("Date conversion failed for dt1$SAMPLEDATETIME. Please inspect the data and do the date conversion yourself.")}                                                                    
#
#if (class(dt1$scientific_name)!="factor") dt1$scientific_name<- as.factor(dt1$scientific_name)
#
## Convert Missing Values to NA for non-dates
#
#dt1$SPECIESNAME <- as.factor(ifelse((trimws(as.character(dt1$SPECIESNAME))==trimws("NA")),NA,as.character(dt1$SPECIESNAME)))
#dt1$SITE <- as.factor(ifelse((trimws(as.character(dt1$SITE))==trimws("NA")),NA,as.character(dt1$SITE)))
#dt1$SAMPLETIME <- as.factor(ifelse((trimws(as.character(dt1$SAMPLETIME))==trimws("NA")),NA,as.character(dt1$SAMPLETIME)))
#dt1$DISSOLVEDOXYGEN <- ifelse((trimws(as.character(dt1$DISSOLVEDOXYGEN))==trimws("999")),NA,dt1$DISSOLVEDOXYGEN)               
#suppressWarnings(dt1$DISSOLVEDOXYGEN <- ifelse(!is.na(as.numeric("999")) & (trimws(as.character(dt1$DISSOLVEDOXYGEN))==as.character(as.numeric("999"))),NA,dt1$DISSOLVEDOXYGEN))
#dt1$DISSOLVEDOXYGEN <- ifelse((trimws(as.character(dt1$DISSOLVEDOXYGEN))==trimws("0")),NA,dt1$DISSOLVEDOXYGEN)               
#suppressWarnings(dt1$DISSOLVEDOXYGEN <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$DISSOLVEDOXYGEN))==as.character(as.numeric("0"))),NA,dt1$DISSOLVEDOXYGEN))
#dt1$DISSOLVEDOXYGEN <- ifelse((trimws(as.character(dt1$DISSOLVEDOXYGEN))==trimws("NA")),NA,dt1$DISSOLVEDOXYGEN)               
#suppressWarnings(dt1$DISSOLVEDOXYGEN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DISSOLVEDOXYGEN))==as.character(as.numeric("NA"))),NA,dt1$DISSOLVEDOXYGEN))
#dt1$TEMPERATURE <- ifelse((trimws(as.character(dt1$TEMPERATURE))==trimws("NA")),NA,dt1$TEMPERATURE)               
#suppressWarnings(dt1$TEMPERATURE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TEMPERATURE))==as.character(as.numeric("NA"))),NA,dt1$TEMPERATURE))
#dt1$SALINITY <- ifelse((trimws(as.character(dt1$SALINITY))==trimws("0")),NA,dt1$SALINITY)               
#suppressWarnings(dt1$SALINITY <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$SALINITY))==as.character(as.numeric("0"))),NA,dt1$SALINITY))
#dt1$SALINITY <- ifelse((trimws(as.character(dt1$SALINITY))==trimws("999")),NA,dt1$SALINITY)               
#suppressWarnings(dt1$SALINITY <- ifelse(!is.na(as.numeric("999")) & (trimws(as.character(dt1$SALINITY))==as.character(as.numeric("999"))),NA,dt1$SALINITY))
#dt1$SALINITY <- ifelse((trimws(as.character(dt1$SALINITY))==trimws("NA")),NA,dt1$SALINITY)               
#suppressWarnings(dt1$SALINITY <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SALINITY))==as.character(as.numeric("NA"))),NA,dt1$SALINITY))
#dt1$CONDUCTIVITY <- ifelse((trimws(as.character(dt1$CONDUCTIVITY))==trimws("999")),NA,dt1$CONDUCTIVITY)               
#suppressWarnings(dt1$CONDUCTIVITY <- ifelse(!is.na(as.numeric("999")) & (trimws(as.character(dt1$CONDUCTIVITY))==as.character(as.numeric("999"))),NA,dt1$CONDUCTIVITY))
#dt1$CONDUCTIVITY <- ifelse((trimws(as.character(dt1$CONDUCTIVITY))==trimws("0")),NA,dt1$CONDUCTIVITY)               
#suppressWarnings(dt1$CONDUCTIVITY <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$CONDUCTIVITY))==as.character(as.numeric("0"))),NA,dt1$CONDUCTIVITY))
#dt1$CONDUCTIVITY <- ifelse((trimws(as.character(dt1$CONDUCTIVITY))==trimws("NA")),NA,dt1$CONDUCTIVITY)               
#suppressWarnings(dt1$CONDUCTIVITY <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$CONDUCTIVITY))==as.character(as.numeric("NA"))),NA,dt1$CONDUCTIVITY))
#dt1$LENGTH <- ifelse((trimws(as.character(dt1$LENGTH))==trimws("0")),NA,dt1$LENGTH)               
#suppressWarnings(dt1$LENGTH <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$LENGTH))==as.character(as.numeric("0"))),NA,dt1$LENGTH))
#dt1$LENGTH <- ifelse((trimws(as.character(dt1$LENGTH))==trimws("NA")),NA,dt1$LENGTH)               
#suppressWarnings(dt1$LENGTH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$LENGTH))==as.character(as.numeric("NA"))),NA,dt1$LENGTH))
#dt1$COUNT <- ifelse((trimws(as.character(dt1$COUNT))==trimws("NA")),NA,dt1$COUNT)               
#suppressWarnings(dt1$COUNT <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$COUNT))==as.character(as.numeric("NA"))),NA,dt1$COUNT))
#dt1$DATEID <- ifelse((trimws(as.character(dt1$DATEID))==trimws("0")),NA,dt1$DATEID)               
#suppressWarnings(dt1$DATEID <- ifelse(!is.na(as.numeric("0")) & (trimws(as.character(dt1$DATEID))==as.character(as.numeric("0"))),NA,dt1$DATEID))
#dt1$scientific_name <- as.factor(ifelse((trimws(as.character(dt1$scientific_name))==trimws("NA")),NA,as.character(dt1$scientific_name)))
#
#
## 2019 onwards
## Package ID: knb-lter-vcr.367.8 Cataloging System:https://pasta.edirepository.org.
## Data set title: Abundance and Size of Seagrass-Associated Fishes in the Virginia Coastal Lagoons, 2019-2024.
## Data set creator:  Max Castorani -  
## Metadata Provider:    - Virginia Coast Reserve Long-Term Ecological Research Project 
## Contact:  Max Castorani -    - castorani@virginia.edu
## Contact:    - Information manager - Virginia Coast Reserve Long-Term Ecological Research Project   - jhp7e@virginia.edu
## Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
## Uncomment the following lines to have R clear previous work, or set a working directory
## rm(list=ls())      
#
## setwd("C:/users/my_name/my_dir")       
#
#
#inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-vcr/367/8/db53a90bb8f3535240c498db96e858f6" 
#infile2 <- tempfile()
#try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
#if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")
#
#
#dt2 <-read.csv(infile2,header=F 
#               ,skip=23
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "year",     
#                 "month",     
#                 "date",     
#                 "time",     
#                 "iso_date_time",     
#                 "site",     
#                 "fish_site",     
#                 "depth",     
#                 "temperature",     
#                 "salinity",     
#                 "conductivity",     
#                 "dissolved_oxygen",     
#                 "taxon_code",     
#                 "length",     
#                 "common_name",     
#                 "scientific_name",     
#                 "taxon_kingdom",     
#                 "taxon_phylum",     
#                 "taxon_class",     
#                 "taxon_order",     
#                 "taxon_family",     
#                 "taxon_genus",     
#                 "taxon_species",     
#                 "taxon_tsn",     
#                 "comment"    ), check.names=TRUE)
#
#unlink(infile2)
#
## Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#
#if (class(dt2$year)!="factor") dt2$year<- as.factor(dt2$year)
#if (class(dt2$month)=="factor") dt2$month <-as.numeric(levels(dt2$month))[as.integer(dt2$month) ]               
#if (class(dt2$month)=="character") dt2$month <-as.numeric(dt2$month)                                   
## attempting to convert dt2$date dateTime string to R date structure (date or POSIXct)                                
#tmpDateFormat<-"%Y-%m-%d"
#tmp2date<-as.Date(dt2$date,format=tmpDateFormat)
## Keep the new dates only if they all converted correctly
#if(nrow(dt2[dt2$date != "",]) == length(tmp2date[!is.na(tmp2date)])){dt2$date <- tmp2date } else {print("Date conversion failed for dt2$date. Please inspect the data and do the date conversion yourself.")}                                                                    
#
#if (class(dt2$time)!="factor") dt2$time<- as.factor(dt2$time)                                   
## attempting to convert dt2$iso_date_time dateTime string to R date structure (date or POSIXct)                                
#tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
#tmp2iso_date_time<-as.POSIXct(dt2$iso_date_time,format=tmpDateFormat)
## Keep the new dates only if they all converted correctly
#if(nrow(dt2[dt2$iso_date_time != "",]) == length(tmp2iso_date_time[!is.na(tmp2iso_date_time)])){dt2$iso_date_time <- tmp2iso_date_time } else {print("Date conversion failed for dt2$iso_date_time. Please inspect the data and do the date conversion yourself.")}                                                                    
#
#if (class(dt2$site)!="factor") dt2$site<- as.factor(dt2$site)
#if (class(dt2$fish_site)!="factor") dt2$fish_site<- as.factor(dt2$fish_site)
#if (class(dt2$depth)=="factor") dt2$depth <-as.numeric(levels(dt2$depth))[as.integer(dt2$depth) ]               
#if (class(dt2$depth)=="character") dt2$depth <-as.numeric(dt2$depth)
#if (class(dt2$temperature)=="factor") dt2$temperature <-as.numeric(levels(dt2$temperature))[as.integer(dt2$temperature) ]               
#if (class(dt2$temperature)=="character") dt2$temperature <-as.numeric(dt2$temperature)
#if (class(dt2$salinity)=="factor") dt2$salinity <-as.numeric(levels(dt2$salinity))[as.integer(dt2$salinity) ]               
#if (class(dt2$salinity)=="character") dt2$salinity <-as.numeric(dt2$salinity)
#if (class(dt2$conductivity)=="factor") dt2$conductivity <-as.numeric(levels(dt2$conductivity))[as.integer(dt2$conductivity) ]               
#if (class(dt2$conductivity)=="character") dt2$conductivity <-as.numeric(dt2$conductivity)
#if (class(dt2$dissolved_oxygen)=="factor") dt2$dissolved_oxygen <-as.numeric(levels(dt2$dissolved_oxygen))[as.integer(dt2$dissolved_oxygen) ]               
#if (class(dt2$dissolved_oxygen)=="character") dt2$dissolved_oxygen <-as.numeric(dt2$dissolved_oxygen)
#if (class(dt2$taxon_code)!="factor") dt2$taxon_code<- as.factor(dt2$taxon_code)
#if (class(dt2$length)=="factor") dt2$length <-as.numeric(levels(dt2$length))[as.integer(dt2$length) ]               
#if (class(dt2$length)=="character") dt2$length <-as.numeric(dt2$length)
#if (class(dt2$common_name)!="factor") dt2$common_name<- as.factor(dt2$common_name)
#if (class(dt2$scientific_name)!="factor") dt2$scientific_name<- as.factor(dt2$scientific_name)
#if (class(dt2$taxon_kingdom)!="factor") dt2$taxon_kingdom<- as.factor(dt2$taxon_kingdom)
#if (class(dt2$taxon_phylum)!="factor") dt2$taxon_phylum<- as.factor(dt2$taxon_phylum)
#if (class(dt2$taxon_class)!="factor") dt2$taxon_class<- as.factor(dt2$taxon_class)
#if (class(dt2$taxon_order)!="factor") dt2$taxon_order<- as.factor(dt2$taxon_order)
#if (class(dt2$taxon_family)!="factor") dt2$taxon_family<- as.factor(dt2$taxon_family)
#if (class(dt2$taxon_genus)!="factor") dt2$taxon_genus<- as.factor(dt2$taxon_genus)
#if (class(dt2$taxon_species)!="factor") dt2$taxon_species<- as.factor(dt2$taxon_species)
#if (class(dt2$taxon_tsn)!="factor") dt2$taxon_tsn<- as.factor(dt2$taxon_tsn)
#if (class(dt2$comment)!="factor") dt2$comment<- as.factor(dt2$comment)
#
## Convert Missing Values to NA for non-dates
#
#dt2$year <- as.factor(ifelse((trimws(as.character(dt2$year))==trimws("NA")),NA,as.character(dt2$year)))
#dt2$month <- ifelse((trimws(as.character(dt2$month))==trimws("NA")),NA,dt2$month)               
#suppressWarnings(dt2$month <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$month))==as.character(as.numeric("NA"))),NA,dt2$month))
#dt2$time <- as.factor(ifelse((trimws(as.character(dt2$time))==trimws("NA")),NA,as.character(dt2$time)))
#dt2$site <- as.factor(ifelse((trimws(as.character(dt2$site))==trimws("NA")),NA,as.character(dt2$site)))
#dt2$fish_site <- as.factor(ifelse((trimws(as.character(dt2$fish_site))==trimws("NA")),NA,as.character(dt2$fish_site)))
#dt2$depth <- ifelse((trimws(as.character(dt2$depth))==trimws("NA")),NA,dt2$depth)               
#suppressWarnings(dt2$depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$depth))==as.character(as.numeric("NA"))),NA,dt2$depth))
#dt2$temperature <- ifelse((trimws(as.character(dt2$temperature))==trimws("NA")),NA,dt2$temperature)               
#suppressWarnings(dt2$temperature <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$temperature))==as.character(as.numeric("NA"))),NA,dt2$temperature))
#dt2$salinity <- ifelse((trimws(as.character(dt2$salinity))==trimws("NA")),NA,dt2$salinity)               
#suppressWarnings(dt2$salinity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$salinity))==as.character(as.numeric("NA"))),NA,dt2$salinity))
#dt2$conductivity <- ifelse((trimws(as.character(dt2$conductivity))==trimws("NA")),NA,dt2$conductivity)               
#suppressWarnings(dt2$conductivity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$conductivity))==as.character(as.numeric("NA"))),NA,dt2$conductivity))
#dt2$dissolved_oxygen <- ifelse((trimws(as.character(dt2$dissolved_oxygen))==trimws("NA")),NA,dt2$dissolved_oxygen)               
#suppressWarnings(dt2$dissolved_oxygen <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$dissolved_oxygen))==as.character(as.numeric("NA"))),NA,dt2$dissolved_oxygen))
#dt2$taxon_code <- as.factor(ifelse((trimws(as.character(dt2$taxon_code))==trimws("NA")),NA,as.character(dt2$taxon_code)))
#dt2$length <- ifelse((trimws(as.character(dt2$length))==trimws("NA")),NA,dt2$length)               
#suppressWarnings(dt2$length <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$length))==as.character(as.numeric("NA"))),NA,dt2$length))
#dt2$taxon_tsn <- as.factor(ifelse((trimws(as.character(dt2$taxon_tsn))==trimws("NA")),NA,as.character(dt2$taxon_tsn)))
#
#
#
#
##combine two datsets
#dt2<-dt2 %>% 
#  rename(SAMPLEDATE=date,
#         LENGTH=length,
#         SPECIESNAME=common_name,
#         SITE=fish_site)
#
##final step for counts is that we want ONE ROW to equal ONE FISH. Dt2 is already like this but dt1 isn't
#
#dt1 <- dt1 %>% 
#  uncount(COUNT,
#          .remove = T) #the default is to remove the count column after it "expands" since the count won't necessarily represent anything anymore
#data<-bind_rows(dt1,dt2)
#
##write csv
# write.csv(data,
#            file = file.path("data",
#                             "raw_data",                            
#                             "LTER_VCR",
#                             "raw_LTER_VCR_fish.csv"),
#           row.names = F)  
# rm(data)
#rm(dt1)
#rm(dt2)
#
#read in data - make sure to download from Google Drive

data <- read.csv(file = file.path("data",
                                  "raw_data",
                                  "LTER_VCR",
                                  "raw_LTER_VCR_fish.csv"))

#PART 1: FISH -----

# checks --------------------

#get generic output on data structures and unique values for each - this you can keep! It's set up so that it will name it based on your unique site value you named earlier. 

summarytools::view(summarytools::dfSummary(data),
                   file = file.path("data",
                                    "metadata",
                                    paste0(dataset, 
                                           "_datasummary.html")))

## dates ------

#here is where I would create a YMD column if necessary if the date was in any other format. 


data <- data %>% 
  mutate(DATE = ymd(SAMPLEDATE),.keep = "unused",
         YEAR = year(DATE))

## sites and spatial info --------
#multiple transects, of 25m seines along a 25m transect (625m^2) for each area
data$EFFORT<-625


##sample effort ------------------------------------



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
  select(SPECIESNAME,DATE,YEAR,EFFORT,SPECIESNAME,SITE,LENGTH,scientific_name,taxon_code) %>% 
  rename(SUBSITE = SITE,
         SCI_NAME=scientific_name,
         SIZE=LENGTH,
         SP_CODE=taxon_code,
         COMMON_NAME=SPECIESNAME)

rm(data)

#fish from 2019 onwards were measured in cm, need to change those years to mm

fish<-fish %>%
  mutate(SIZE=ifelse(YEAR>=2019,SIZE*10,SIZE))

#quick look at distribution of counts
count_check <- data.frame(fish %>% group_by(DATE, SCI_NAME) %>% 
                            reframe(count=n())) # count for each species for each sampling event 
hist(count_check$count, breaks = 20) 
summary(count_check)

#at the end of this data you should have DATE, SUBSITE, SP_CODE, SIZE, SCIENTIFIC_NAME, COMMON_NAME, EFFORT

#PART 2: TEMP and DO  ----

#same thing here - only un-comment and run this code if you don't have access to the Google Drive where this was uploaded to 
#download data from edi, note that there are multiple locations across the VCR that are far away from fish were sampled so these 


#temp and do also over a longer time frame (1992 started, so removed the extra data)

# Package ID: knb-lter-vcr.247.18 Cataloging System:https://pasta.edirepository.org.
# Data set title: Water Quality Sampling - integrated measurements for the Virginia Coast, 1992-2023.
# Data set creator: Dr. Karen McGlathery -  
# Data set creator: Dr. Robert Christian -  
# Metadata Provider:    - Virginia Coast Reserve Long-Term Ecological Research Project 
# Contact: Dr. Karen McGlathery -    - kjm4k@virginia.edu
# Contact:    - Information manager - Virginia Coast Reserve Long-Term Ecological Research Project   - jhp7e@virginia.edu
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       


#
#options(HTTPUserAgent="EDI_CodeGen")
#
#
#inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-vcr/247/18/ce44ee0ccaccf489f3ccb04d3cf85e39" 
#infile1 <- tempfile()
#try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
#if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
#
#
#dt1 <-read.csv(infile1,header=F 
#               ,skip=23
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "STATION",     
#                 "SESSIONDATE",     
#                 "STAGRPCODE",     
#                 "STATYPE",     
#                 "TRANSECT",     
#                 "LATITUDE",     
#                 "LONGITUDE",     
#                 "MEASUREDATE",     
#                 "TIME",     
#                 "ANALYST",     
#                 "TIDESTAGE",     
#                 "SCTTEMP",     
#                 "SCTSAL",     
#                 "SCTCOND",     
#                 "AIRTEMP",     
#                 "REFRSAL",     
#                 "DOTEMP",     
#                 "DO",     
#                 "SECCHI",     
#                 "DEPTH",     
#                 "WINDDIR",     
#                 "WINDSP",     
#                 "COMMENTOBS",     
#                 "CHLA",     
#                 "CHLA_SD",     
#                 "CHLA_N",     
#                 "CHLA_QUAL",     
#                 "PHAEOPIGMENTS",     
#                 "PHAEOPIGMENTS_SD",     
#                 "PHAEOPIGMENTS_N",     
#                 "PHAEOPIGMENTS_QUAL",     
#                 "NH4",     
#                 "NH4_SD",     
#                 "NH4_N",     
#                 "NH4QUAL",     
#                 "PO4",     
#                 "PO4_SD",     
#                 "PO4_N",     
#                 "PO4QUAL",     
#                 "NO3_NO2",     
#                 "NO3_NO2_SD",     
#                 "NO3_NO2_N",     
#                 "NO3_NO2QUAL",     
#                 "COMMENTNUTR",     
#                 "TDN",     
#                 "TDN_SD",     
#                 "TDN_N",     
#                 "TDNQUAL",     
#                 "COMMENTTDN",     
#                 "PROCESSORTDN",     
#                 "ORGANIC_CONTENT",     
#                 "ORGANIC_CONTENT_SD",     
#                 "ORGANIC_CONTENT_N",     
#                 "OC_QUAL",     
#                 "POROSITY",     
#                 "POROSITY_SD",     
#                 "POROSITY_N",     
#                 "POROSITY_QUAL",     
#                 "COMMENTS_ORGANIC_CONTENT",     
#                 "CHLOR_A",     
#                 "CHLOR_A_SD",     
#                 "CHLOR_A_N",     
#                 "CHLOR_AQUAL",     
#                 "PHAEOPIGMENTS_A",     
#                 "PHAEOPIGMENTS_A_SD",     
#                 "PHAEOPIGMENTS_A_N",     
#                 "PHAEOPIGMENTS_AQUAL",     
#                 "COMMENTWATERCHL",     
#                 "GRACILARIA_MASS",     
#                 "GRACILARIA_SD",     
#                 "GRACILARIA_N",     
#                 "GRACILARIA_QUAL",     
#                 "ULVA_MASS",     
#                 "ULVA_SD",     
#                 "ULVA_N",     
#                 "ULVA_QUAL",     
#                 "COMMENTMACROALGAE",     
#                 "TSS",     
#                 "TSS_SD",     
#                 "TSS_N",     
#                 "TSSQUAL",     
#                 "POM",     
#                 "POM_SD",     
#                 "POM_N",     
#                 "POMQUAL",     
#                 "PIM",     
#                 "PIM_SD",     
#                 "PIM_N",     
#                 "PIMQUAL",     
#                 "COMMENTTSS",     
#                 "SEDMASS",     
#                 "SEDMASS_SD",     
#                 "SEDMASS_N",     
#                 "SEDMASSQUAL",     
#                 "SEDPERC_N",     
#                 "SEDPERC_N_SD",     
#                 "SEDPERC_N_N",     
#                 "SEDPERC_NQUAL",     
#                 "SEDPERC_C",     
#                 "SEDPERC_C_SD",     
#                 "SEDPERC_C_N",     
#                 "SEDPERC_CQUAL",     
#                 "COMMENTSEDCN"    ), check.names=TRUE)
#
#unlink(infile1)
#
## Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#
#if (class(dt1$STATION)!="factor") dt1$STATION<- as.factor(dt1$STATION)                                   
## attempting to convert dt1$SESSIONDATE dateTime string to R date structure (date or POSIXct)                                
#tmpDateFormat<-"%Y-%m-%d"
#tmp1SESSIONDATE<-as.Date(dt1$SESSIONDATE,format=tmpDateFormat)
## Keep the new dates only if they all converted correctly
#if(nrow(dt1[dt1$SESSIONDATE != "",]) == length(tmp1SESSIONDATE[!is.na(tmp1SESSIONDATE)])){dt1$SESSIONDATE <- tmp1SESSIONDATE } else {print("Date conversion failed for dt1$SESSIONDATE. Please inspect the data and do the date conversion yourself.")}                                                                    
#
#if (class(dt1$STAGRPCODE)!="factor") dt1$STAGRPCODE<- as.factor(dt1$STAGRPCODE)
#if (class(dt1$STATYPE)!="factor") dt1$STATYPE<- as.factor(dt1$STATYPE)
#if (class(dt1$TRANSECT)!="factor") dt1$TRANSECT<- as.factor(dt1$TRANSECT)
#if (class(dt1$LATITUDE)=="factor") dt1$LATITUDE <-as.numeric(levels(dt1$LATITUDE))[as.integer(dt1$LATITUDE) ]               
#if (class(dt1$LATITUDE)=="character") dt1$LATITUDE <-as.numeric(dt1$LATITUDE)
#if (class(dt1$LONGITUDE)=="factor") dt1$LONGITUDE <-as.numeric(levels(dt1$LONGITUDE))[as.integer(dt1$LONGITUDE) ]               
#if (class(dt1$LONGITUDE)=="character") dt1$LONGITUDE <-as.numeric(dt1$LONGITUDE)                                   
## attempting to convert dt1$MEASUREDATE dateTime string to R date structure (date or POSIXct)                                
#tmpDateFormat<-"%Y-%m-%d"
#tmp1MEASUREDATE<-as.Date(dt1$MEASUREDATE,format=tmpDateFormat)
## Keep the new dates only if they all converted correctly
#if(nrow(dt1[dt1$MEASUREDATE != "",]) == length(tmp1MEASUREDATE[!is.na(tmp1MEASUREDATE)])){dt1$MEASUREDATE <- tmp1MEASUREDATE } else {print("Date conversion failed for dt1$MEASUREDATE. Please inspect the data and do the date conversion yourself.")}                                                                    
#
#if (class(dt1$TIME)=="factor") dt1$TIME <-as.numeric(levels(dt1$TIME))[as.integer(dt1$TIME) ]               
#if (class(dt1$TIME)=="character") dt1$TIME <-as.numeric(dt1$TIME)
#if (class(dt1$ANALYST)!="factor") dt1$ANALYST<- as.factor(dt1$ANALYST)
#if (class(dt1$TIDESTAGE)!="factor") dt1$TIDESTAGE<- as.factor(dt1$TIDESTAGE)
#if (class(dt1$SCTTEMP)=="factor") dt1$SCTTEMP <-as.numeric(levels(dt1$SCTTEMP))[as.integer(dt1$SCTTEMP) ]               
#if (class(dt1$SCTTEMP)=="character") dt1$SCTTEMP <-as.numeric(dt1$SCTTEMP)
#if (class(dt1$SCTSAL)=="factor") dt1$SCTSAL <-as.numeric(levels(dt1$SCTSAL))[as.integer(dt1$SCTSAL) ]               
#if (class(dt1$SCTSAL)=="character") dt1$SCTSAL <-as.numeric(dt1$SCTSAL)
#if (class(dt1$SCTCOND)=="factor") dt1$SCTCOND <-as.numeric(levels(dt1$SCTCOND))[as.integer(dt1$SCTCOND) ]               
#if (class(dt1$SCTCOND)=="character") dt1$SCTCOND <-as.numeric(dt1$SCTCOND)
#if (class(dt1$AIRTEMP)=="factor") dt1$AIRTEMP <-as.numeric(levels(dt1$AIRTEMP))[as.integer(dt1$AIRTEMP) ]               
#if (class(dt1$AIRTEMP)=="character") dt1$AIRTEMP <-as.numeric(dt1$AIRTEMP)
#if (class(dt1$REFRSAL)=="factor") dt1$REFRSAL <-as.numeric(levels(dt1$REFRSAL))[as.integer(dt1$REFRSAL) ]               
#if (class(dt1$REFRSAL)=="character") dt1$REFRSAL <-as.numeric(dt1$REFRSAL)
#if (class(dt1$DOTEMP)=="factor") dt1$DOTEMP <-as.numeric(levels(dt1$DOTEMP))[as.integer(dt1$DOTEMP) ]               
#if (class(dt1$DOTEMP)=="character") dt1$DOTEMP <-as.numeric(dt1$DOTEMP)
#if (class(dt1$DO)=="factor") dt1$DO <-as.numeric(levels(dt1$DO))[as.integer(dt1$DO) ]               
#if (class(dt1$DO)=="character") dt1$DO <-as.numeric(dt1$DO)
#if (class(dt1$SECCHI)=="factor") dt1$SECCHI <-as.numeric(levels(dt1$SECCHI))[as.integer(dt1$SECCHI) ]               
#if (class(dt1$SECCHI)=="character") dt1$SECCHI <-as.numeric(dt1$SECCHI)
#if (class(dt1$DEPTH)=="factor") dt1$DEPTH <-as.numeric(levels(dt1$DEPTH))[as.integer(dt1$DEPTH) ]               
#if (class(dt1$DEPTH)=="character") dt1$DEPTH <-as.numeric(dt1$DEPTH)
#if (class(dt1$WINDDIR)!="factor") dt1$WINDDIR<- as.factor(dt1$WINDDIR)
#if (class(dt1$WINDSP)=="factor") dt1$WINDSP <-as.numeric(levels(dt1$WINDSP))[as.integer(dt1$WINDSP) ]               
#if (class(dt1$WINDSP)=="character") dt1$WINDSP <-as.numeric(dt1$WINDSP)
#if (class(dt1$COMMENTOBS)!="factor") dt1$COMMENTOBS<- as.factor(dt1$COMMENTOBS)
#if (class(dt1$CHLA)=="factor") dt1$CHLA <-as.numeric(levels(dt1$CHLA))[as.integer(dt1$CHLA) ]               
#if (class(dt1$CHLA)=="character") dt1$CHLA <-as.numeric(dt1$CHLA)
#if (class(dt1$CHLA_SD)=="factor") dt1$CHLA_SD <-as.numeric(levels(dt1$CHLA_SD))[as.integer(dt1$CHLA_SD) ]               
#if (class(dt1$CHLA_SD)=="character") dt1$CHLA_SD <-as.numeric(dt1$CHLA_SD)
#if (class(dt1$CHLA_N)=="factor") dt1$CHLA_N <-as.numeric(levels(dt1$CHLA_N))[as.integer(dt1$CHLA_N) ]               
#if (class(dt1$CHLA_N)=="character") dt1$CHLA_N <-as.numeric(dt1$CHLA_N)
#if (class(dt1$CHLA_QUAL)!="factor") dt1$CHLA_QUAL<- as.factor(dt1$CHLA_QUAL)
#if (class(dt1$PHAEOPIGMENTS)=="factor") dt1$PHAEOPIGMENTS <-as.numeric(levels(dt1$PHAEOPIGMENTS))[as.integer(dt1$PHAEOPIGMENTS) ]               
#if (class(dt1$PHAEOPIGMENTS)=="character") dt1$PHAEOPIGMENTS <-as.numeric(dt1$PHAEOPIGMENTS)
#if (class(dt1$PHAEOPIGMENTS_SD)=="factor") dt1$PHAEOPIGMENTS_SD <-as.numeric(levels(dt1$PHAEOPIGMENTS_SD))[as.integer(dt1$PHAEOPIGMENTS_SD) ]               
#if (class(dt1$PHAEOPIGMENTS_SD)=="character") dt1$PHAEOPIGMENTS_SD <-as.numeric(dt1$PHAEOPIGMENTS_SD)
#if (class(dt1$PHAEOPIGMENTS_N)=="factor") dt1$PHAEOPIGMENTS_N <-as.numeric(levels(dt1$PHAEOPIGMENTS_N))[as.integer(dt1$PHAEOPIGMENTS_N) ]               
#if (class(dt1$PHAEOPIGMENTS_N)=="character") dt1$PHAEOPIGMENTS_N <-as.numeric(dt1$PHAEOPIGMENTS_N)
#if (class(dt1$PHAEOPIGMENTS_QUAL)!="factor") dt1$PHAEOPIGMENTS_QUAL<- as.factor(dt1$PHAEOPIGMENTS_QUAL)
#if (class(dt1$NH4)=="factor") dt1$NH4 <-as.numeric(levels(dt1$NH4))[as.integer(dt1$NH4) ]               
#if (class(dt1$NH4)=="character") dt1$NH4 <-as.numeric(dt1$NH4)
#if (class(dt1$NH4_SD)=="factor") dt1$NH4_SD <-as.numeric(levels(dt1$NH4_SD))[as.integer(dt1$NH4_SD) ]               
#if (class(dt1$NH4_SD)=="character") dt1$NH4_SD <-as.numeric(dt1$NH4_SD)
#if (class(dt1$NH4_N)=="factor") dt1$NH4_N <-as.numeric(levels(dt1$NH4_N))[as.integer(dt1$NH4_N) ]               
#if (class(dt1$NH4_N)=="character") dt1$NH4_N <-as.numeric(dt1$NH4_N)
#if (class(dt1$NH4QUAL)!="factor") dt1$NH4QUAL<- as.factor(dt1$NH4QUAL)
#if (class(dt1$PO4)=="factor") dt1$PO4 <-as.numeric(levels(dt1$PO4))[as.integer(dt1$PO4) ]               
#if (class(dt1$PO4)=="character") dt1$PO4 <-as.numeric(dt1$PO4)
#if (class(dt1$PO4_SD)=="factor") dt1$PO4_SD <-as.numeric(levels(dt1$PO4_SD))[as.integer(dt1$PO4_SD) ]               
#if (class(dt1$PO4_SD)=="character") dt1$PO4_SD <-as.numeric(dt1$PO4_SD)
#if (class(dt1$PO4_N)=="factor") dt1$PO4_N <-as.numeric(levels(dt1$PO4_N))[as.integer(dt1$PO4_N) ]               
#if (class(dt1$PO4_N)=="character") dt1$PO4_N <-as.numeric(dt1$PO4_N)
#if (class(dt1$PO4QUAL)!="factor") dt1$PO4QUAL<- as.factor(dt1$PO4QUAL)
#if (class(dt1$NO3_NO2)=="factor") dt1$NO3_NO2 <-as.numeric(levels(dt1$NO3_NO2))[as.integer(dt1$NO3_NO2) ]               
#if (class(dt1$NO3_NO2)=="character") dt1$NO3_NO2 <-as.numeric(dt1$NO3_NO2)
#if (class(dt1$NO3_NO2_SD)=="factor") dt1$NO3_NO2_SD <-as.numeric(levels(dt1$NO3_NO2_SD))[as.integer(dt1$NO3_NO2_SD) ]               
#if (class(dt1$NO3_NO2_SD)=="character") dt1$NO3_NO2_SD <-as.numeric(dt1$NO3_NO2_SD)
#if (class(dt1$NO3_NO2_N)=="factor") dt1$NO3_NO2_N <-as.numeric(levels(dt1$NO3_NO2_N))[as.integer(dt1$NO3_NO2_N) ]               
#if (class(dt1$NO3_NO2_N)=="character") dt1$NO3_NO2_N <-as.numeric(dt1$NO3_NO2_N)
#if (class(dt1$NO3_NO2QUAL)!="factor") dt1$NO3_NO2QUAL<- as.factor(dt1$NO3_NO2QUAL)
#if (class(dt1$COMMENTNUTR)!="factor") dt1$COMMENTNUTR<- as.factor(dt1$COMMENTNUTR)
#if (class(dt1$TDN)=="factor") dt1$TDN <-as.numeric(levels(dt1$TDN))[as.integer(dt1$TDN) ]               
#if (class(dt1$TDN)=="character") dt1$TDN <-as.numeric(dt1$TDN)
#if (class(dt1$TDN_SD)=="factor") dt1$TDN_SD <-as.numeric(levels(dt1$TDN_SD))[as.integer(dt1$TDN_SD) ]               
#if (class(dt1$TDN_SD)=="character") dt1$TDN_SD <-as.numeric(dt1$TDN_SD)
#if (class(dt1$TDN_N)=="factor") dt1$TDN_N <-as.numeric(levels(dt1$TDN_N))[as.integer(dt1$TDN_N) ]               
#if (class(dt1$TDN_N)=="character") dt1$TDN_N <-as.numeric(dt1$TDN_N)
#if (class(dt1$TDNQUAL)!="factor") dt1$TDNQUAL<- as.factor(dt1$TDNQUAL)
#if (class(dt1$COMMENTTDN)!="factor") dt1$COMMENTTDN<- as.factor(dt1$COMMENTTDN)
#if (class(dt1$PROCESSORTDN)!="factor") dt1$PROCESSORTDN<- as.factor(dt1$PROCESSORTDN)
#if (class(dt1$ORGANIC_CONTENT)=="factor") dt1$ORGANIC_CONTENT <-as.numeric(levels(dt1$ORGANIC_CONTENT))[as.integer(dt1$ORGANIC_CONTENT) ]               
#if (class(dt1$ORGANIC_CONTENT)=="character") dt1$ORGANIC_CONTENT <-as.numeric(dt1$ORGANIC_CONTENT)
#if (class(dt1$ORGANIC_CONTENT_SD)=="factor") dt1$ORGANIC_CONTENT_SD <-as.numeric(levels(dt1$ORGANIC_CONTENT_SD))[as.integer(dt1$ORGANIC_CONTENT_SD) ]               
#if (class(dt1$ORGANIC_CONTENT_SD)=="character") dt1$ORGANIC_CONTENT_SD <-as.numeric(dt1$ORGANIC_CONTENT_SD)
#if (class(dt1$ORGANIC_CONTENT_N)=="factor") dt1$ORGANIC_CONTENT_N <-as.numeric(levels(dt1$ORGANIC_CONTENT_N))[as.integer(dt1$ORGANIC_CONTENT_N) ]               
#if (class(dt1$ORGANIC_CONTENT_N)=="character") dt1$ORGANIC_CONTENT_N <-as.numeric(dt1$ORGANIC_CONTENT_N)
#if (class(dt1$OC_QUAL)!="factor") dt1$OC_QUAL<- as.factor(dt1$OC_QUAL)
#if (class(dt1$POROSITY)=="factor") dt1$POROSITY <-as.numeric(levels(dt1$POROSITY))[as.integer(dt1$POROSITY) ]               
#if (class(dt1$POROSITY)=="character") dt1$POROSITY <-as.numeric(dt1$POROSITY)
#if (class(dt1$POROSITY_SD)=="factor") dt1$POROSITY_SD <-as.numeric(levels(dt1$POROSITY_SD))[as.integer(dt1$POROSITY_SD) ]               
#if (class(dt1$POROSITY_SD)=="character") dt1$POROSITY_SD <-as.numeric(dt1$POROSITY_SD)
#if (class(dt1$POROSITY_N)=="factor") dt1$POROSITY_N <-as.numeric(levels(dt1$POROSITY_N))[as.integer(dt1$POROSITY_N) ]               
#if (class(dt1$POROSITY_N)=="character") dt1$POROSITY_N <-as.numeric(dt1$POROSITY_N)
#if (class(dt1$POROSITY_QUAL)=="factor") dt1$POROSITY_QUAL <-as.numeric(levels(dt1$POROSITY_QUAL))[as.integer(dt1$POROSITY_QUAL) ]               
#if (class(dt1$POROSITY_QUAL)=="character") dt1$POROSITY_QUAL <-as.numeric(dt1$POROSITY_QUAL)
#if (class(dt1$COMMENTS_ORGANIC_CONTENT)!="factor") dt1$COMMENTS_ORGANIC_CONTENT<- as.factor(dt1$COMMENTS_ORGANIC_CONTENT)
#if (class(dt1$CHLOR_A)=="factor") dt1$CHLOR_A <-as.numeric(levels(dt1$CHLOR_A))[as.integer(dt1$CHLOR_A) ]               
#if (class(dt1$CHLOR_A)=="character") dt1$CHLOR_A <-as.numeric(dt1$CHLOR_A)
#if (class(dt1$CHLOR_A_SD)=="factor") dt1$CHLOR_A_SD <-as.numeric(levels(dt1$CHLOR_A_SD))[as.integer(dt1$CHLOR_A_SD) ]               
#if (class(dt1$CHLOR_A_SD)=="character") dt1$CHLOR_A_SD <-as.numeric(dt1$CHLOR_A_SD)
#if (class(dt1$CHLOR_A_N)=="factor") dt1$CHLOR_A_N <-as.numeric(levels(dt1$CHLOR_A_N))[as.integer(dt1$CHLOR_A_N) ]               
#if (class(dt1$CHLOR_A_N)=="character") dt1$CHLOR_A_N <-as.numeric(dt1$CHLOR_A_N)
#if (class(dt1$CHLOR_AQUAL)!="factor") dt1$CHLOR_AQUAL<- as.factor(dt1$CHLOR_AQUAL)
#if (class(dt1$PHAEOPIGMENTS_A)=="factor") dt1$PHAEOPIGMENTS_A <-as.numeric(levels(dt1$PHAEOPIGMENTS_A))[as.integer(dt1$PHAEOPIGMENTS_A) ]               
#if (class(dt1$PHAEOPIGMENTS_A)=="character") dt1$PHAEOPIGMENTS_A <-as.numeric(dt1$PHAEOPIGMENTS_A)
#if (class(dt1$PHAEOPIGMENTS_A_SD)=="factor") dt1$PHAEOPIGMENTS_A_SD <-as.numeric(levels(dt1$PHAEOPIGMENTS_A_SD))[as.integer(dt1$PHAEOPIGMENTS_A_SD) ]               
#if (class(dt1$PHAEOPIGMENTS_A_SD)=="character") dt1$PHAEOPIGMENTS_A_SD <-as.numeric(dt1$PHAEOPIGMENTS_A_SD)
#if (class(dt1$PHAEOPIGMENTS_A_N)=="factor") dt1$PHAEOPIGMENTS_A_N <-as.numeric(levels(dt1$PHAEOPIGMENTS_A_N))[as.integer(dt1$PHAEOPIGMENTS_A_N) ]               
#if (class(dt1$PHAEOPIGMENTS_A_N)=="character") dt1$PHAEOPIGMENTS_A_N <-as.numeric(dt1$PHAEOPIGMENTS_A_N)
#if (class(dt1$PHAEOPIGMENTS_AQUAL)!="factor") dt1$PHAEOPIGMENTS_AQUAL<- as.factor(dt1$PHAEOPIGMENTS_AQUAL)
#if (class(dt1$COMMENTWATERCHL)!="factor") dt1$COMMENTWATERCHL<- as.factor(dt1$COMMENTWATERCHL)
#if (class(dt1$GRACILARIA_MASS)=="factor") dt1$GRACILARIA_MASS <-as.numeric(levels(dt1$GRACILARIA_MASS))[as.integer(dt1$GRACILARIA_MASS) ]               
#if (class(dt1$GRACILARIA_MASS)=="character") dt1$GRACILARIA_MASS <-as.numeric(dt1$GRACILARIA_MASS)
#if (class(dt1$GRACILARIA_SD)=="factor") dt1$GRACILARIA_SD <-as.numeric(levels(dt1$GRACILARIA_SD))[as.integer(dt1$GRACILARIA_SD) ]               
#if (class(dt1$GRACILARIA_SD)=="character") dt1$GRACILARIA_SD <-as.numeric(dt1$GRACILARIA_SD)
#if (class(dt1$GRACILARIA_N)=="factor") dt1$GRACILARIA_N <-as.numeric(levels(dt1$GRACILARIA_N))[as.integer(dt1$GRACILARIA_N) ]               
#if (class(dt1$GRACILARIA_N)=="character") dt1$GRACILARIA_N <-as.numeric(dt1$GRACILARIA_N)
#if (class(dt1$GRACILARIA_QUAL)!="factor") dt1$GRACILARIA_QUAL<- as.factor(dt1$GRACILARIA_QUAL)
#if (class(dt1$ULVA_MASS)=="factor") dt1$ULVA_MASS <-as.numeric(levels(dt1$ULVA_MASS))[as.integer(dt1$ULVA_MASS) ]               
#if (class(dt1$ULVA_MASS)=="character") dt1$ULVA_MASS <-as.numeric(dt1$ULVA_MASS)
#if (class(dt1$ULVA_SD)=="factor") dt1$ULVA_SD <-as.numeric(levels(dt1$ULVA_SD))[as.integer(dt1$ULVA_SD) ]               
#if (class(dt1$ULVA_SD)=="character") dt1$ULVA_SD <-as.numeric(dt1$ULVA_SD)
#if (class(dt1$ULVA_N)=="factor") dt1$ULVA_N <-as.numeric(levels(dt1$ULVA_N))[as.integer(dt1$ULVA_N) ]               
#if (class(dt1$ULVA_N)=="character") dt1$ULVA_N <-as.numeric(dt1$ULVA_N)
#if (class(dt1$ULVA_QUAL)!="factor") dt1$ULVA_QUAL<- as.factor(dt1$ULVA_QUAL)
#if (class(dt1$COMMENTMACROALGAE)!="factor") dt1$COMMENTMACROALGAE<- as.factor(dt1$COMMENTMACROALGAE)
#if (class(dt1$TSS)=="factor") dt1$TSS <-as.numeric(levels(dt1$TSS))[as.integer(dt1$TSS) ]               
#if (class(dt1$TSS)=="character") dt1$TSS <-as.numeric(dt1$TSS)
#if (class(dt1$TSS_SD)=="factor") dt1$TSS_SD <-as.numeric(levels(dt1$TSS_SD))[as.integer(dt1$TSS_SD) ]               
#if (class(dt1$TSS_SD)=="character") dt1$TSS_SD <-as.numeric(dt1$TSS_SD)
#if (class(dt1$TSS_N)=="factor") dt1$TSS_N <-as.numeric(levels(dt1$TSS_N))[as.integer(dt1$TSS_N) ]               
#if (class(dt1$TSS_N)=="character") dt1$TSS_N <-as.numeric(dt1$TSS_N)
#if (class(dt1$TSSQUAL)!="factor") dt1$TSSQUAL<- as.factor(dt1$TSSQUAL)
#if (class(dt1$POM)=="factor") dt1$POM <-as.numeric(levels(dt1$POM))[as.integer(dt1$POM) ]               
#if (class(dt1$POM)=="character") dt1$POM <-as.numeric(dt1$POM)
#if (class(dt1$POM_SD)=="factor") dt1$POM_SD <-as.numeric(levels(dt1$POM_SD))[as.integer(dt1$POM_SD) ]               
#if (class(dt1$POM_SD)=="character") dt1$POM_SD <-as.numeric(dt1$POM_SD)
#if (class(dt1$POM_N)=="factor") dt1$POM_N <-as.numeric(levels(dt1$POM_N))[as.integer(dt1$POM_N) ]               
#if (class(dt1$POM_N)=="character") dt1$POM_N <-as.numeric(dt1$POM_N)
#if (class(dt1$POMQUAL)!="factor") dt1$POMQUAL<- as.factor(dt1$POMQUAL)
#if (class(dt1$PIM)=="factor") dt1$PIM <-as.numeric(levels(dt1$PIM))[as.integer(dt1$PIM) ]               
#if (class(dt1$PIM)=="character") dt1$PIM <-as.numeric(dt1$PIM)
#if (class(dt1$PIM_SD)=="factor") dt1$PIM_SD <-as.numeric(levels(dt1$PIM_SD))[as.integer(dt1$PIM_SD) ]               
#if (class(dt1$PIM_SD)=="character") dt1$PIM_SD <-as.numeric(dt1$PIM_SD)
#if (class(dt1$PIM_N)=="factor") dt1$PIM_N <-as.numeric(levels(dt1$PIM_N))[as.integer(dt1$PIM_N) ]               
#if (class(dt1$PIM_N)=="character") dt1$PIM_N <-as.numeric(dt1$PIM_N)
#if (class(dt1$PIMQUAL)!="factor") dt1$PIMQUAL<- as.factor(dt1$PIMQUAL)
#if (class(dt1$COMMENTTSS)!="factor") dt1$COMMENTTSS<- as.factor(dt1$COMMENTTSS)
#if (class(dt1$SEDMASS)=="factor") dt1$SEDMASS <-as.numeric(levels(dt1$SEDMASS))[as.integer(dt1$SEDMASS) ]               
#if (class(dt1$SEDMASS)=="character") dt1$SEDMASS <-as.numeric(dt1$SEDMASS)
#if (class(dt1$SEDMASS_SD)=="factor") dt1$SEDMASS_SD <-as.numeric(levels(dt1$SEDMASS_SD))[as.integer(dt1$SEDMASS_SD) ]               
#if (class(dt1$SEDMASS_SD)=="character") dt1$SEDMASS_SD <-as.numeric(dt1$SEDMASS_SD)
#if (class(dt1$SEDMASS_N)=="factor") dt1$SEDMASS_N <-as.numeric(levels(dt1$SEDMASS_N))[as.integer(dt1$SEDMASS_N) ]               
#if (class(dt1$SEDMASS_N)=="character") dt1$SEDMASS_N <-as.numeric(dt1$SEDMASS_N)
#if (class(dt1$SEDMASSQUAL)!="factor") dt1$SEDMASSQUAL<- as.factor(dt1$SEDMASSQUAL)
#if (class(dt1$SEDPERC_N)=="factor") dt1$SEDPERC_N <-as.numeric(levels(dt1$SEDPERC_N))[as.integer(dt1$SEDPERC_N) ]               
#if (class(dt1$SEDPERC_N)=="character") dt1$SEDPERC_N <-as.numeric(dt1$SEDPERC_N)
#if (class(dt1$SEDPERC_N_SD)=="factor") dt1$SEDPERC_N_SD <-as.numeric(levels(dt1$SEDPERC_N_SD))[as.integer(dt1$SEDPERC_N_SD) ]               
#if (class(dt1$SEDPERC_N_SD)=="character") dt1$SEDPERC_N_SD <-as.numeric(dt1$SEDPERC_N_SD)
#if (class(dt1$SEDPERC_N_N)=="factor") dt1$SEDPERC_N_N <-as.numeric(levels(dt1$SEDPERC_N_N))[as.integer(dt1$SEDPERC_N_N) ]               
#if (class(dt1$SEDPERC_N_N)=="character") dt1$SEDPERC_N_N <-as.numeric(dt1$SEDPERC_N_N)
#if (class(dt1$SEDPERC_NQUAL)!="factor") dt1$SEDPERC_NQUAL<- as.factor(dt1$SEDPERC_NQUAL)
#if (class(dt1$SEDPERC_C)=="factor") dt1$SEDPERC_C <-as.numeric(levels(dt1$SEDPERC_C))[as.integer(dt1$SEDPERC_C) ]               
#if (class(dt1$SEDPERC_C)=="character") dt1$SEDPERC_C <-as.numeric(dt1$SEDPERC_C)
#if (class(dt1$SEDPERC_C_SD)=="factor") dt1$SEDPERC_C_SD <-as.numeric(levels(dt1$SEDPERC_C_SD))[as.integer(dt1$SEDPERC_C_SD) ]               
#if (class(dt1$SEDPERC_C_SD)=="character") dt1$SEDPERC_C_SD <-as.numeric(dt1$SEDPERC_C_SD)
#if (class(dt1$SEDPERC_C_N)=="factor") dt1$SEDPERC_C_N <-as.numeric(levels(dt1$SEDPERC_C_N))[as.integer(dt1$SEDPERC_C_N) ]               
#if (class(dt1$SEDPERC_C_N)=="character") dt1$SEDPERC_C_N <-as.numeric(dt1$SEDPERC_C_N)
#if (class(dt1$SEDPERC_CQUAL)!="factor") dt1$SEDPERC_CQUAL<- as.factor(dt1$SEDPERC_CQUAL)
#if (class(dt1$COMMENTSEDCN)!="factor") dt1$COMMENTSEDCN<- as.factor(dt1$COMMENTSEDCN)
#
## Convert Missing Values to NA for non-dates
#
#dt1$SCTTEMP <- ifelse((trimws(as.character(dt1$SCTTEMP))==trimws("NA")),NA,dt1$SCTTEMP)               
#suppressWarnings(dt1$SCTTEMP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SCTTEMP))==as.character(as.numeric("NA"))),NA,dt1$SCTTEMP))
#
##remove the data we don't need
#dt1a<-dt1 %>% 
#  select(c(STATION,SESSIONDATE,LATITUDE,LONGITUDE,SCTTEMP,DO,DEPTH))
#
# 
# #write file for temperature and do
# write.csv(dt1a,
#           file = file.path("data", 
#                            "raw_data", 
#                            "LTER_VCR", 
#                            "raw_LTER_VCR_do_temp.csv"),
#           row.names = F)
#
#rm(dt1)
#rm(dt1a)
###start here for temp & do if files already download-------


envdata <- read.csv(file = file.path("data",
                                     "raw_data",
                                     "LTER_VCR",
                                     "raw_LTER_VCR_do_temp.csv"))

#match column names and date format with fish and remove data from before 2011 and the one 2024 date since its the only value

envdata <- envdata %>% 
  rename(SUBSITE = STATION,
         DATE = SESSIONDATE,
         TEMP = SCTTEMP) %>% 
  mutate(DATE = ymd(DATE),
         DATE = as.Date(DATE),
         YEAR = year(DATE))%>% 
  filter(YEAR>=2011&YEAR!="2024")

hist(envdata$TEMP)
setdiff(unique(fish$SUBSITE), unique(envdata$SUBSITE))
sort(unique(fish$SUBSITE))
sort(unique(envdata$SUBSITE))

# VARIABLES
# A) annual average temperature (rational: growing season avg would require identifying growing season at each site)
# B) annual mean daily max temperature
# C) annual mean daily min temperature


annual_TEMP <- envdata %>% 
  group_by(YEAR) %>%
  reframe(mean_daily_temp = mean(TEMP, na.rm = T))  #variable A

daily_TEMP <- envdata%>% drop_na(TEMP) %>% 
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
daily_DO <- envdata%>%
  drop_na(DO) %>% 
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
fish$SITE<-"LTER_VCR"


#remove any rows where the date is missing
fish<-fish%>%drop_na(c("DATE"))



#merge fish and environmental data by year and habitat type

intermediate <- left_join(fish, enviro_final, by = c("YEAR"))


#Here, you'll want to rename any columns you already fit to have our required column naming conventions

intermediate.names()

intermediate <- intermediate %>% 
  select(SITE,DATE,COMMON_NAME, SUBSITE,SCI_NAME,SIZE, YEAR, EFFORT,SP_CODE, 
         mean_daily_temp, mean_max_temp, mean_min_temp,mean_daily_DO,mean_min_DO,annual_avg_DO)



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




