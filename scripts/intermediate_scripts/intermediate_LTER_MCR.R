#_________________________________
#LTER MCR
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

dataset <- "LTER_MCR"

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

# Package ID: knb-lter-mcr.6.63 Cataloging System:https://pasta.edirepository.org.
# Data set title: MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Fishes, ongoing since 2005.
# Data set creator:    - Moorea Coral Reef LTER 
# Data set creator:  Andrew Brooks - Moorea Coral Reef LTER 
# Metadata Provider:   Information Manager - Moorea Coral Reef LTER 
# Contact:    - Information Manager Moorea Coral Reef LTER  - mcrlter@msi.ucsb.edu
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       

#options(HTTPUserAgent="EDI_CodeGen")


#inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcr/6/63/ac2c7a859ce8595ec1339e8530b9ba50" 
#infile1 <- tempfile()
#try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
#if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


#dt1 <-read.csv(infile1,header=F 
#              ,skip=1
#              ,sep=","  
#              ,quot='"' 
#              , col.names=c(
#                "Year",     
#               "Date",     
#                 "Start",     
#                 "End",     
#                 "Location",     
#                 "Site",     
#                 "Habitat",     
#                 "Depth",     
#                 "Transect",     
#                 "Swath",     
#                 "Taxonomy",     
#                 "Family",     
#                 "Count",     
#                 "Total_Length",     
#                 "Length_Anomaly",     
#                 "Biomass",     
#                 "Comment",     
#                 "Coarse_Trophic",     
#                 "Fine_Trophic",     
#                 "Cloud_Cover",     
#                 "Wind_Velocity",     
#                 "Sea_State",     
#                 "Swell",     
#                 "Visibility",     
#                 "Surge",     
#                 "Diver"    ), check.names=TRUE)
#
#unlink(infile1)
#
## Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#
## attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
#tmpDateFormat<-"%Y-%m-%d"
#tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)
## Keep the new dates only if they all converted correctly
#if(nrow(dt1[dt1$Date != "",]) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
#
#if (class(dt1$Start)!="factor") dt1$Start<- as.factor(dt1$Start)
#if (class(dt1$End)!="factor") dt1$End<- as.factor(dt1$End)
#if (class(dt1$Location)!="factor") dt1$Location<- as.factor(dt1$Location)
#if (class(dt1$Site)!="factor") dt1$Site<- as.factor(dt1$Site)
#if (class(dt1$Habitat)!="factor") dt1$Habitat<- as.factor(dt1$Habitat)
#if (class(dt1$Depth)=="factor") dt1$Depth <-as.numeric(levels(dt1$Depth))[as.integer(dt1$Depth) ]               
#if (class(dt1$Depth)=="character") dt1$Depth <-as.numeric(dt1$Depth)
#if (class(dt1$Transect)!="factor") dt1$Transect<- as.factor(dt1$Transect)
#if (class(dt1$Swath)!="factor") dt1$Swath<- as.factor(dt1$Swath)
#if (class(dt1$Taxonomy)!="factor") dt1$Taxonomy<- as.factor(dt1$Taxonomy)
#if (class(dt1$Family)!="factor") dt1$Family<- as.factor(dt1$Family)
#if (class(dt1$Count)=="factor") dt1$Count <-as.numeric(levels(dt1$Count))[as.integer(dt1$Count) ]               
#if (class(dt1$Count)=="character") dt1$Count <-as.numeric(dt1$Count)
#if (class(dt1$Total_Length)=="factor") dt1$Total_Length <-as.numeric(levels(dt1$Total_Length))[as.integer(dt1$Total_Length) ]               
#if (class(dt1$Total_Length)=="character") dt1$Total_Length <-as.numeric(dt1$Total_Length)
#if (class(dt1$Length_Anomaly)!="factor") dt1$Length_Anomaly<- as.factor(dt1$Length_Anomaly)
#if (class(dt1$Biomass)=="factor") dt1$Biomass <-as.numeric(levels(dt1$Biomass))[as.integer(dt1$Biomass) ]               
#if (class(dt1$Biomass)=="character") dt1$Biomass <-as.numeric(dt1$Biomass)
#if (class(dt1$Comment)!="factor") dt1$Comment<- as.factor(dt1$Comment)
#if (class(dt1$Coarse_Trophic)!="factor") dt1$Coarse_Trophic<- as.factor(dt1$Coarse_Trophic)
#if (class(dt1$Fine_Trophic)!="factor") dt1$Fine_Trophic<- as.factor(dt1$Fine_Trophic)
#if (class(dt1$Cloud_Cover)=="factor") dt1$Cloud_Cover <-as.numeric(levels(dt1$Cloud_Cover))[as.integer(dt1$Cloud_Cover) ]               
#if (class(dt1$Cloud_Cover)=="character") dt1$Cloud_Cover <-as.numeric(dt1$Cloud_Cover)
#if (class(dt1$Swell)=="factor") dt1$Swell <-as.numeric(levels(dt1$Swell))[as.integer(dt1$Swell) ]               
#if (class(dt1$Swell)=="character") dt1$Swell <-as.numeric(dt1$Swell)
#if (class(dt1$Visibility)=="factor") dt1$Visibility <-as.numeric(levels(dt1$Visibility))[as.integer(dt1$Visibility) ]               
#if (class(dt1$Visibility)=="character") dt1$Visibility <-as.numeric(dt1$Visibility)
#if (class(dt1$Surge)=="factor") dt1$Surge <-as.numeric(levels(dt1$Surge))[as.integer(dt1$Surge) ]               
#if (class(dt1$Surge)=="character") dt1$Surge <-as.numeric(dt1$Surge)
#if (class(dt1$Diver)!="factor") dt1$Diver<- as.factor(dt1$Diver)
#
## Convert Missing Values to NA for non-dates
#
#dt1$Taxonomy <- as.factor(ifelse((trimws(as.character(dt1$Taxonomy))==trimws("No fish observed")),NA,as.character(dt1$Taxonomy)))
#dt1$Total_Length <- ifelse((trimws(as.character(dt1$Total_Length))==trimws("-1.0")),NA,dt1$Total_Length)               
#suppressWarnings(dt1$Total_Length <- ifelse(!is.na(as.numeric("-1.0")) & (trimws(as.character(dt1$Total_Length))==as.character(as.numeric("-1.0"))),NA,dt1$Total_Length))
#dt1$Length_Anomaly <- as.factor(ifelse((trimws(as.character(dt1$Length_Anomaly))==trimws("na")),NA,as.character(dt1$Length_Anomaly)))
#dt1$Biomass <- ifelse((trimws(as.character(dt1$Biomass))==trimws("-1")),NA,dt1$Biomass)               
#suppressWarnings(dt1$Biomass <- ifelse(!is.na(as.numeric("-1")) & (trimws(as.character(dt1$Biomass))==as.character(as.numeric("-1"))),NA,dt1$Biomass))
#dt1$Biomass <- ifelse((trimws(as.character(dt1$Biomass))==trimws("######.#")),NA,dt1$Biomass)               
#suppressWarnings(dt1$Biomass <- ifelse(!is.na(as.numeric("######.#")) & (trimws(as.character(dt1$Biomass))==as.character(as.numeric("######.#"))),NA,dt1$Biomass))
#dt1$Comment <- as.factor(ifelse((trimws(as.character(dt1$Comment))==trimws("NA")),NA,as.character(dt1$Comment)))
#dt1$Coarse_Trophic <- as.factor(ifelse((trimws(as.character(dt1$Coarse_Trophic))==trimws("na")),NA,as.character(dt1$Coarse_Trophic)))
#dt1$Fine_Trophic <- as.factor(ifelse((trimws(as.character(dt1$Fine_Trophic))==trimws("na")),NA,as.character(dt1$Fine_Trophic)))
#dt1$Visibility <- ifelse((trimws(as.character(dt1$Visibility))==trimws("-1")),NA,dt1$Visibility)               
#suppressWarnings(dt1$Visibility <- ifelse(!is.na(as.numeric("-1")) & (trimws(as.character(dt1$Visibility))==as.character(as.numeric("-1"))),NA,dt1$Visibility))
#
#
#
# write.csv(dt1,
#            file = file.path("data",
#                             "raw_data",                            "LTER_MCR",
#                             "raw_LTER_MCR_fish.csv"),
#           row.names = F)  
# rm(dt1)
#
##read in data - make sure to download from Google Drive

data <- read.csv(file = file.path("data",
                                  "raw_data",
                                  "LTER_MCR",
                                  "raw_LTER_MCR_fish.csv"))

#PART 1: FISH -----

# checks --------------------

#get generic output on data structures and unique values for each - this you can keep! It's set up so that it will name it based on your unique site value you named earlier. 

summarytools::view(summarytools::dfSummary(data),
                   file = file.path("data",
                                    "metadata",
                                    paste0(dataset, 
                                           "_datasummary.html")))



#no duplicate rows found (summary output). If there were, definitely remove them here first before expanding any data for counts/abundance.

## counts/abundance -----


#final step for counts is that we want ONE ROW to equal ONE FISH. We can do this relatively easily using some nice dplyr tools. 

data <- data %>% 
  uncount(Count,
          .remove = T) #the default is to remove the count column after it "expands" since the count won't necessarily represent anything anymore

## dates ------

#here is where I would create a YMD column if necessary if the date was in any other format. 


data <- data %>% 
  mutate(DATE = ymd(Date),.keep = "unused")

## sites and spatial info --------
#within each location there are sites, transect, and swath

##sample effort ------------------------------------

#across years lumped (no transects)

#this is back to base R, but the output is really appealing 

timeseries <- function(x){length(unique(x))} #function can can count unique years for each station

tapply(data$Year, list(data$Location), timeseries)

#we can look at the same thing visually 

data %>% 
  distinct(Year, Location) %>% 
  ggplot(aes(x = as.factor(Year),
             y = Location,
             color = as.factor(Location))) +
  geom_point(show.legend = F) +
  labs(x = "",
       y = "Site")

#here I am calculating effort as time in seconds of transect, to get catch per unit effort will need to group by subsite/date then divide by effort
#keeping habitat because it can be used to more accurately match temperature sensors to their sites
fish <- data %>% 
  select(Year,Start,End,Location, Taxonomy,Total_Length,DATE,Habitat) %>% 
  rename(SUBSITE = Location,
         YEAR=Year,
         SCI_NAME=Taxonomy,
         SIZE=Total_Length,
         reef_type_code=Habitat) %>% 
  mutate(EFFORT=as.duration(hm(End)-hm(Start)),
         EFFORT=dseconds(EFFORT),
         EFFORT=as.numeric(EFFORT))
#changing habitat names to match temperature data below
unique(fish$reef_type_code)

fish$reef_type_code[fish$reef_type_code=="Backreef"] <- "BAK"
fish$reef_type_code[fish$reef_type_code=="Forereef"] <- "FOR"
fish$reef_type_code[fish$reef_type_code=="Fringing"] <- "FRI"
rm(data)

#quick look at distribution of counts
count_check <- data.frame(fish %>% group_by(DATE, SCI_NAME) %>% 
                            reframe(count=n())) # count for each species for each sampling event 
hist(count_check$count, breaks = 20) 
summary(count_check)

#at the end of this data you should have DATE, SITE, SP_CODE, SIZE, SCIENTIFIC_NAME, COMMON_NAME, EFFORT

#PART 2: TEMP  ----

#same thing here - only un-comment and run this code if you don't have access to the Google Drive where this was uploaded to 
#download data from edi, note that there are six mounted temperature probes at different depths and there is not DO data
#fish data were all collected at 12 m in depth or shallower, so I have excluded the deeper water measurements in a later step

# Package ID: knb-lter-mcr.1035.16 Cataloging System:https://pasta.edirepository.org.
# Data set title: MCR LTER: Coral Reef: Benthic Water Temperature, ongoing since 2005.
# Data set creator:    - Moorea Coral Reef LTER 
# Data set creator:  James Leichter - Moorea Coral Reef LTER 
# Data set creator:  Tom Adam - Moorea Coral Reef LTER 
# Data set creator:  Keith Seydel - Moorea Coral Reef LTER 
# Data set creator:  Chris Gotschalk - Moorea Coral Reef LTER 
# Metadata Provider:    - Moorea Coral Reef LTER 
# Contact:    - Information Manager Moorea Coral Reef LTER  - mcrlter@msi.ucsb.edu
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       


#inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcr/1035/16/edeb61ce89ea1cba66a80ae06b201122" 
#infile1 <- tempfile()
#try(download.file(inUrl1,infile1,method="curl"))
#if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
#
#
#dt1 <-read.csv(infile1,header=F 
#               ,skip=1
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "site",     
#                 "time_local",     
#                 "time_utc",     
#                 "reef_type_code",     
#                 "sensor_type",     
#                 "sensor_depth_m",     
#                 "temperature_c"    ), check.names=TRUE)
#
#unlink(infile1)
#
#
#inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcr/1035/16/757932fc807e6e88304d031632a19a05" 
#infile2 <- tempfile()
#try(download.file(inUrl2,infile2,method="curl"))
#if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")
#
#
#dt2 <-read.csv(infile2,header=F 
#               ,skip=1
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "site",     
#                 "time_local",     
#                 "time_utc",     
#                 "reef_type_code",     
#                 "sensor_type",     
#                 "sensor_depth_m",     
#                 "temperature_c"    ), check.names=TRUE)
#
#unlink(infile2)
#
#inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcr/1035/16/caf881252e2ef8c6d652bff49f4cbe32" 
#infile3 <- tempfile()
#try(download.file(inUrl3,infile3,method="curl"))
#if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")
#
#
#dt3 <-read.csv(infile3,header=F 
#               ,skip=1
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "site",     
#                 "time_local",     
#                 "time_utc",     
#                 "reef_type_code",     
#                 "sensor_type",     
#                 "sensor_depth_m",     
#                 "temperature_c"    ), check.names=TRUE)
#
#unlink(infile3)
#
## Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#
#
#inUrl4  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcr/1035/16/194415ea470e0f6815e9d74b6ccde84c" 
#infile4 <- tempfile()
#try(download.file(inUrl4,infile4,method="curl"))
#if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")
#
#
#dt4 <-read.csv(infile4,header=F 
#               ,skip=1
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "site",     
#                 "time_local",     
#                 "time_utc",     
#                 "reef_type_code",     
#                 "sensor_type",     
#                 "sensor_depth_m",     
#                 "temperature_c"    ), check.names=TRUE)
#
#unlink(infile4)
#
#inUrl5  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcr/1035/16/0528e6f0da3cfed1ec9ae3816f4e8c2c" 
#infile5 <- tempfile()
#try(download.file(inUrl5,infile5,method="curl"))
#if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")
#
#
#dt5 <-read.csv(infile5,header=F 
#               ,skip=1
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "site",     
#                 "time_local",     
#                 "time_utc",     
#                 "reef_type_code",     
#                 "sensor_type",     
#                 "sensor_depth_m",     
#                 "temperature_c"    ), check.names=TRUE)
#
#unlink(infile5)
#
#
#inUrl6  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcr/1035/16/5edcca099f8805b773941aea028ab605" 
#infile6 <- tempfile()
#try(download.file(inUrl6,infile6,method="curl"))
#if (is.na(file.size(infile6))) download.file(inUrl6,infile6,method="auto")
#
#
#dt6 <-read.csv(infile6,header=F 
#               ,skip=1
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "site",     
#                 "time_local",     
#                 "time_utc",     
#                 "reef_type_code",     
#                 "sensor_type",     
#                 "sensor_depth_m",     
#                 "temperature_c"    ), check.names=TRUE)
#
#unlink(infile6)
#
#
#
#inUrl7  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcr/1035/16/a48dc71fd89525628d3c169ac6be5654" 
#infile7 <- tempfile()
#try(download.file(inUrl7,infile7,method="curl"))
#if (is.na(file.size(infile7))) download.file(inUrl7,infile7,method="auto")
#
#
#dt7 <-read.csv(infile7,header=F 
#               ,skip=1
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "site",     
#                 "time_local",     
#                 "time_utc",     
#                 "reef_type_code",     
#                 "sensor_type",     
#                 "sensor_depth_m",     
#                 "temperature_c"    ), check.names=TRUE)
#
#unlink(infile7)
#
#
#inUrl8  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcr/1035/16/51298cc515a159c3ebf3770c35d760b8" 
#infile8 <- tempfile()
#try(download.file(inUrl8,infile8,method="curl"))
#if (is.na(file.size(infile8))) download.file(inUrl8,infile8,method="auto")
#
#
#dt8 <-read.csv(infile8,header=F 
#               ,skip=1
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "site",     
#                 "year",     
#                 "reef_type_code",     
#                 "sensor_depth_m"    ), check.names=TRUE)
#
#unlink(infile8)
#
##rbind
#data<-do.call("rbind",list(dt1,dt2,dt3,dt4,dt5,dt6,dt7))
#
#
## Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#
#if (class(data$site)!="factor") data$site<- as.factor(data$site)                                   
#if (class(data$reef_type_code)!="factor") data$reef_type_code<- as.factor(data$reef_type_code)
#if (class(data$sensor_type)!="factor") data$sensor_type<- as.factor(data$sensor_type)
#if (class(data$sensor_depth_m)=="factor") data$sensor_depth_m <-as.numeric(levels(data$sensor_depth_m))[as.integer(data$sensor_depth_m) ]               
#if (class(data$sensor_depth_m)=="character") data$sensor_depth_m <-as.numeric(data$sensor_depth_m)
#if (class(data$temperature_c)=="factor") data$temperature_c <-as.numeric(levels(data$temperature_c))[as.integer(data$temperature_c) ]               
#if (class(data$temperature_c)=="character") data$temperature_c <-as.numeric(data$temperature_c)
#
#
 #write file for temperature and do
# write.csv(data,
#           file = file.path("data", 
#                            "raw_data", 
#                            "LTER_MCR", 
#                            "raw_LTER_MCR_temp.csv"),
#           row.names = F)
#
#
#rm(data)
##start here for temp & do if files already download-------


envdata <- read.csv(file = file.path("data",
                                     "raw_data",
                                     "LTER_MCR",
                                     "raw_LTER_MCR_temp.csv"))

#match column names and date format with fish and remove temperatures that are from deeper than 12 m

envdata <- envdata %>% 
  rename(SUBSITE = site,
         DATE = time_utc,
         TEMP = temperature_c) %>% 
  mutate(DATE = ymd_hms(DATE),
         DATE = as.Date(DATE),
         YEAR = year(DATE))%>% 
  filter(sensor_depth_m<12)

setdiff(unique(fish$SUBSITE), unique(envdata$SUBSITE))
sort(unique(fish$SUBSITE))
sort(unique(envdata$SUBSITE))

# VARIABLES
# A) annual average temperature (rational: growing season avg would require identifying growing season at each site)
# B) annual mean daily max temperature
# C) annual mean daily min temperature


annual_TEMP <- envdata %>% 
  group_by(YEAR,reef_type_code) %>%
  reframe(mean_daily_temp = mean(TEMP, na.rm = T))  #variable A

daily_TEMP <- drop_na(envdata) %>% 
  group_by(YEAR, DATE,reef_type_code) %>% 
  reframe(mean_max_temp = max(TEMP, na.rm = T),
          mean_min_temp = min(TEMP, na.rm = T)) # get daily max & min

daily_TEMP <- daily_TEMP %>% 
  group_by(YEAR) %>% 
  reframe(mean_max_temp = mean(mean_max_temp, na.rm = T),
          mean_min_temp = mean(mean_min_temp, na.rm = T))   # variables B & C 


temp_final <- left_join(daily_TEMP, annual_TEMP)
temp_final$YEAR <- temp_final$YEAR + 1 # offset year before joining to fish data

#finalize environmental data 

enviro_final <- temp_final 

#finalize intermediate data -----


#add name of overall site for when its added to rest of datasite
fish$SITE<-"LTER_MCR"


#remove any rows where the date or subsite is missing
fish<-fish%>%drop_na(c("DATE","SUBSITE"))



#merge fish and environmental data by year and habitat type

intermediate <- left_join(fish, enviro_final, by = c("YEAR","reef_type_code"))


#Here, you'll want to rename any columns you already fit to have our required column naming conventions

intermediate.names()

intermediate <- intermediate %>% 
  select(DATE, SUBSITE,SCI_NAME,SIZE, YEAR, EFFORT,, 
         mean_daily_temp, mean_max_temp, mean_min_temp)



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




