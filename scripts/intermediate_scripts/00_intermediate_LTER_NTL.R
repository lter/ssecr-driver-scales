#LTER NTL- Annual fish surveys  
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

dataset <- "LTER_NTL"

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
# Package ID: knb-lter-ntl.6.36 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Fish Lengths and Weights 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin-Madison 
# Data set creator:  Stephen Carpenter - University of Wisconsin-Madison 
# Data set creator:  Emily Stanley - University of Wisconsin-Madison 
# Metadata Provider:  NTL Information Manager - University of Wisconsin-Madison 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       


#options(HTTPUserAgent="EDI_CodeGen")
#
#
#inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/6/36/7dddb5e70250d2e24537e34272316220" 
#infile1 <- tempfile()
#try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
#if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
#
#
#dt1 <-read.csv(infile1,header=F 
#               ,skip=1
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "lakeid",     
#                 "year4",     
#                 "sampledate",     
#                 "gearid",     
#                 "spname",     
#                 "sampletype",     
#                 "depth",     
#                 "rep",     
#                 "indid",     
#                 "length",     
#                 "weight",     
#                 "sex",     
#                 "fishpart",     
#                 "spseq",     
#                 "flag"    ), check.names=TRUE)
#
#unlink(infile1)
#
## Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#
#if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
#if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
#if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)                                   
## attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
#tmpDateFormat<-"%Y-%m-%d"
#tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
## Keep the new dates only if they all converted correctly
#if(nrow(dt1[dt1$sampledate != "",]) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
#
#if (class(dt1$gearid)!="factor") dt1$gearid<- as.factor(dt1$gearid)
#if (class(dt1$spname)!="factor") dt1$spname<- as.factor(dt1$spname)
#if (class(dt1$sampletype)!="factor") dt1$sampletype<- as.factor(dt1$sampletype)
#if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
#if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
#if (class(dt1$rep)=="factor") dt1$rep <-as.numeric(levels(dt1$rep))[as.integer(dt1$rep) ]               
#if (class(dt1$rep)=="character") dt1$rep <-as.numeric(dt1$rep)
#if (class(dt1$indid)!="factor") dt1$indid<- as.factor(dt1$indid)
#if (class(dt1$length)=="factor") dt1$length <-as.numeric(levels(dt1$length))[as.integer(dt1$length) ]               
#if (class(dt1$length)=="character") dt1$length <-as.numeric(dt1$length)
#if (class(dt1$weight)=="factor") dt1$weight <-as.numeric(levels(dt1$weight))[as.integer(dt1$weight) ]               
#if (class(dt1$weight)=="character") dt1$weight <-as.numeric(dt1$weight)
#if (class(dt1$sex)!="factor") dt1$sex<- as.factor(dt1$sex)
#if (class(dt1$fishpart)!="factor") dt1$fishpart<- as.factor(dt1$fishpart)
#if (class(dt1$spseq)!="factor") dt1$spseq<- as.factor(dt1$spseq)
#if (class(dt1$flag)!="factor") dt1$flag<- as.factor(dt1$flag)
#
#write.csv(dt1,
#                      file = file.path("data",
#                                       "raw_data",
#                                       "LTER_NTL",
#                                       "raw_LTER_NTL_fish.csv"),
#                      row.names = F)
#            rm(dt1)

#read in data - make sure to download from Google Drive

data <- read.csv(file = file.path("data",
                                  "raw_data",
                                  "LTER_NTL",
                                  "raw_LTER_NTL_fish.csv"))
#PART 1: FISH -----

# checks --------------------

#get generic output on data structures and unique values for each - this you can keep! It's set up so that it will name it based on your unique site value you named earlier. 

summarytools::view(summarytools::dfSummary(data),
                   file = file.path("data",
                                    "metadata",
                                    paste0(dataset, 
                                           "_datasummary.html")))
# 11 lakes,
# many gear types, use Elfish for electrofishing
#only gives common names
#total length is in mm
#https://lter.limnology.wisc.edu/wp-content/uploads/sites/2029/2023/06/NTLProtocols_Archived2023.pdf
#rep gives an id of what the sampling number was, keep for effort measurement

#Filter by Gear type

data<- subset(data,gearid=="ELFISH")
data$EFFORT<-"1800"
#effort = thirty minute trasnects 

## dates ------

#here is where I would create a YMD column if necessary if the date was in any other format. 


#Month: ~98% of data comes from July or August

data <- data %>% 
  mutate(DATE = ymd(sampledate),
         YEAR = year(DATE))

##sample effort ------------------------------------

#across years lumped (no transects)

#this is back to base R, but the output is really appealing 

timeseries <- function(x){length(unique(x))} #function can can count unique years for each station

tapply(data$YEAR, list(data$SITE), timeseries)

#we can look at the same thing visually 

data %>% 
  distinct(YEAR, lakeid) %>% 
  ggplot(aes(x = as.factor(YEAR),
             y = lakeid,
             color = as.factor(lakeid))) +
  geom_point(show.legend = F) +
  labs(x = "",
       y = "Site")

fish <- data %>% 
  select(DATE, lakeid, spname, length, rep, YEAR,EFFORT) %>% 
  rename(SUBSITE = lakeid,
         COMMON_NAME=spname,
         SIZE=length)

rm(data)

#####TEMP and DO#######################################################
# Package ID: knb-lter-ntl.29.35 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

## setwd("C:/users/my_name/my_dir")       
#
#
#options(HTTPUserAgent="EDI_CodeGen")
#
#
#inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/35/03e232a1b362900e0f059859abe8eb97" 
#infile1 <- tempfile()
#try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
#if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
#
#
#dt1 <-read.csv(infile1,header=F 
#               ,skip=1
#               ,sep=","  
#               ,quot='"' 
#               , col.names=c(
#                 "lakeid",     
#                 "year4",     
#                 "sampledate",     
#                 "depth",     
#                 "rep",     
#                 "sta",     
#                 "event",     
#                 "wtemp",     
#                 "o2",     
#                 "o2sat",     
#                 "deck",     
#                 "light",     
#                 "frlight",     
#                 "flagdepth",     
#                 "flagwtemp",     
#                 "flago2",     
#                 "flago2sat",     
#                 "flagdeck",     
#                 "flaglight",     
#                 "flagfrlight"    ), check.names=TRUE)
#
#unlink(infile1)
#
## Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
#
#if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
#if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
#if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)                                   
## attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
#tmpDateFormat<-"%Y-%m-%d"
#tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
## Keep the new dates only if they all converted correctly
#if(nrow(dt1[dt1$sampledate != "",]) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
#
#if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]               
#if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
#if (class(dt1$rep)!="factor") dt1$rep<- as.factor(dt1$rep)
#if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
#if (class(dt1$event)!="factor") dt1$event<- as.factor(dt1$event)
#if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]               
#if (class(dt1$wtemp)=="character") dt1$wtemp <-as.numeric(dt1$wtemp)
#if (class(dt1$o2)=="factor") dt1$o2 <-as.numeric(levels(dt1$o2))[as.integer(dt1$o2) ]               
#if (class(dt1$o2)=="character") dt1$o2 <-as.numeric(dt1$o2)
#if (class(dt1$o2sat)=="factor") dt1$o2sat <-as.numeric(levels(dt1$o2sat))[as.integer(dt1$o2sat) ]               
#if (class(dt1$o2sat)=="character") dt1$o2sat <-as.numeric(dt1$o2sat)
#if (class(dt1$deck)=="factor") dt1$deck <-as.numeric(levels(dt1$deck))[as.integer(dt1$deck) ]               
#if (class(dt1$deck)=="character") dt1$deck <-as.numeric(dt1$deck)
#if (class(dt1$light)=="factor") dt1$light <-as.numeric(levels(dt1$light))[as.integer(dt1$light) ]               
#if (class(dt1$light)=="character") dt1$light <-as.numeric(dt1$light)
#if (class(dt1$frlight)=="factor") dt1$frlight <-as.numeric(levels(dt1$frlight))[as.integer(dt1$frlight) ]               
#if (class(dt1$frlight)=="character") dt1$frlight <-as.numeric(dt1$frlight)
#if (class(dt1$flagdepth)!="factor") dt1$flagdepth<- as.factor(dt1$flagdepth)
#if (class(dt1$flagwtemp)!="factor") dt1$flagwtemp<- as.factor(dt1$flagwtemp)
#if (class(dt1$flago2)!="factor") dt1$flago2<- as.factor(dt1$flago2)
#if (class(dt1$flago2sat)!="factor") dt1$flago2sat<- as.factor(dt1$flago2sat)
#if (class(dt1$flagdeck)!="factor") dt1$flagdeck<- as.factor(dt1$flagdeck)
#if (class(dt1$flaglight)!="factor") dt1$flaglight<- as.factor(dt1$flaglight)
#if (class(dt1$flagfrlight)!="factor") dt1$flagfrlight<- as.factor(dt1$flagfrlight)
#
#
##write file for temperature and do
#write.csv(dt1,
#           file = file.path("data", 
#                            "raw_data", 
#                            "LTER_NTL", 
#                            "raw_LTER_NTL_temp.csv"),
#           row.names = F)
#
#
#rm(dt1)

##start here for temp & do if files already download-------


envdata <- read.csv(file = file.path("data",
                                     "raw_data",
                                     "LTER_NTL",
                                     "raw_LTER_NTL_temp.csv"))

envdata <- envdata %>% 
  rename(SUBSITE = lakeid,
         DATE = sampledate,
         TEMP = wtemp,
         DO=o2) %>% 
  mutate(DATE = ymd(DATE),
         YEAR = year(DATE))

# VARIABLES
#group by year and lake id
# A) annual average temperature (rational: growing season avg would require identifying growing season at each site)
# B) annual mean daily max temperature
# C) annual mean daily min temperature


annual_TEMP <- envdata %>% 
  group_by(YEAR,SUBSITE) %>%
  reframe(mean_daily_temp = mean(TEMP, na.rm = T))  #variable A

daily_TEMP <- drop_na(envdata) %>% 
  group_by(YEAR, DATE,SUBSITE) %>% 
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
  group_by(YEAR,SUBSITE) %>%
  reframe(annual_avg_DO = mean(DO, na.rm = T))  #variable D

#drop_na needed here because some Na's in do effect minimum calculation
daily_DO <- drop_na(envdata) %>% 
  group_by(YEAR, DATE,SUBSITE) %>% 
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
  merge(DO_final, by=c("YEAR","SUBSITE"), all = T) # use merge not join--join drops years if temp or DO missing for year


#remove any rows where the date or subsite is missing
fish<-fish%>%drop_na(c("DATE","SUBSITE"))
enviro_final<-enviro_final%>%drop_na(c("SUBSITE"))


#merge fish and environmental data by year 

intermediate <- left_join(fish, enviro_final, by = c("YEAR","SUBSITE"))


#Here, you'll want to rename any columns you already fit to have our required column naming conventions

intermediate.names()

intermediate.prep(intermediate)

#OPTIONAL VIZ: Play around with subsites by changing to True! The default is false. 

plot.top5(intermediate, species_col = "COMMON_NAME")

plot.presence(intermediate, 
              species_col = "COMMON_NAME",
              subsite = F)

plot.speciesaccum(intermediate,
                  species_col = "COMMON_NAME",
                  subsite = F)




