#_________________________________
# UC Davis Suisun Marsh Fish Study, CA,1980-2024 ver 1.
# Source: https://doi.org/10.6073/pasta/8e83d00acabe9754bf87d39f7e18e476
# SCALES/ SSECR                  
# Sierra Perez  
# R version 4.4.2 (2024-10-31)
# R Studio Version 2024.12.0+467
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

dataset <- "UCD_SUMA"

# PART #1: FISH  ---------------------

## LOAD FISH DATA ---------------------

# make sure to double check above you've set the "dataset" object to the name of the subfolder where this data is going to live! 

# loading fish dataset from EDI repo

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/1947/1/c5d59568835d0760e318829b800ee06d" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


data <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "CatchRowID",     
                 "SampleRowID",     
                 "OrganismCode",     
                 "StandardLength",     
                 "Dead",     
                 "Weight",     
                 "Sex",     
                 "Count",     
                 "CatchComments",     
                 "Volume",     
                 "AgeClassforUnmeasuredFish"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(data$CatchRowID)!="factor") data$CatchRowID<- as.factor(data$CatchRowID)
if (class(data$SampleRowID)!="factor") data$SampleRowID<- as.factor(data$SampleRowID)
if (class(data$OrganismCode)!="factor") data$OrganismCode<- as.factor(data$OrganismCode)
if (class(data$StandardLength)=="factor") data$StandardLength <-as.numeric(levels(data$StandardLength))[as.integer(data$StandardLength) ]               
if (class(data$StandardLength)=="character") data$StandardLength <-as.numeric(data$StandardLength)
if (class(data$Dead)!="factor") data$Dead<- as.factor(data$Dead)
if (class(data$Weight)!="factor") data$Weight<- as.factor(data$Weight)
if (class(data$Sex)!="factor") data$Sex<- as.factor(data$Sex)
if (class(data$Count)=="factor") data$Count <-as.numeric(levels(data$Count))[as.integer(data$Count) ]               
if (class(data$Count)=="character") data$Count <-as.numeric(data$Count)
if (class(data$CatchComments)!="factor") data$CatchComments<- as.factor(data$CatchComments)
if (class(data$Volume)=="factor") data$Volume <-as.numeric(levels(data$Volume))[as.integer(data$Volume) ]               
if (class(data$Volume)=="character") data$Volume <-as.numeric(data$Volume)
if (class(data$AgeClassforUnmeasuredFish)!="factor") data$AgeClassforUnmeasuredFish<- as.factor(data$AgeClassforUnmeasuredFish)

# Convert Missing Values to NA for non-dates


# CHECKS --------------------

#get generic output on data structures and unique values for each - this you can keep! 
# It's set up so that it will name it based on your unique site value you named earlier. 

summarytools::view(summarytools::dfSummary(data),
                   file = file.path("data",
                                    "metadata",
                                    paste0(dataset, 
                                           "_datasummary.html")))

## DUPLICATES --------------------

data[duplicated(data),]
data <- data[!duplicated(data),] 

## DROP COLUMNS --------------------

data <- data %>% 
  select(!c(Dead, Weight, Sex, Volume, AgeClassforUnmeasuredFish))

## TAXONOMY LIST ---------------
inUrl11  <- "https://pasta.lternet.edu/package/data/eml/edi/1947/1/dab1eed5cb4ff3772d39d3b7e8fb2c65" 
infile11 <- tempfile()
try(download.file(inUrl11,infile11,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile11))) download.file(inUrl11,infile11,method="auto")


species <-read.csv(infile11,header=F 
                ,skip=1
                ,sep=","  
                ,quot='"' 
                , col.names=c(
                  "OrganismRowID",     
                  "OrganismCode",     
                  "Native",     
                  "Resident",     
                  "Active",     
                  "CommonName",     
                  "Phylum",     
                  "Class",     
                  "Order",     
                  "Family",     
                  "Genus",     
                  "Species",     
                  "Comments",     
                  "TL_to_StdLen_Coefficient"    ), check.names=TRUE)

unlink(infile11)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(species$OrganismRowID)!="factor") species$OrganismRowID<- as.factor(species$OrganismRowID)
if (class(species$OrganismCode)!="factor") species$OrganismCode<- as.factor(species$OrganismCode)
if (class(species$Native)!="factor") species$Native<- as.factor(species$Native)
if (class(species$Resident)!="factor") species$Resident<- as.factor(species$Resident)
if (class(species$Active)!="factor") species$Active<- as.factor(species$Active)
if (class(species$CommonName)!="factor") species$CommonName<- as.factor(species$CommonName)
if (class(species$Phylum)!="factor") species$Phylum<- as.factor(species$Phylum)
if (class(species$Class)!="factor") species$Class<- as.factor(species$Class)
if (class(species$Order)!="factor") species$Order<- as.factor(species$Order)
if (class(species$Family)!="factor") species$Family<- as.factor(species$Family)
if (class(species$Genus)!="factor") species$Genus<- as.factor(species$Genus)
if (class(species$Species)!="factor") species$Species<- as.factor(species$Species)
if (class(species$Comments)!="factor") species$Comments<- as.factor(species$Comments)
if (class(species$TL_to_StdLen_Coefficient)=="factor") species$TL_to_StdLen_Coefficient <-as.numeric(levels(species$TL_to_StdLen_Coefficient))[as.integer(species$TL_to_StdLen_Coefficient) ]               
if (class(species$TL_to_StdLen_Coefficient)=="character") species$TL_to_StdLen_Coefficient <-as.numeric(species$TL_to_StdLen_Coefficient)

# Convert Missing Values to NA for non-dates

species$Phylum <- as.factor(ifelse((trimws(as.character(species$Phylum))==trimws("NA")),NA,as.character(species$Phylum)))
species$Class <- as.factor(ifelse((trimws(as.character(species$Class))==trimws("NA")),NA,as.character(species$Class)))
species$Order <- as.factor(ifelse((trimws(as.character(species$Order))==trimws("NA")),NA,as.character(species$Order)))
species$Family <- as.factor(ifelse((trimws(as.character(species$Family))==trimws("NA")),NA,as.character(species$Family)))
species$Genus <- as.factor(ifelse((trimws(as.character(species$Genus))==trimws("NA")),NA,as.character(species$Genus)))
species$Species <- as.factor(ifelse((trimws(as.character(species$Species))==trimws("NA")),NA,as.character(species$Species)))


data <- merge(x = data, y = species[ , c("OrganismCode", "Class", "Genus", "Species")], by = "OrganismCode", all.x=TRUE) # join species names to fish data

## CLEAN SPECIES ---------------

# dataset includes non-fish organisms -> need to filter these out

fish <- c("Osteichthyes", "Elasmobranchiomorphi")
data <- filter(data, Class %in% fish) # keep only fish

data <- data %>% 
  filter(!OrganismCode == "BASS" & !OrganismCode == "CENTRARCH" & !OrganismCode == "CFU" &
           !OrganismCode == "CLUPEIDAE" & !OrganismCode == "COTTIDAE" & !OrganismCode == "FISHUNK" &
           !OrganismCode == "GBU" & !OrganismCode == "LPU" & !OrganismCode == "MINU" & 
           !OrganismCode == "PRU" & !OrganismCode == "RFLU" & !OrganismCode == "ROCK" & 
           !OrganismCode == "SKRU" & !OrganismCode == "SMU" & !OrganismCode == "SSU" & 
           !OrganismCode == "STU" & !OrganismCode == "UKNLARV") # drop samples not ID'd to species level (unknowns in CommonName in species list)


## LENGTH --------------------

data %>% 
  filter(StandardLength == 0) %>% dplyr::count() # 5471 zeros

data$StandardLength[which(data$StandardLength == 0)] <- NA # unmeasured fish recorded as zeros -> change to NA

data %>% 
  filter(is.na(StandardLength)) %>% dplyr::count() # 5471 NA's

# drop measurements if noted that fork length instead of standard length used
data$StandardLength[which(data$CatchComments == "FL")] <- NA 
data$StandardLength[which(data$CatchComments == "fork length")] <- NA 
data$StandardLength[which(data$CatchComments == "Fork length")] <- NA 


## BULK COUNTS ----------------

# From Methods (https://portal.edirepository.org/nis/metadataviewer?packageid=edi.1947.1):
# "Only the first 15 individuals of a species is measured for standard length, with the remainder counted, to account for time constraints."

# all fish in a row (i.e., # in Count column) have the same length, species, etc

data <- data[rep(row.names(data), data$Count), 1:9] # each fish now = 1 row

data <- data %>% select(!c(Count)) # drop count column

## GEAR TYPE & ENVIRO --------------
inUrl14  <- "https://pasta.lternet.edu/package/data/eml/edi/1947/1/c5dd1b2697720fe692c529688d3f4f8d" 
infile14 <- tempfile()
try(download.file(inUrl14,infile14,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile14))) download.file(inUrl14,infile14,method="auto")


sampling <-read.csv(infile14,header=F 
                ,skip=1
                ,sep=","  
                ,quot='"' 
                , col.names=c(
                  "SampleRowID",     
                  "MethodCode",     
                  "StationCode",     
                  "SampleDate",     
                  "SampleTime",     
                  "QADone",     
                  "GearID",     
                  "WaterTemperature",     
                  "Salinity",     
                  "DO",     
                  "PctSaturation",     
                  "Secchi",     
                  "SpecificConductance",     
                  "TideCode",     
                  "UserName",     
                  "ElecCond"    ), check.names=TRUE)

unlink(infile14)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(sampling$SampleRowID)!="factor") sampling$SampleRowID<- as.factor(sampling$SampleRowID)
if (class(sampling$MethodCode)!="factor") sampling$MethodCode<- as.factor(sampling$MethodCode)
if (class(sampling$StationCode)!="factor") sampling$StationCode<- as.factor(sampling$StationCode)
if (class(sampling$SampleDate)!="factor") sampling$SampleDate<- as.factor(sampling$SampleDate)
if (class(sampling$SampleTime)!="factor") sampling$SampleTime<- as.factor(sampling$SampleTime)
if (class(sampling$QADone)!="factor") sampling$QADone<- as.factor(sampling$QADone)
if (class(sampling$GearID)!="factor") sampling$GearID<- as.factor(sampling$GearID)
if (class(sampling$WaterTemperature)=="factor") sampling$WaterTemperature <-as.numeric(levels(sampling$WaterTemperature))[as.integer(sampling$WaterTemperature) ]               
if (class(sampling$WaterTemperature)=="character") sampling$WaterTemperature <-as.numeric(sampling$WaterTemperature)
if (class(sampling$Salinity)=="factor") sampling$Salinity <-as.numeric(levels(sampling$Salinity))[as.integer(sampling$Salinity) ]               
if (class(sampling$Salinity)=="character") sampling$Salinity <-as.numeric(sampling$Salinity)
if (class(sampling$DO)=="factor") sampling$DO <-as.numeric(levels(sampling$DO))[as.integer(sampling$DO) ]               
if (class(sampling$DO)=="character") sampling$DO <-as.numeric(sampling$DO)
if (class(sampling$PctSaturation)=="factor") sampling$PctSaturation <-as.numeric(levels(sampling$PctSaturation))[as.integer(sampling$PctSaturation) ]               
if (class(sampling$PctSaturation)=="character") sampling$PctSaturation <-as.numeric(sampling$PctSaturation)
if (class(sampling$Secchi)=="factor") sampling$Secchi <-as.numeric(levels(sampling$Secchi))[as.integer(sampling$Secchi) ]               
if (class(sampling$Secchi)=="character") sampling$Secchi <-as.numeric(sampling$Secchi)
if (class(sampling$SpecificConductance)=="factor") sampling$SpecificConductance <-as.numeric(levels(sampling$SpecificConductance))[as.integer(sampling$SpecificConductance) ]               
if (class(sampling$SpecificConductance)=="character") sampling$SpecificConductance <-as.numeric(sampling$SpecificConductance)
if (class(sampling$TideCode)!="factor") sampling$TideCode<- as.factor(sampling$TideCode)
if (class(sampling$UserName)!="factor") sampling$UserName<- as.factor(sampling$UserName)
if (class(sampling$ElecCond)=="factor") sampling$ElecCond <-as.numeric(levels(sampling$ElecCond))[as.integer(sampling$ElecCond) ]               
if (class(sampling$ElecCond)=="character") sampling$ElecCond <-as.numeric(sampling$ElecCond)


data <- merge(x = data, y = sampling[ , c("SampleRowID", "MethodCode", "StationCode", "SampleDate", "WaterTemperature", "DO")], by = "SampleRowID", all.x=TRUE) # join sampling info to fish data

data$SampleDate <- as.Date(data$SampleDate, "%m/%d/%Y") # convert to date

## GEAR METHOD -----------
summary(data$MethodCode)
# most observations from otter trawl (OTR=301211) 
# BSEIN is what we used for YOLO (BSEIN = 268248) 
# other methods have fewer: hook and line (HKLN = 2539), midwater trawl (MWTR = 138), benthic sled (SLED = 13338)

# used OTR as method for the site

data <- data %>%
  filter(MethodCode == "OTR") # keep only seining data


## SAMPLING EFFORT --------------------
inUrl18  <- "https://pasta.lternet.edu/package/data/eml/edi/1947/1/036e46dcc72b6d9581f063c222ff5f83" 
infile18 <- tempfile()
try(download.file(inUrl18,infile18,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile18))) download.file(inUrl18,infile18,method="auto")


effort <-read.csv(infile18,header=F 
                ,skip=1
                ,sep=","  
                ,quot='"' 
                , col.names=c(
                  "TrawlRowID",     
                  "SampleRowID",     
                  "TowDuration",     
                  "TrawlComments",     
                  "StartMeter",     
                  "EndMeter"    ), check.names=TRUE)

unlink(infile18)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(effort$TrawlRowID)!="factor") effort$TrawlRowID<- as.factor(effort$TrawlRowID)
if (class(effort$SampleRowID)!="factor") effort$SampleRowID<- as.factor(effort$SampleRowID)
if (class(effort$TowDuration)=="factor") effort$TowDuration <-as.numeric(levels(effort$TowDuration))[as.integer(effort$TowDuration) ]               
if (class(effort$TowDuration)=="character") effort$TowDuration <-as.numeric(effort$TowDuration)
if (class(effort$TrawlComments)!="factor") effort$TrawlComments<- as.factor(effort$TrawlComments)
if (class(effort$StartMeter)=="factor") effort$StartMeter <-as.numeric(levels(effort$StartMeter))[as.integer(effort$StartMeter) ]               
if (class(effort$StartMeter)=="character") effort$StartMeter <-as.numeric(effort$StartMeter)
if (class(effort$EndMeter)=="factor") effort$EndMeter <-as.numeric(levels(effort$EndMeter))[as.integer(effort$EndMeter) ]               
if (class(effort$EndMeter)=="character") effort$EndMeter <-as.numeric(effort$EndMeter)

# Convert Missing Values to NA for non-dates

data <- merge(x = data, y = effort[ , c("SampleRowID", "TowDuration")], by = "SampleRowID", all.x=TRUE) # join sampling info to fish data

## ADD MIDSITES  ---------------

# Suisun Marsh has 4 ecologically/hydrographically distinct quadrants (NE, SE, NW, SW) 
# see map on pg 9 of report for subsite in midsite groupings:
# https://escholarship.org/content/qt170910gb/qt170910gb.pdf

data$MIDSITE <- NA

data$MIDSITE[which(data$StationCode == "DV1")] <- "NE"
data$MIDSITE[which(data$StationCode == "DV2")] <- "NE"
data$MIDSITE[which(data$StationCode == "DV3")] <- "NE"
data$MIDSITE[which(data$StationCode == "MZ6")] <- "NE"
data$MIDSITE[which(data$StationCode == "NS1")] <- "NE"
data$MIDSITE[which(data$StationCode == "NS2")] <- "NE"
data$MIDSITE[which(data$StationCode == "NS3")] <- "NE"

data$MIDSITE[which(data$StationCode == "BY1")] <- "NW"
data$MIDSITE[which(data$StationCode == "BY3")] <- "NW"
data$MIDSITE[which(data$StationCode == "CO1")] <- "NW"
data$MIDSITE[which(data$StationCode == "CO2")] <- "NW"
data$MIDSITE[which(data$StationCode == "PT1")] <- "NW"
data$MIDSITE[which(data$StationCode == "PT2")] <- "NW"
data$MIDSITE[which(data$StationCode == "SB1")] <- "NW"
data$MIDSITE[which(data$StationCode == "SB2")] <- "NW"
data$MIDSITE[which(data$StationCode == "SU1")] <- "NW"
data$MIDSITE[which(data$StationCode == "SU2")] <- "NW"

data$MIDSITE[which(data$StationCode == "MZ1")] <- "SE"
data$MIDSITE[which(data$StationCode == "MZ2")] <- "SE"

data$MIDSITE[which(data$StationCode == "GY1")] <- "SW"
data$MIDSITE[which(data$StationCode == "GY2")] <- "SW"
data$MIDSITE[which(data$StationCode == "GY3")] <- "SW"
data$MIDSITE[which(data$StationCode == "SU3")] <- "SW"
data$MIDSITE[which(data$StationCode == "SU4")] <- "SW"

# use only main sampling sites that fall within one of those locations (~7% of observations dropped)
data <- data %>% filter(!is.na(MIDSITE)) # drop stations if they don't correspond to midsite

## FINALIZE FISH ---------------

data$Taxa <- paste(data$Genus, data$Species)

# at the end of the fish section, we should have:
# DATE, SITE/SUBSITE, SP_CODE, SIZE, SCIENTIFIC_NAME, COMMON_NAME (not for NEON stuff), YEAR, EFFORT

fish <- data %>% 
  dplyr::rename(DATE = SampleDate,
                SP_CODE = OrganismCode,
                SCI_NAME = Taxa,
                SIZE = StandardLength,
                MIDSITE = MIDSITE,
                SUBSITE = StationCode,
                EFFORT = TowDuration) %>% 
  mutate(YEAR = year(DATE)) %>% 
  select(DATE, MIDSITE, SUBSITE, SP_CODE, SIZE, SCI_NAME, YEAR, EFFORT)


# PART #2: TEMP ------

## CALCULATE TEMP VARIABLES --------------------
sampling$SampleDate <- as.Date(sampling$SampleDate, "%m/%d/%Y") # convert to date

sampling$YEAR <- substr(sampling$SampleDate, 1, 4) # get year

## ADD MIDSITES
sampling$MIDSITE <- NA 

sampling$MIDSITE[which(sampling$StationCode == "DV1")] <- "NE"
sampling$MIDSITE[which(sampling$StationCode == "DV2")] <- "NE"
sampling$MIDSITE[which(sampling$StationCode == "DV3")] <- "NE"
sampling$MIDSITE[which(sampling$StationCode == "MZ6")] <- "NE"
sampling$MIDSITE[which(sampling$StationCode == "NS1")] <- "NE"
sampling$MIDSITE[which(sampling$StationCode == "NS2")] <- "NE"
sampling$MIDSITE[which(sampling$StationCode == "NS3")] <- "NE"

sampling$MIDSITE[which(sampling$StationCode == "BY1")] <- "NW"
sampling$MIDSITE[which(sampling$StationCode == "BY3")] <- "NW"
sampling$MIDSITE[which(sampling$StationCode == "CO1")] <- "NW"
sampling$MIDSITE[which(sampling$StationCode == "CO2")] <- "NW"
sampling$MIDSITE[which(sampling$StationCode == "PT1")] <- "NW"
sampling$MIDSITE[which(sampling$StationCode == "PT2")] <- "NW"
sampling$MIDSITE[which(sampling$StationCode == "SB1")] <- "NW"
sampling$MIDSITE[which(sampling$StationCode == "SB2")] <- "NW"
sampling$MIDSITE[which(sampling$StationCode == "SU1")] <- "NW"
sampling$MIDSITE[which(sampling$StationCode == "SU2")] <- "NW"

sampling$MIDSITE[which(sampling$StationCode == "MZ1")] <- "SE"
sampling$MIDSITE[which(sampling$StationCode == "MZ2")] <- "SE"

sampling$MIDSITE[which(sampling$StationCode == "GY1")] <- "SW"
sampling$MIDSITE[which(sampling$StationCode == "GY2")] <- "SW"
sampling$MIDSITE[which(sampling$StationCode == "GY3")] <- "SW"
sampling$MIDSITE[which(sampling$StationCode == "SU3")] <- "SW"
sampling$MIDSITE[which(sampling$StationCode == "SU4")] <- "SW"

sampling <- sampling %>% filter(!is.na(MIDSITE)) # drop stations if they don't correspond to midsite

# VARIABLES
# A) annual average temperature (rational: growing season avg would require identifying growing season at each site)
# B) annual mean daily max temperature
# C) annual mean daily min temperature

temp_final <- sampling %>% 
  group_by(YEAR, MIDSITE, SampleDate) %>% 
  reframe(mean_daily_temp = mean(WaterTemperature, na.rm = T),
          mean_max_temp = max(WaterTemperature, na.rm = T),
          mean_min_temp = min(WaterTemperature, na.rm = T))  %>% 
  filter(!mean_daily_temp == "NaN") %>%
  ungroup() %>%
  group_by(YEAR, MIDSITE) %>%
  reframe(mean_daily_temp = mean(mean_daily_temp, na.rm = T),
          mean_max_temp = mean(mean_max_temp, na.rm = T),
          mean_min_temp = mean(mean_min_temp,na.rm = T)) # get variables A, B, C

ggplot(sampling, aes(x=SampleDate, y = WaterTemperature, color = MIDSITE)) +
  geom_point(alpha = .5)
# drop first and last years because only have data for part of year so average is wonky

temp_final <- temp_final %>% 
  filter(!YEAR == 1979 & !YEAR == 2025) # drop years with missing data

temp_final$YEAR <- as.numeric(temp_final$YEAR)
temp_final$YEAR <- temp_final$YEAR + 1 # offset year before joining to fish data


#PART #3: DO ------

## CALCULATE DO VARIABLES --------------------

# VARIABLES
# D) annual average DO (might scratch?)
# E) annual mean daily DO
# F) annual mean min DO

daily_DO <- sampling %>% 
  group_by(YEAR, MIDSITE, SampleDate) %>% 
  reframe(mean_daily_DO = mean(DO, na.rm = T),
          mean_min_DO = min(DO, na.rm = T)) # get daily mean & min

daily_DO <- daily_DO %>% 
  filter(!mean_daily_DO == "NaN") %>%
  filter(!mean_min_DO == "Inf") %>%
  group_by(YEAR, MIDSITE) %>% 
  reframe(mean_daily_DO = mean(mean_daily_DO, na.rm = T),
          mean_min_DO = mean(mean_min_DO, na.rm = T))  # variables E & F 

annual_DO <- sampling %>% 
  group_by(YEAR, MIDSITE) %>%
  reframe(annual_avg_DO = mean(DO, na.rm = T))

DO_final <- merge(daily_DO, annual_DO, by = c("YEAR", "MIDSITE"), all = T)

ggplot(sampling, aes(x=SampleDate, y = DO, color = MIDSITE)) +
  geom_point() # began collecting DO data in 2000

DO_final <- DO_final %>% 
  filter(!YEAR < 2000) # drop years with missing data

DO_final$YEAR <- as.numeric(DO_final$YEAR)
DO_final$YEAR <- DO_final$YEAR + 1 # offset year before joining to fish data

#finalize environmental data 

enviro_final <- temp_final %>%
  merge(DO_final, by=c("YEAR", "MIDSITE"), all = T) # use merge not join--join drops years if temp or DO missing for year

#PART #4: HARMONIZE TEMP & DO with FISH

intermediate <- left_join(fish, enviro_final, by = c("YEAR", "MIDSITE"))

# PART 5: FINALIZE INTERMEDIATE DATA --------------------

#Here, you'll want to rename any columns you already fit to have our required column naming conventions

intermediate.names() # note that MIDSITE is not one of the options so need to manually save vs using the intermediate.prep function

colnames(intermediate)

#intermediate.prep(intermediate)
write.csv(intermediate, "UCD_SUMA_intermediate.csv", row.names=FALSE)

# (OPTIONAL) DATA VIZ --------------------
#Play around with subsites by changing to True! The default is false. 

plot.top5(intermediate, species_col = "SCI_NAME", subsite = F)

plot.presence(intermediate, 
              species_col = "SCI_NAME",
              subsite = F)

plot.speciesaccum(count_check,
                  species_col = "SP_CODE",
                  subsite = F)
