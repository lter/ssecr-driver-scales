#_________________________________
# IEP YOLO - Yolo Bypass Fish Monitoring Program (v.5)
# Source: https://doi.org/10.6073/pasta/9eb0928f980e28f5df489eda4896b0da
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

dataset <- "IEP_YOLO"

# PART #1: FISH  ---------------------

## LOAD FISH DATA ---------------------

# make sure to double check above you've set the "dataset" object to the name of the subfolder where this data is going to live! 

# loading fish dataset from EDI repo
inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/233/5/402732c0e6c782db8b8229c3b9310afa" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


data <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "OrganismID",     
                 "FishGenID",     
                 "EventID",     
                 "OrganismCode",     
                 "ForkLength",     
                 "TotalLength",     
                 "Weight",     
                 "Sex",     
                 "RaceByLength",     
                 "StageCode",     
                 "Dead",     
                 "Expression",     
                 "GeneticSample",     
                 "FishIDComments"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
if (class(data$OrganismID)!="factor") data$OrganismID<- as.factor(data$OrganismID)
if (class(data$FishGenID)!="factor") data$FishGenID<- as.factor(data$FishGenID)
if (class(data$EventID)!="factor") data$EventID<- as.factor(data$EventID)
if (class(data$OrganismCode)!="factor") data$OrganismCode<- as.factor(data$OrganismCode)
if (class(data$ForkLength)=="factor") data$ForkLength <-as.numeric(levels(data$ForkLength))[as.integer(data$ForkLength) ]               
if (class(data$ForkLength)=="character") data$ForkLength <-as.numeric(data$ForkLength)
if (class(data$TotalLength)=="factor") data$TotalLength <-as.numeric(levels(data$TotalLength))[as.integer(data$TotalLength) ]               
if (class(data$TotalLength)=="character") data$TotalLength <-as.numeric(data$TotalLength)
if (class(data$Weight)=="factor") data$Weight <-as.numeric(levels(data$Weight))[as.integer(data$Weight) ]               
if (class(data$Weight)=="character") data$Weight <-as.numeric(data$Weight)
if (class(data$Sex)!="factor") data$Sex<- as.factor(data$Sex)
if (class(data$RaceByLength)!="factor") data$RaceByLength<- as.factor(data$RaceByLength)
if (class(data$StageCode)!="factor") data$StageCode<- as.factor(data$StageCode)
if (class(data$Dead)!="factor") data$Dead<- as.factor(data$Dead)
if (class(data$Expression)!="factor") data$Expression<- as.factor(data$Expression)
if (class(data$GeneticSample)!="factor") data$GeneticSample<- as.factor(data$GeneticSample)
if (class(data$FishIDComments)!="factor") data$FishIDComments<- as.factor(data$FishIDComments)

# Convert Missing Values to NA for non-dates

data$FishGenID <- as.factor(ifelse((trimws(as.character(data$FishGenID))==trimws("NA")),NA,as.character(data$FishGenID)))
data$ForkLength <- ifelse((trimws(as.character(data$ForkLength))==trimws("NA")),NA,data$ForkLength)               
suppressWarnings(data$ForkLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(data$ForkLength))==as.character(as.numeric("NA"))),NA,data$ForkLength))
data$TotalLength <- ifelse((trimws(as.character(data$TotalLength))==trimws("NA")),NA,data$TotalLength)               
suppressWarnings(data$TotalLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(data$TotalLength))==as.character(as.numeric("NA"))),NA,data$TotalLength))
data$Weight <- ifelse((trimws(as.character(data$Weight))==trimws("NA")),NA,data$Weight)               
suppressWarnings(data$Weight <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(data$Weight))==as.character(as.numeric("NA"))),NA,data$Weight))
data$Sex <- as.factor(ifelse((trimws(as.character(data$Sex))==trimws("NA")),NA,as.character(data$Sex)))
data$RaceByLength <- as.factor(ifelse((trimws(as.character(data$RaceByLength))==trimws("NA")),NA,as.character(data$RaceByLength)))
data$StageCode <- as.factor(ifelse((trimws(as.character(data$StageCode))==trimws("NA")),NA,as.character(data$StageCode)))
data$Dead <- as.factor(ifelse((trimws(as.character(data$Dead))==trimws("NA")),NA,as.character(data$Dead)))
data$Expression <- as.factor(ifelse((trimws(as.character(data$Expression))==trimws("NA")),NA,as.character(data$Expression)))
data$GeneticSample <- as.factor(ifelse((trimws(as.character(data$GeneticSample))==trimws("NA")),NA,as.character(data$GeneticSample)))
data$FishIDComments <- as.factor(ifelse((trimws(as.character(data$FishIDComments))==trimws("NA")),NA,as.character(data$FishIDComments)))


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
  select(!c(FishGenID, TotalLength, Weight, Sex, RaceByLength, StageCode, Dead, Expression, GeneticSample, FishIDComments))
# using ForkLength instead of TotalLength because 1114 vs 325 observations

## LENGTH --------------------

data %>% 
  filter(ForkLength == 0) %>% dplyr::count() # 0 zeros

data %>% 
  filter(is.na(ForkLength)) %>% dplyr::count() # 434 NA's

## TAXONOMY LIST ---------------
inUrl8  <- "https://pasta.lternet.edu/package/data/eml/edi/233/5/405122cb55c6996661c0dee20ab77a6c" 
infile8 <- tempfile()
try(download.file(inUrl8,infile8,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile8))) download.file(inUrl8,infile8,method="auto")

species <-read.csv(infile8,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "OrganismCode",     
                 "IEPFishCode",     
                 "CommonName",     
                 "Native",     
                 "Phylum",     
                 "Class",     
                 "Order",     
                 "Family",     
                 "Genus",     
                 "Species",     
                 "Taxa"    ), check.names=TRUE)

unlink(infile8)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(species$OrganismCode)!="factor") species$OrganismCode<- as.factor(species$OrganismCode)
if (class(species$IEPFishCode)!="factor") species$IEPFishCode<- as.factor(species$IEPFishCode)
if (class(species$CommonName)!="factor") species$CommonName<- as.factor(species$CommonName)
if (class(species$Native)!="factor") species$Native<- as.factor(species$Native)
if (class(species$Phylum)!="factor") species$Phylum<- as.factor(species$Phylum)
if (class(species$Class)!="factor") species$Class<- as.factor(species$Class)
if (class(species$Order)!="factor") species$Order<- as.factor(species$Order)
if (class(species$Family)!="factor") species$Family<- as.factor(species$Family)
if (class(species$Genus)!="factor") species$Genus<- as.factor(species$Genus)
if (class(species$Species)!="factor") species$Species<- as.factor(species$Species)
if (class(species$Taxa)!="factor") species$Taxa<- as.factor(species$Taxa)

# Convert Missing Values to NA for non-dates

species$Native <- as.factor(ifelse((trimws(as.character(species$Native))==trimws("NA")),NA,as.character(species$Native)))
species$Phylum <- as.factor(ifelse((trimws(as.character(species$Phylum))==trimws("NA")),NA,as.character(species$Phylum)))
species$Class <- as.factor(ifelse((trimws(as.character(species$Class))==trimws("NA")),NA,as.character(species$Class)))
species$Order <- as.factor(ifelse((trimws(as.character(species$Order))==trimws("NA")),NA,as.character(species$Order)))
species$Family <- as.factor(ifelse((trimws(as.character(species$Family))==trimws("NA")),NA,as.character(species$Family)))
species$Genus <- as.factor(ifelse((trimws(as.character(species$Genus))==trimws("NA")),NA,as.character(species$Genus)))
species$Species <- as.factor(ifelse((trimws(as.character(species$Species))==trimws("NA")),NA,as.character(species$Species)))

data <- merge(x = data, y = species[ , c("OrganismCode", "Taxa", "Species")], by = "OrganismCode", all.x=TRUE) # join species names to fish data

## CLEAN SPECIES ---------------

data <- data %>% 
  filter(!Species == "unknown") # drop unidentified samples

data <- data %>% 
  filter(!Species == "spp.") # drop samples not identified to species level 

## GEAR TYPE & ENVIRO --------------
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/233/5/4488201fee45953b001f70acf30f7734" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

sampling <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "EventID",     
                 "StationCode",     
                 "Datetime",     
                 "SampleDate",     
                 "WaterTemp",     
                 "SpecificConductance",     
                 "Conductivity",     
                 "Turbidity",     
                 "DO",     
                 "pH",     
                 "Secchi",     
                 "Tide",     
                 "WeatherCode",     
                 "VegetationRank",     
                 "SubstrateCode",     
                 "HabitatType",     
                 "MicrocystisRank",     
                 "MethodCode",     
                 "GearCode",     
                 "GearConditionCode",     
                 "SampleAltered",     
                 "FieldComments",     
                 "Flag_WQ",     
                 "Comment_WQ",     
                 "Duplicated"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(sampling$EventID)!="factor") sampling$EventID<- as.factor(sampling$EventID)
if (class(sampling$StationCode)!="factor") sampling$StationCode<- as.factor(sampling$StationCode)                                   
# attempting to convert sampling$Datetime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%m/%d/%Y %H:%M" 
tmp2Datetime<-as.POSIXct(sampling$Datetime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(sampling[sampling$Datetime != "",]) == length(tmp2Datetime[!is.na(tmp2Datetime)])){sampling$Datetime <- tmp2Datetime } else {print("Date conversion failed for sampling$Datetime. Please inspect the data and do the date conversion yourself.")}                                                                    

# attempting to convert sampling$SampleDate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%m/%d/%Y"
tmp2SampleDate<-as.Date(sampling$SampleDate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(sampling[sampling$SampleDate != "",]) == length(tmp2SampleDate[!is.na(tmp2SampleDate)])){sampling$SampleDate <- tmp2SampleDate } else {print("Date conversion failed for sampling$SampleDate. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(sampling$WaterTemp)=="factor") sampling$WaterTemp <-as.numeric(levels(sampling$WaterTemp))[as.integer(sampling$WaterTemp) ]               
if (class(sampling$WaterTemp)=="character") sampling$WaterTemp <-as.numeric(sampling$WaterTemp)
if (class(sampling$SpecificConductance)=="factor") sampling$SpecificConductance <-as.numeric(levels(sampling$SpecificConductance))[as.integer(sampling$SpecificConductance) ]               
if (class(sampling$SpecificConductance)=="character") sampling$SpecificConductance <-as.numeric(sampling$SpecificConductance)
if (class(sampling$Conductivity)=="factor") sampling$Conductivity <-as.numeric(levels(sampling$Conductivity))[as.integer(sampling$Conductivity) ]               
if (class(sampling$Conductivity)=="character") sampling$Conductivity <-as.numeric(sampling$Conductivity)
if (class(sampling$Turbidity)=="factor") sampling$Turbidity <-as.numeric(levels(sampling$Turbidity))[as.integer(sampling$Turbidity) ]               
if (class(sampling$Turbidity)=="character") sampling$Turbidity <-as.numeric(sampling$Turbidity)
if (class(sampling$DO)=="factor") sampling$DO <-as.numeric(levels(sampling$DO))[as.integer(sampling$DO) ]               
if (class(sampling$DO)=="character") sampling$DO <-as.numeric(sampling$DO)
if (class(sampling$pH)=="factor") sampling$pH <-as.numeric(levels(sampling$pH))[as.integer(sampling$pH) ]               
if (class(sampling$pH)=="character") sampling$pH <-as.numeric(sampling$pH)
if (class(sampling$Secchi)=="factor") sampling$Secchi <-as.numeric(levels(sampling$Secchi))[as.integer(sampling$Secchi) ]               
if (class(sampling$Secchi)=="character") sampling$Secchi <-as.numeric(sampling$Secchi)
if (class(sampling$Tide)!="factor") sampling$Tide<- as.factor(sampling$Tide)
if (class(sampling$WeatherCode)!="factor") sampling$WeatherCode<- as.factor(sampling$WeatherCode)
if (class(sampling$VegetationRank)!="factor") sampling$VegetationRank<- as.factor(sampling$VegetationRank)
if (class(sampling$SubstrateCode)!="factor") sampling$SubstrateCode<- as.factor(sampling$SubstrateCode)
if (class(sampling$HabitatType)!="factor") sampling$HabitatType<- as.factor(sampling$HabitatType)
if (class(sampling$MicrocystisRank)!="factor") sampling$MicrocystisRank<- as.factor(sampling$MicrocystisRank)
if (class(sampling$MethodCode)!="factor") sampling$MethodCode<- as.factor(sampling$MethodCode)
if (class(sampling$GearCode)!="factor") sampling$GearCode<- as.factor(sampling$GearCode)
if (class(sampling$GearConditionCode)!="factor") sampling$GearConditionCode<- as.factor(sampling$GearConditionCode)
if (class(sampling$SampleAltered)!="factor") sampling$SampleAltered<- as.factor(sampling$SampleAltered)
if (class(sampling$FieldComments)!="factor") sampling$FieldComments<- as.factor(sampling$FieldComments)
if (class(sampling$Flag_WQ)!="factor") sampling$Flag_WQ<- as.factor(sampling$Flag_WQ)
if (class(sampling$Comment_WQ)!="factor") sampling$Comment_WQ<- as.factor(sampling$Comment_WQ)
if (class(sampling$Duplicated)!="factor") sampling$Duplicated<- as.factor(sampling$Duplicated)

# Convert Missing Values to NA for non-dates

sampling$WaterTemp <- ifelse((trimws(as.character(sampling$WaterTemp))==trimws("NA")),NA,sampling$WaterTemp)               
suppressWarnings(sampling$WaterTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(sampling$WaterTemp))==as.character(as.numeric("NA"))),NA,sampling$WaterTemp))
sampling$SpecificConductance <- ifelse((trimws(as.character(sampling$SpecificConductance))==trimws("NA")),NA,sampling$SpecificConductance)               
suppressWarnings(sampling$SpecificConductance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(sampling$SpecificConductance))==as.character(as.numeric("NA"))),NA,sampling$SpecificConductance))
sampling$Conductivity <- ifelse((trimws(as.character(sampling$Conductivity))==trimws("NA")),NA,sampling$Conductivity)               
suppressWarnings(sampling$Conductivity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(sampling$Conductivity))==as.character(as.numeric("NA"))),NA,sampling$Conductivity))
sampling$Turbidity <- ifelse((trimws(as.character(sampling$Turbidity))==trimws("NA")),NA,sampling$Turbidity)               
suppressWarnings(sampling$Turbidity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(sampling$Turbidity))==as.character(as.numeric("NA"))),NA,sampling$Turbidity))
sampling$DO <- ifelse((trimws(as.character(sampling$DO))==trimws("NA")),NA,sampling$DO)               
suppressWarnings(sampling$DO <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(sampling$DO))==as.character(as.numeric("NA"))),NA,sampling$DO))
sampling$pH <- ifelse((trimws(as.character(sampling$pH))==trimws("NA")),NA,sampling$pH)               
suppressWarnings(sampling$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(sampling$pH))==as.character(as.numeric("NA"))),NA,sampling$pH))
sampling$Secchi <- ifelse((trimws(as.character(sampling$Secchi))==trimws("NA")),NA,sampling$Secchi)               
suppressWarnings(sampling$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(sampling$Secchi))==as.character(as.numeric("NA"))),NA,sampling$Secchi))
sampling$Tide <- as.factor(ifelse((trimws(as.character(sampling$Tide))==trimws("NA")),NA,as.character(sampling$Tide)))
sampling$WeatherCode <- as.factor(ifelse((trimws(as.character(sampling$WeatherCode))==trimws("NA")),NA,as.character(sampling$WeatherCode)))
sampling$VegetationRank <- as.factor(ifelse((trimws(as.character(sampling$VegetationRank))==trimws("NA")),NA,as.character(sampling$VegetationRank)))
sampling$SubstrateCode <- as.factor(ifelse((trimws(as.character(sampling$SubstrateCode))==trimws("NA")),NA,as.character(sampling$SubstrateCode)))
sampling$HabitatType <- as.factor(ifelse((trimws(as.character(sampling$HabitatType))==trimws("NA")),NA,as.character(sampling$HabitatType)))
sampling$MicrocystisRank <- as.factor(ifelse((trimws(as.character(sampling$MicrocystisRank))==trimws("NA")),NA,as.character(sampling$MicrocystisRank)))
sampling$MethodCode <- as.factor(ifelse((trimws(as.character(sampling$MethodCode))==trimws("NA")),NA,as.character(sampling$MethodCode)))
sampling$GearCode <- as.factor(ifelse((trimws(as.character(sampling$GearCode))==trimws("NA")),NA,as.character(sampling$GearCode)))
sampling$GearConditionCode <- as.factor(ifelse((trimws(as.character(sampling$GearConditionCode))==trimws("NA")),NA,as.character(sampling$GearConditionCode)))
sampling$SampleAltered <- as.factor(ifelse((trimws(as.character(sampling$SampleAltered))==trimws("NA")),NA,as.character(sampling$SampleAltered)))
sampling$FieldComments <- as.factor(ifelse((trimws(as.character(sampling$FieldComments))==trimws("NA")),NA,as.character(sampling$FieldComments)))

data <- merge(x = data, y = sampling[ , c("EventID", "StationCode", "MethodCode", "SampleDate")], by = "EventID", all.x=TRUE) # join sampling info to fish data

## GEAR METHOD -----------
summary(data$MethodCode)
# more observations from seining (BSEIN=112672) than fyke trap (FKTR=42199) or screw trap (RSTR=66428) -> keep seining observations 

data <- data %>% 
  filter(MethodCode == "BSEIN") # keep only seining data

data <- data %>% 
  filter(!OrganismCode == "NONE") # filter out observations with no fish

## SAMPLING EFFORT --------------------

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/233/5/1ca343f66d18c8cde0ec58ad893aad10" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


effort <-read.csv(infile1,header=F 
                  ,skip=1
                  ,sep=","  
                  ,quot='"' 
                  , col.names=c(
                    "EventID",     
                    "SeineLength",     
                    "SeineWidth",     
                    "SeineDepth",     
                    "SeineVolume",     
                    "TrapStatus",     
                    "TrapHours"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(effort$EventID)!="factor") effort$EventID<- as.factor(effort$EventID)
if (class(effort$SeineLength)=="factor") effort$SeineLength <-as.numeric(levels(effort$SeineLength))[as.integer(effort$SeineLength) ]               
if (class(effort$SeineLength)=="character") effort$SeineLength <-as.numeric(effort$SeineLength)
if (class(effort$SeineWidth)=="factor") effort$SeineWidth <-as.numeric(levels(effort$SeineWidth))[as.integer(effort$SeineWidth) ]               
if (class(effort$SeineWidth)=="character") effort$SeineWidth <-as.numeric(effort$SeineWidth)
if (class(effort$SeineDepth)=="factor") effort$SeineDepth <-as.numeric(levels(effort$SeineDepth))[as.integer(effort$SeineDepth) ]               
if (class(effort$SeineDepth)=="character") effort$SeineDepth <-as.numeric(effort$SeineDepth)
if (class(effort$SeineVolume)=="factor") effort$SeineVolume <-as.numeric(levels(effort$SeineVolume))[as.integer(effort$SeineVolume) ]               
if (class(effort$SeineVolume)=="character") effort$SeineVolume <-as.numeric(effort$SeineVolume)
if (class(effort$TrapStatus)!="factor") effort$TrapStatus<- as.factor(effort$TrapStatus)
if (class(effort$TrapHours)=="factor") effort$TrapHours <-as.numeric(levels(effort$TrapHours))[as.integer(effort$TrapHours) ]               
if (class(effort$TrapHours)=="character") effort$TrapHours <-as.numeric(effort$TrapHours)

# Convert Missing Values to NA for non-dates

effort$SeineLength <- ifelse((trimws(as.character(effort$SeineLength))==trimws("NA")),NA,effort$SeineLength)               
suppressWarnings(effort$SeineLength <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(effort$SeineLength))==as.character(as.numeric("NA"))),NA,effort$SeineLength))
effort$SeineWidth <- ifelse((trimws(as.character(effort$SeineWidth))==trimws("NA")),NA,effort$SeineWidth)               
suppressWarnings(effort$SeineWidth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(effort$SeineWidth))==as.character(as.numeric("NA"))),NA,effort$SeineWidth))
effort$SeineDepth <- ifelse((trimws(as.character(effort$SeineDepth))==trimws("NA")),NA,effort$SeineDepth)               
suppressWarnings(effort$SeineDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(effort$SeineDepth))==as.character(as.numeric("NA"))),NA,effort$SeineDepth))
effort$SeineVolume <- ifelse((trimws(as.character(effort$SeineVolume))==trimws("NA")),NA,effort$SeineVolume)               
suppressWarnings(effort$SeineVolume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(effort$SeineVolume))==as.character(as.numeric("NA"))),NA,effort$SeineVolume))
effort$TrapStatus <- as.factor(ifelse((trimws(as.character(effort$TrapStatus))==trimws("NA")),NA,as.character(effort$TrapStatus)))
effort$TrapHours <- ifelse((trimws(as.character(effort$TrapHours))==trimws("NA")),NA,effort$TrapHours)               
suppressWarnings(effort$TrapHours <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(effort$TrapHours))==as.character(as.numeric("NA"))),NA,effort$TrapHours))

data <- merge(x = data, y = effort[ , c("EventID", "SeineVolume")], by = "EventID", all.x=TRUE) # join sampling info to fish data

## FINALIZE FISH ---------------

# at the end of the fish section, we should have:
# DATE, SITE/SUBSITE, SP_CODE, SIZE, SCIENTIFIC_NAME, COMMON_NAME (not for NEON stuff), YEAR, EFFORT

fish <- data %>% 
  dplyr::rename(DATE = SampleDate,
                SP_CODE = OrganismCode,
                SCI_NAME = Taxa,
                SIZE = ForkLength,
                SUBSITE = StationCode,
                EFFORT = SeineVolume) %>% 
  mutate(YEAR = year(DATE)) %>% 
  select(DATE, SUBSITE, SP_CODE, SIZE, SCI_NAME, YEAR, EFFORT)


# PART #2: TEMP ------

## QC --------------------    
sampling$Flag_WQ <- as.character(sampling$Flag_WQ)
# Order in Flag_WQ column is WaterTemp, Conductivity, SpecificConductance, DO, pH, Turbidity, Secchi
sampling$temp_flag <- substr(sampling$Flag_WQ, 1, 1) # create temp flag

# NA and 1 = acceptable; 2 = suspicious; 3 = highly suspicious

temp <- sampling %>% 
  filter(is.na(temp_flag) | temp_flag == 1 | temp_flag == "N")  # keep only NA's or 1's

## CALCULATE TEMP VARIABLES --------------------
temp$YEAR <- substr(temp$SampleDate, 1, 4) # get year

# VARIABLES
# A) annual average temperature (rational: growing season avg would require identifying growing season at each site)
# B) annual mean daily max temperature
# C) annual mean daily min temperature

temp_final <- temp %>% 
  group_by(YEAR, SampleDate) %>% 
  reframe(mean_daily_temp = mean(WaterTemp, na.rm = T),
          mean_max_temp = max(WaterTemp, na.rm = T),
          mean_min_temp = min(WaterTemp, na.rm = T))  %>% 
  filter(!mean_daily_temp == "NaN") %>%
  ungroup() %>%
  group_by(YEAR) %>%
  reframe(mean_daily_temp = mean(mean_daily_temp, na.rm = T),
          mean_max_temp = mean(mean_max_temp, na.rm = T),
          mean_min_temp = mean(mean_min_temp,na.rm = T)) # get variables A, B, C

ggplot(temp, aes(x=SampleDate, y = WaterTemp)) +
  geom_point()
# 1999 and 2020 have big gaps in temp data that artifically lowers means -> drop years

temp_final <- temp_final %>% 
  filter(!YEAR == 1999 & !YEAR == 2020) # drop years with missing data

temp_final$YEAR <- as.numeric(temp_final$YEAR)
temp_final$YEAR <- temp_final$YEAR + 1 # offset year before joining to fish data


#PART #3: DO ------

## QC --------------------    

splitQC <- sampling %>% select(Flag_WQ)
splitQC <- cSplit(splitQC, "Flag_WQ", ",")

# Order in Flag_WQ column is WaterTemp, Conductivity, SpecificConductance, DO, pH, Turbidity, Secchi

splitQC <- splitQC %>% select(Flag_WQ_4) %>%
  dplyr::rename(DO_flag = Flag_WQ_4)

sampling <- cbind(sampling, splitQC)

# NA and 1 = acceptable; 2 = suspicious; 3 = highly suspicious

DO <- sampling %>% 
  filter(is.na(DO_flag) | DO_flag == 1) # keep only NA's or 1's

## CALCULATE DO VARIABLES --------------------

DO$YEAR <- substr(DO$SampleDate, 1, 4) # get year

# VARIABLES
# D) annual average DO (might scratch?)
# E) annual mean daily DO
# F) annual mean min DO

daily_DO <- DO %>% 
  group_by(YEAR, SampleDate) %>% 
  reframe(mean_daily_DO = mean(DO, na.rm = T),
          mean_min_DO = min(DO, na.rm = T)) # get daily mean & min

daily_DO <- daily_DO %>% 
  filter(!mean_daily_DO == "NaN") %>%
  filter(!mean_min_DO == "Inf") %>%
  group_by(YEAR) %>% 
  reframe(mean_daily_DO = mean(mean_daily_DO, na.rm = T),
          mean_min_DO = mean(mean_min_DO, na.rm = T))  # variables E & F 

annual_DO <- DO %>% 
  group_by(YEAR) %>%
  reframe(annual_avg_DO = mean(DO, na.rm = T))

DO_final <- merge(daily_DO, annual_DO, by = "YEAR", all = T)

ggplot(DO, aes(x=SampleDate, y = DO)) +
  geom_point() # DO data missing before 2008 & during 2020

DO_final <- DO_final %>% 
  filter(!YEAR < 2008 & !YEAR == 2020) # drop years with missing data

DO_final$YEAR <- as.numeric(DO_final$YEAR)
DO_final$YEAR <- DO_final$YEAR + 1 # offset year before joining to fish data

#finalize environmental data 

enviro_final <- temp_final %>%
  merge(DO_final, by=c("YEAR"), all = T) # use merge not join--join drops years if temp or DO missing for year

#PART #4: HARMONIZE TEMP & DO with FISH

intermediate <- left_join(fish, enviro_final, by = c("YEAR"))

# PART 5: FINALIZE INTERMEDIATE DATA --------------------

#Here, you'll want to rename any columns you already fit to have our required column naming conventions

intermediate.names()

colnames(intermediate)

intermediate.prep(intermediate)

# (OPTIONAL) DATA VIZ --------------------
#Play around with subsites by changing to True! The default is false. 

plot.top5(intermediate, species_col = "SCI_NAME", subsite = F)

plot.presence(intermediate, 
              species_col = "SCI_NAME",
              subsite = F)

plot.speciesaccum(count_check,
                  species_col = "SP_CODE",
                  subsite = F)
