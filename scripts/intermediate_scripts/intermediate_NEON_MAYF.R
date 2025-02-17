#_________________________________
# NEON - MAYF - Fish electrofishing, gill netting, and fyke netting counts (https://doi.org/10.48443/ap3d-rp07)
# SCALES/ SSECR                  
# Sierra Perez   
# R version 4.4.2 (2024-10-31)
#_________________________________

# SETUP ---------------------

rm(list = ls())

## LOAD PACKAGES AND FUNCTIONS --------------------

#install.packages("librarian")

librarian::shelf(supportR, tidyverse, summarytools, 
                 datacleanr, lterdatasampler,
                 cowplot, gt,
                 vegan, neonUtilities)

source(file = file.path("scripts",
                        "functions.R"))

source(file = file.path("scripts",
                        "viz_ideas.R"))


#create directories for project if they don't already exist

intermediate.directories()


# set "site name" based on the name of your raw data folder and what the output should look like for naming convention, This step is critical!  

dataset <- "NEON_MAYF"

## LOAD ---------------------

#make sure to double check above you've set the "dataset" object to the name of the subfolder where this data is going to live! 

data_type <- "fish"

neon_download(site = "MAYF", 
              dpID = "DP1.20107.001", 
              dataset,
              data_type = "fish")

#add file name here of the downloaded zip folder

#here, add the name of the folder to find everything in for the NEON stacked data! 

folder <- "filesToStack20107"

neon_stack(folder = folder,
           dataset,
           data_type = "fish")

#you will need to change this for your own data 

data <- read.csv(file = file.path("data",
                                  "raw_data",
                                  dataset,
                                  data_type,
                                  folder,
                                  "stackedFiles",
                                  "fsh_perFish.csv"))

# CHECKS --------------------

#get generic output on data structures and unique values for each - this you can keep! 
# It's set up so that it will name it based on your unique site value you named earlier. 

summarytools::view(summarytools::dfSummary(data),
                   file = file.path("data",
                                    "metadata",
                                    paste0(dataset, 
                                           "_datasummary.html")))

## DUPLICATES --------------------

data[duplicated(data),] # no dupes!
data <- data[!duplicated(data),] 

## DROP COLUMNS --------------------

data <- data %>% 
  select(!c(uid, domainID, passEndTime, boutEndDate, specimenNumber,
            identificationReferences, samplerType, sampleTypeCollected, voucherSampleID, voucherSampleCode, dnaSampleID,            
            dnaSampleCode, identifiedBy, dataQF, barrierSubReach, publicationDate, release))

## LENGTH --------------------

# checLECO length before count/abun because that requires joining bulk counts 

data %>% 
  filter(fishTotalLength == 0) %>% dplyr::count() # 1 zeros

data %>% 
  filter(is.na(fishTotalLength)) %>% dplyr::count() # 2 NA's

data <- data %>% 
  filter(fishTotalLength != 0) #this drops the NAs 

## LOAD & JOIN BULK DATA  --------------------

bulk_data <- read.csv(file = file.path("data",
                                       "raw_data",
                                       dataset,
                                       data_type,
                                       folder,
                                       "stackedFiles",
                                       "fsh_bulkCount.csv"))
bulk_data <- bulk_data %>% 
  select(!c(uid, domainID,  passEndTime, boutEndDate,actualOrEstimated, 
            identificationQualifier, identificationReferences, identifiedBy, identificationHistoryID, 
            dataQF, barrierSubReach, publicationDate, release))

bulk_data <- bulk_data %>% 
  dplyr::rename(freq = bulkFishCount) # rename "bulkFishCount" to "freq"
data$freq <- 1 # add count column (i.e., each fish = 1)
data <- data %>% 
  merge(bulk_data, by=c("siteID", "passStartTime", "eventID", 
                        "taxonID", "scientificName", 
                        "morphospeciesID", "freq", "remarks", "namedLocation", "passNumber"), all = T) # join bulk data

data <- data[rep(row.names(data), data$freq), 1:18] # each fish now = 1 row
data <- data %>% 
  select(!c(freq)) # dropping freq column

## DATES --------------------

data$date <- substr(data$passStartTime, 1, 10) # extract date from date-time string
data <- data %>% 
  select(!c(passStartTime)) # dropping passStartTime column

unique(substr(data$date, 6, 7)) # months sampled: "04" "10" "11" "03" "12"

## COUNTS/ABUNDANCE --------------------

count_check <- data.frame(data %>% group_by(date, taxonID) %>% 
                            reframe(count=n())) # count for each species for each sampling event 
hist(count_check$count, breaks = 20) 
summary(count_check)

# most species have low abundances (mean = 9.681) and somewhat right skewed (max = 159.000)

# some missing taxonRank -> imputing where needed:

data$taxonRank[data$scientificName == "Cyprinidae spp."] <- "family"
data$taxonRank[data$scientificName == "Notropis baileyi"] <- "species"

data %>% 
  filter(taxonRank == "genus" | taxonRank == "family" | taxonRank == "order" | taxonRank == "phylum" |  taxonRank == "class") %>% dplyr::count() 
# 204 observations not ID'd to species 

data %>% 
  filter(taxonRank == "subspecies") %>% dplyr::count() 
# 2 obs of a subspecies

## JOIN ENVIRONMENTAL & SAMPLING EFFORT DATA --------------------
enviro_data <- read.csv(file = file.path("data",
                                         "raw_data",
                                         dataset,
                                         data_type,
                                         folder,
                                         "stackedFiles",
                                         "fsh_perPass.csv"))
enviro_data <- enviro_data %>% 
  select(!c(uid, domainID, passStartTime, passEndTime, boutEndDate, specificConductance,
            habitatType, subdominantHabitatType, initialFrequency, initialDutyCycle, initialVoltage,
            finalFrequency, finalDutyCycle, finalVoltage, settingsChanged, initialFrequency2, initialDutyCycle2,
            initialVoltage2, finalFrequency2, finalDutyCycle2, finalVoltage2, efTime2, settingsChanged2,
            netIntegrity, netSetTime, netEndTime, netDeploymentTime, netLength, netDepth, targetTaxaPresent, barrierSubReach, dataQF,
            remarks, publicationDate, release))
data <- 
  data %>% 
  merge(enviro_data, by=c("siteID", "eventID", "namedLocation", "passNumber"), all = T) # join enviro data

data <- data %>% 
  filter(!is.na(taxonID)) # drops NAs that were introduced in merge

## SAMPLING EFFORT --------------------

# FOR ALL NEON STREAM SITES: 
# "10 fish sampling reaches or segments are established at each site; 
# with 3 fixed reaches sampled during every sampling bout 
# and a random subset of 3 additional reaches or segments selected for sampling each year."

unique(data$samplerType) # all sampling done by electrofishing for stream sites

hist(data$efTime, breaks = 20) 

timeseries <- function(x){length(unique(x))} 
tapply(substr(data$date, 1, 7), list(substr(data$eventID, 1, 16)), timeseries) # unique samplings

## CHECK x2 --------------------

# rerunning summarytools now all data are appended 
summarytools::view(summarytools::dfSummary(data),
                   file = file.path("data",
                                    "metadata",
                                    paste0(dataset, 
                                           "_datasummary.html")))

# FINALIZE INTERMEDIATE DATA --------------------


#Here, you'll want to rename any columns you already fit to have our required column naming conventions

intermediate.names()

intermediate <- data %>% 
  dplyr::rename(DATE = date,
                SP_CODE = taxonID,
                SCI_NAME = scientificName,
                SIZE = fishTotalLength)

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
# much higher species diversity compared to other NEON sites
