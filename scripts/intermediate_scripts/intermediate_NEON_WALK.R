#_________________________________
# NEON - WALK - Fish electrofishing, gill netting, and fyke netting counts (https://doi.org/10.48443/kb2e-va82)
# SCALES/ SSECR                  
# Sierra Perez   
# R version 4.3.2 (2023-10-31)
#_________________________________

# SETUP ---------------------

rm(list = ls())

## LOAD PACKAGES AND FUNCTIONS --------------------

#install.packages("librarian")

librarian::shelf(supportR, tidyverse, summarytools, 
                 datacleanr, lterdatasampler,
                 cowplot, gt,
                 vegan)

source(file = file.path("scripts",
                        "functions.R"))

source(file = file.path("scripts",
                        "viz_ideas.R"))


#create directories for project if they don't already exist

intermediate.directories()

  
  # set "site name" based on the name of your raw data folder and what the output should look like for naming convention, This step is critical!  
  
  dataset <- "NEON_WALK"

## LOAD ---------------------
  
#make sure to double check above you've set the "dataset" object to the name of the subfolder where this data is going to live! 
  
  neon_download(site = "WALK", 
                      dpID = "DP1.20107.001", 
                      dataset)
  
#add file name here of the downloaded zip folder

  #here, add the name of the folder to find everything in for the NEON stacked data! 
  
  folder <- "filesToStack20107"
  
  neon_stack(folder)
  
  #you will need to change this for your own data 
  
  data <- read.csv(file = file.path("data",
                                    "raw_data",
                                    dataset,
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
    select(!c(uid, domainID, namedLocation, passEndTime, passNumber, specimenNumber,
              sampleTypeCollected, voucherSampleID, dnaSampleID, identifiedBy, publicationDate, release))

## LENGTH --------------------
  
  # checking length before count/abun because that requires joining bulk counts 
  
  data %>% 
    filter(fishTotalLength == 0) %>% dplyr::count() # no 0's!
  
  data %>% 
    filter(is.na(fishTotalLength)) %>% dplyr::count() # 5 NA's
  
  data <- data %>% 
    filter(fishTotalLength != 0) #this drops the NAs 
  


## LOAD & JOIN BULK DATA  --------------------
  
  bulk_data <- read.csv(file = file.path("data",
                                         "raw_data",
                                         dataset,
                                        folder,
                                         "stackedFiles",
                                         "fsh_bulkCount.csv"))
  bulk_data <- bulk_data %>% 
    select(!c(uid, domainID, namedLocation, passEndTime, passNumber, actualOrEstimated, 
              identificationQualifier, identifiedBy, publicationDate, release))
  
  colnames(bulk_data)[7]  <- "freq" # rename "bulkFishCount" to "n"
  data$freq <- 1 # add count column (i.e., each fish = 1)
  data <- data %>% 
    merge(bulk_data, by=c("siteID", "passStartTime", "eventID", 
                          "taxonID", "scientificName", 
                          "morphospeciesID", "freq", "remarks"), all = T) # join bulk data
  
  data <- data[rep(row.names(data), data$freq), 1:17] # each fish now = 1 row
  data <- data %>% 
    select(!c(freq)) # dropping freq column

## DATES --------------------
  
  data$date <- substr(data$passStartTime, 1, 10) # extract date from date-time string
  data <- data %>% 
    select(!c(passStartTime)) # dropping passStartTime column
  
  unique(substr(data$date, 6, 7)) # months sampled: Mar, Oct, Nov
  
  # ******* do we want to use both spring and fall samplings? ******

## COUNTS/ABUNDANCE --------------------

  count_check <- data.frame(data %>% group_by(date, taxonID) %>% 
                              reframe(count=n())) # count for each species for each sampling event 
  hist(count_check$count, breaks = 20) 
  summary(count_check)
  
  # most species have low abundances (mean = 80) -- less right skewed than some other sites (max = 324)
  # RHIATR seems to be dominant species (this is the same as for POSE!)

  # some missing taxonRank -> imputing where needed:
  
  data$taxonRank[data$taxonID == "RHIATR"] <- "species"

  data %>% 
    filter(taxonRank == "genus" | taxonRank == "family" | taxonRank == "order") %>% dplyr::count() 
  # ***** 4 observations only ID'd to family, genus, or order *****
  
  data %>% 
    filter(taxonRank == "subspecies") %>% dplyr::count() 
  # 0 obs of a subspecies

## JOIN ENVIRONMENTAL & SAMPLING EFFORT DATA --------------------

  enviro_data <- read.csv(file = file.path("data",
                                           "raw_data",
                                           dataset,
                                           folder,
                                           "stackedFiles",
                                           "fsh_perPass.csv"))
  enviro_data <- enviro_data %>% 
    select(!c(uid, domainID, namedLocation, passStartTime, passEndTime, passNumber, reachID, specificConductance,
              habitatType, subdominantHabitatType, initialFrequency, initialDutyCycle, initialVoltage,
              finalFrequency, finalDutyCycle, finalVoltage, settingsChanged, initialFrequency2, initialDutyCycle2,
              initialVoltage2, finalFrequency2, finalDutyCycle2, finalVoltage2, efTime2, settingsChanged2,
              netIntegrity, netSetTime, netEndTime, netDeploymentTime, netLength, netDepth, targetTaxaPresent,
              remarks, publicationDate, release))
  
  data <- data %>% 
    merge(enviro_data, by=c("siteID", "eventID"), all = T) # join enviro data

## SITES & SPATIAL INFO --------------------

  # there are "points" and "passes" per point
  # "10 fish sampling reaches or segments are established at each site [I think these are the "points"]; 
  # with 3 fixed reaches sampled during every sampling bout 
  # and a random subset of 3 additional reaches or segments selected for sampling each year."
  
  unique(substr(data$eventID, 15, 16)) # there are 10 points sampled -- points 07, 09, 02 are fixed (ie sampled every time)
  unique(substr(data$eventID, 18, 18)) # there are 3 passes

## SAMPLING EFFORT --------------------
  
  unique(data$samplerType) # "electrofisher"
  
  hist(data$efTime, breaks = 20) 
  
  tapply(substr(data$date, 1, 7), list(substr(data$eventID, 15, 16)), timeseries) # unique samplings per point

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
  # **** 7 species only appear in 1 year *****
  
  plot.speciesaccum(count_check,
                    species_col = "SP_CODE",
                    subsite = F)



