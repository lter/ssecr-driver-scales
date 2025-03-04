#_________________________________
# NEON - WALK - Fish electrofishing, gill netting, and fyke netting counts (https://doi.org/10.48443/ap3d-rp07)
#Temperature (digital thermistor) of surface water (https://doi.org/10.48443/tp3q-yc71)
# Water quality (https://doi.org/10.48443/03mj-t174)
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
  
  dataset <- "NEON_WALK"

## LOAD ---------------------
  
#make sure to double check above you've set the "dataset" object to the name of the subfolder where this data is going to live! 
  
  neon_download(site = "WALK", 
                      dpID = "DP1.20107.001", 
                      dataset,
                data_type = "fish")
  
#add file name here of the downloaded zip folder

  #here, add the name of the folder to find everything in for the NEON stacked data!
  
  folder <- "filesToStack20107"
  
  #select either "fish" or "enviro"

  data_type <- "fish"
  
  neon_stack(folder = folder,
             data_type = "fish",
             dataset = dataset)
  
  #you will need to change this for your own data 
  
  data <- read.csv(file = file.path("data",
                                    "raw_data",
                                    dataset,
                                    "fish",
                                    folder,
                                    "stackedFiles",
                                    "fsh_perFish.csv"))
#PART #1: FISH ------
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
  
  # checking length before count/abun because that requires joining bulk counts 
  
  data %>% 
    filter(fishTotalLength == 0) %>% dplyr::count() # no 0's!
  
  data %>% 
    filter(is.na(fishTotalLength)) %>% dplyr::count() # 3 NA's
  
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
  
  unique(substr(data$date, 6, 7)) # months sampled: Mar, Oct, Nov
  
  # ******* do we want to use both spring and fall samplings? ******

## COUNTS/ABUNDANCE --------------------

  count_check <- data.frame(data %>% group_by(date, taxonID) %>% 
                              reframe(count=n())) # count for each species for each sampling event 
  hist(count_check$count, breaks = 20) 
  summary(count_check)
  
  # most species have low abundances (mean = 83.6) -- less right skewed than some other sites (max = 351.0)
  # RHIATR seems to be dominant species (this is the same as for POSE!)

  # some missing taxonRank -> imputing where needed:
  
  data$taxonRank[data$taxonID == "RHIATR"] <- "species"

  data %>% 
    filter(taxonRank == "genus" | taxonRank == "family" | taxonRank == "order" | taxonRank == "phylum" |  taxonRank == "class") %>% dplyr::count() 
  # 4 observations not ID'd to species 
  
  data %>% 
    filter(taxonRank == "subspecies") %>% dplyr::count() 
  # 0 obs of a subspecies

# ## JOIN ENVIRONMENTAL & SAMPLING EFFORT DATA (old) 
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
  
 # hist(data$efTime, breaks = 20) 
  
  timeseries <- function(x){length(unique(x))} 
  tapply(substr(data$date, 1, 7), list(substr(data$eventID, 1, 16)), timeseries) # unique samplings
  
## CHECK x2 --------------------
  
  # rerunning summarytools now all data are appended 
  summarytools::view(summarytools::dfSummary(data),
                     file = file.path("data",
                                      "metadata",
                                      paste0(dataset, 
                                             "_datasummary.html")))


  #at the end of the fish section, we should have data that is DATE, SITE/SUBSITE, SP_CODE, SIZE, SCIENTIFIC_NAME, COMMON_NAME (not for NEON stuff), YEAR, EFFORT
  
  fish <- data %>% 
    dplyr::rename(DATE = date,
                  SP_CODE = taxonID,
                  SCI_NAME = scientificName,
                  SIZE = fishTotalLength,
                  SUBSITE = siteID,
                  EFFORT = efTime) %>% 
    mutate(YEAR = year(DATE)) %>% 
    select(DATE, SUBSITE, SP_CODE, SIZE, SCI_NAME, YEAR, EFFORT)
  
  #PART #2: TEMP ------
  
  # make sure to double check above you've set the "dataset" object to the name of the subfolder where this data is going to live! 
  # "data_type" is the name for the subfolder for this data in site raw data folder
  
  neon_download(site = "WALK", 
                dpID = "DP1.20054.001", 
                dataset,
                data_type = "temp")
  
  # add the name of the folder to find everything in for the NEON stacked data! 
  folder <- "filesToStack20054"
  
  neon_stack(folder = "filesToStack20054", dataset, data_type = "temp")
  
  # this will be the same path for all NEON sites
  temp <- read.csv(file = file.path("data",
                                    "raw_data",
                                    dataset,
                                    "temp",
                                    folder,
                                    "stackedFiles",
                                    "TOSW_30_min.csv")) 
  # used the 30 min resolution (instead of the 5 min) because the file is 6x smaller 
  ## CHECKS --------------------
  
  # get generic output on data structures and unique values for each - this you can keep! 
  #these take a while to make so only use if necessary
  # summarytools::view(summarytools::dfSummary(temp),
  #                    file = file.path("data",
  #                                     "metadata",
  #                                     paste0(dataset, 
  #                                            "_datasummary.html")))
  
  # SOME NOTES ON DP1.20054.001
  # horizontalPosition: 101 (upstream), 102 (downstream)
  # sWatTempFinalQF: 0 (pass), 1 (fail)
  
  ## DATES --------------------
  
  #change date format to match LTER and overall actual date format
  temp <- temp %>% 
    mutate(DATE = ymd_hms(startDateTime),
              DATE = as.Date(DATE),
              YEAR = year(DATE))
  
  ## QC --------------------    
  
  temp <- temp %>% 
    filter(!sWatTempFinalQF == 1) # drop observations that are flagged as potentially inaccurate
  
  ## DROP COLUMNS --------------------
  
  temp <- temp %>% 
    select(siteID, YEAR, DATE, contains(c("Minimum", "Maximum", "Mean"))) %>%
    select(-surfacewaterTempStdErMean)
  
  ## CALCULATE TEMP VARIABLES --------------------
  
  # VARIABLES
  # A) annual average temperature (rational: growing season avg would require identifying growing season at each site)
  # B) annual mean daily max temperature
  # C) annual mean daily min temperature
  
  temp_final <- temp %>% 
    group_by(siteID, YEAR, DATE) %>% 
    reframe(mean_daily_temp = mean(surfacewaterTempMean, na.rm = T),
            mean_max_temp = mean(surfacewaterTempMaximum, na.rm = T),
            mean_min_temp = mean(surfacewaterTempMinimum, na.rm = T)) %>% 
    ungroup() %>% 
    group_by(siteID, YEAR) %>% 
    reframe(mean_daily_temp = mean(mean_daily_temp, na.rm = T),
            mean_max_temp = mean(mean_max_temp, na.rm = T),
            mean_min_temp = mean(mean_min_temp,na.rm = T)) %>% 
    rename(SUBSITE = siteID) # get variables A, B, C

  temp_final$YEAR <- temp_final$YEAR + 1 # offset year before joining to fish data
  
  #PART #3: DO ------
  
  ## LOAD DATA ---------------------
  
  #make sure to double check above you've set the "dataset" object to the name of the subfolder where this data is going to live! 
  
  neon_download(site = "WALK", 
                dpID = "DP1.20288.001", 
                dataset,
                data_type = "DO")
  
  #add file name here of the downloaded zip folder
  
  #here, add the name of the folder to find everything in for the NEON stacked data! 
  
  folder <- "filesToStack20288"
  
  neon_stack(folder = "filesToStack20288", dataset, data_type = "DO")
  
  #you will need to change this for your own data 
  DO <- read.csv(file = file.path("data",
                                  "raw_data",
                                  dataset,
                                  "DO",
                                  folder,
                                  "stackedFiles",
                                  "waq_instantaneous.csv")) 
  
  ## CHECKS --------------------
  
  #get generic output on data structures and unique values for each - this you can keep! 
  # It's set up so that it will name it based on your unique site value you named earlier. 
  #only run if necessary (takes awhile)
  # summarytools::view(summarytools::dfSummary(DO),
  #                    file = file.path("data",
  #                                     "metadata",
  #                                     paste0(dataset, 
  #                                            "_datasummary.html")))
  
  # SOME NOTES ON DP1.20288.001
  # horizontalPosition: 101 (upstream), 102 (downstream)
  # dissolvedOxygenFinalQF: 0 (pass), 1 (fail)
  
  ##DATES -------
  
  DO <- DO %>%
    mutate(DATE = ymd_hms(startDateTime),
      DATE = as.Date(DATE),
      YEAR = year(DATE))
  
  ## QC --------------------    
  
  DO <- DO %>% 
    filter(!dissolvedOxygenFinalQF == 1) # drop observations that are flagged as potentially inaccurate
  
  ## DROP COLUMNS --------------------
  
  DO <- DO %>% 
    select(c(siteID, dissolvedOxygen, YEAR, DATE))
  
  ## CALCULATE DO VARIABLES --------------------
  
  # VARIABLES
  # D) annual average DO (might scratch?)
  # E) annual mean daily DO
  # F) annual mean min DO
  
  daily_DO <- DO %>% 
    group_by(siteID, YEAR, DATE) %>% 
    reframe(mean_daily_DO = mean(dissolvedOxygen, na.rm = T),
            mean_min_DO = min(dissolvedOxygen, na.rm = T)) # get daily mean &
  
    daily_DO <- daily_DO %>% 
      group_by(siteID, YEAR) %>% 
      reframe(mean_daily_DO = mean(mean_daily_DO, na.rm = T),
              mean_min_DO = mean(mean_min_DO, na.rm = T)) %>%  # variables E & F 
      rename(SUBSITE = siteID)
  
    annual_DO <- DO %>% 
      group_by(siteID, YEAR) %>%
      reframe(annual_avg_DO = mean(dissolvedOxygen, na.rm = T)) %>% 
      rename(SUBSITE = siteID) #variable D
  
#finalize DO 
    
 DO_final <- left_join(daily_DO, annual_DO)
  
 DO_final$YEAR <- DO_final$YEAR +1 # offset year before joining to fish data

#finalize environmental data 

 enviro_final <- left_join(DO_final, temp_final)

#PART #4: HARMONIZE TEMP & DO with FISH
 
 intermediate <- left_join(fish, enviro_final, by = c("SUBSITE", "YEAR"))
  
# PART 5: FINALIZE INTERMEDIATE DATA --------------------
 
  #Here, you'll want to rename any columns you already fit to have our required column naming conventions
  
  intermediate.names()
  
  intermediate.prep(intermediate)

# (OPTIONAL) DATA VIZ --------------------
  #Play around with subsites by changing to True! The default is false. 
  
  plot.top5(intermediate, species_col = "SCI_NAME", subsite = F)

  plot.presence(intermediate, 
                species_col = "SCI_NAME",
                subsite = F)
  # **** 5 species only appear in 1 year *****
  
  plot.speciesaccum(count_check,
                    species_col = "SP_CODE",
                    subsite = F)
