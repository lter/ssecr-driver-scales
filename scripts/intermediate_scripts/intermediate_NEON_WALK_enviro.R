#_________________________________
# NEON - WALK
# Temperature (digital thermistor) of surface water (https://doi.org/10.48443/tp3q-yc71)
# Water quality (https://doi.org/10.48443/03mj-t174)
# SCALES/SSECR                  
# Sierra Perez   
# R version 4.4.2 (2024-10-31)
#_________________________________

# SETUP ---------------------

  rm(list = ls())

## LOAD PACKAGES AND FUNCTIONS --------------------

  #install.packages("librarian")
  
  librarian::shelf(supportR, tidyverse, summarytools, datacleanr, lterdatasampler,
                   cowplot, gt, vegan, neonUtilities)
  
  source(file = file.path("~/scripts",
                          "functions.R"))
  
  source(file = file.path("~/scripts",
                          "viz_ideas.R"))
  
  # create directories for project if they don't already exist
  intermediate.directories()
  
  # set "site name" based on the name of your raw data folder and what the output should look like for naming convention
  # This step is critical!  
  dataset <- "NEON_WALK"
  
# TEMP ---------------------

## LOAD DATA ---------------------
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

  summarytools::view(summarytools::dfSummary(temp),
                     file = file.path("data",
                                      "metadata",
                                      paste0(dataset, 
                                             "_datasummary.html")))
  
  # SOME NOTES ON DP1.20054.001
  # horizontalPosition: 101 (upstream), 102 (downstream)
  # sWatTempFinalQF: 0 (pass), 1 (fail)
  
## DATES --------------------
  
  temp$year <- substr(temp$startDateTime, 1, 4) # extract year from date-time string
  temp$date <- substr(temp$startDateTime, 1, 10) # extract date from date-time string
  
## QC --------------------    
  
  temp <- temp %>% 
    filter(!sWatTempFinalQF == 1) # drop observations that are flagged as potentially inaccurate
  
## DROP COLUMNS --------------------
  
  temp <- temp %>% 
    select(!c(domainID, horizontalPosition, verticalPosition, startDateTime, endDateTime,
              surfacewaterTempVariance, surfacewaterTempNumPts, surfacewaterTempExpUncert,
              surfacewaterTempStdErMean, sWatTempFinalQF, publicationDate, release))

## CALCULATE TEMP VARIABLES --------------------
  
  # VARIABLES
  # A) annual average temperature (rational: growing season avg would require identifying growing season at each site)
  # B) annual mean daily max temperature
  # C) annual mean daily min temperature
  
  daily_temps <- 
    temp %>% group_by(siteID, year, date) %>% reframe(mean_daily_temp = mean(surfacewaterTempMean, na.rm = T),
                                      mean_max_temp = mean(surfacewaterTempMinimum, na.rm = T),
                                      mean_min_temp = mean(surfacewaterTempMaximum, na.rm = T)) # get daily mean, max, min temps
  
  annual_temps <- 
    daily_temps %>% group_by(siteID, year) %>% reframe(mean_daily_temp = mean(mean_daily_temp, na.rm = T),
                                                mean_max_temp = mean(mean_max_temp, na.rm = T),
                                                mean_min_temp = mean(mean_min_temp, na.rm = T)) # get variables A, B, C
  
# DISSOLVED OXYGEN ---------------------
  
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
  
  summarytools::view(summarytools::dfSummary(DO),
                     file = file.path("data",
                                      "metadata",
                                      paste0(dataset, 
                                             "_datasummary.html")))
  
  # SOME NOTES ON DP1.20288.001
  # horizontalPosition: 101 (upstream), 102 (downstream)
  # dissolvedOxygenFinalQF: 0 (pass), 1 (fail)
  
## DATES --------------------
  
  DO$year <- substr(DO$startDateTime, 1, 4) # extract year from date-time string
  DO$date <- substr(DO$startDateTime, 1, 10) # extract date from date-time string
  
## QC --------------------    
  
  DO <- DO %>% 
    filter(!dissolvedOxygenFinalQF == 1) # drop observations that are flagged as potentially inaccurate
  
## DROP COLUMNS --------------------
  
  DO <- DO %>% 
    select(c(siteID, dissolvedOxygen, year, date))
  
## CALCULATE DO VARIABLES --------------------
  
  # VARIABLES
  # D) annual average DO
  # E) annual mean daily DO
  # F) annual mean min DO

  daily_DO <- 
    DO %>% group_by(siteID, year, date) %>% reframe(mean_daily_DO = mean(dissolvedOxygen, na.rm = T),
                                                      mean_min_DO = min(dissolvedOxygen, na.rm = T)) # get daily mean & min
  
  annual_DO <- 
    daily_DO %>% group_by(siteID, year) %>% reframe(mean_daily_DO = mean(mean_daily_DO, na.rm = T),
                                                       mean_min_DO = mean(mean_min_DO, na.rm = T)) # variables E & F
  annual_DO$annual_avg_DO <- 
    DO %>% group_by(siteID, year) %>% reframe(annual_avg_DO = mean(dissolvedOxygen, na.rm = T)) # variable D

# FINALIZE DATA --------------------
  
# merge temp & DO
  
# import intermediate fish data
  
# merge fish & enviro data
  
# export
  

