#_________________________________
# Harmonizing all intermediate scripts
# SCALES/ SSECR                  
# Allie Case 
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

## LOAD DATA ----

#First, download the entire clean intermediate_data folder on Google Drive. For now this is just called "intermediate_data2". 

harmonized <- list.files(path = file.path("data", 
                                           "intermediate_data",
                                           "intermediate_data2"),
                          pattern = "\\.csv$", full.names = TRUE) %>%
  purrr::map_dfr(read_csv)  # Reads and binds all csvs



