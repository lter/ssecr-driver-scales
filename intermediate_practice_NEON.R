#_________________________________
#NEON - HOPB - Fish/Temp/DO
# SCALES/ SSECR                  
# Allie Case   
# R Version: 4.4.2 (2024-10-31) -- "Pile of Leaves"
#macOS version: 12.6.3
#RStudio version: 2024.12.0+467

#_________________________________

# setup ---------------------

rm(list = ls())

#create directory for LTER summary output and basic viz
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("data", "raw_data"), showWarnings = F)
dir.create(path = file.path("data", "metadata"), showWarnings = F)

#set "dataset" based on the name of your raw data folder and what the output should look like for naming convention, This step is critical!  

#dataset <- "NEON_HOPB"

## load packages and function --------------------

#install.packages("librarian")

librarian::shelf(supportR, tidyverse, summarytools, 
                 datacleanr, lterdatasampler,
                 cowplot, gt, vegan,
                 neonUtilities, neonOS, terra, googledrive)

# authenticate Google Drive ------

# Tell Google Drive your R session is allowed to acccess Drive stuff
## Open this tutorial and work through it before running the following code
## https://lter.github.io/scicomp/tutorial_googledrive-pkg.html
googledrive::drive_auth()

# Identify the desired file(s)
neon_hopb_zip <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1hWuEzA-QJgNFuSZXZjSL6jtaUjBbBHNZ")) %>% 
  dplyr::filter(name == "NEON_count-fish.zip")

# Check that worked (should be one row / file)
neon_hopb_zip

# Download it
purrr::walk2(.x = neon_hopb_zip$id, .y = neon_hopb_zip$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "raw_data", .y)))

# source(file = file.path("scripts",
#                         "cleaning_function.R"))

## load data ---------------------

#ONLY RUN ONCE 

# The stackByTable() (or stack_by_table()) function will unzip and join the files in the downloaded zip file. Do NOT unzip in Google drive! Download the zip file directly. 

stackByTable(file = file.path("data",
                              "raw_data",
                              "NEON_count-fish.zip"))

#these are the files that end up in the "stackedFiles" folder. This is what I put on Google Drive. 

#Here is the list of files that appear to work (in NEON_count-fish and then stackedFiles):

#fsh_fieldData.csv
#fsh_perFish.csv
#fsh_perPass.csv
#variables_20107.csv
#fsh_bulkCount.csv
#categoricalCodes_20107.csv
#validation_20107.csv

#And here is the error I get after these run: 

# Error in dyn.load(file, DLLpath = DLLpath, ...) : 
#   unable to load shared object '/Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/curl/libs/curl.so':
#   dlopen(/Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/curl/libs/curl.so, 0x0006): symbol not found in flat namespace (_curl_url_strerror)









