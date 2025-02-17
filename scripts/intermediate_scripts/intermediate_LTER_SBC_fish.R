#_________________________________
#LTER SBC - Annual fish surveys  
# SCALES/ SSECR                  
# Allie Case   
# R Version: 4.4.2 (2024-10-31) -- "Pile of Leaves"
#_________________________________

# setup ---------------------

rm(list = ls())

## load packages and functions --------------------

#install.packages("librarian")

librarian::shelf(supportR, tidyverse, summarytools, 
                 datacleanr, lterdatasampler,
                 cowplot, gt,
                 vegan, googledrive)

source(file = file.path("scripts",
                        "functions.R"))

source(file = file.path("scripts",
                        "viz_ideas.R"))


#create LOCAL directories for project if they don't already exist

intermediate.directories()

#set "site name" based on the name of your raw data folder and what the output should look like for naming convention, This step is critical!  

dataset <- "LTER_SBC"


## Downloaded data (AC did 2/17/25. Ignore this step) ---------------------

#only run this code once from EDI, then comment it out!

# Package ID: knb-lter-sbc.17.40 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Reef: Kelp Forest Community Dynamics: Fish abundance.
# Data set creator:  Daniel C Reed -
# Data set creator:  Robert J Miller -
# Contact:    -  Santa Barbara Coastal LTER  - sbclter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu

# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/17/40/a7899f2e57ea29a240be2c00cce7a0d4"
# infile1 <- tempfile()
# try(download.file(inUrl1,infile1,method="curl"))
# if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
#
#
# dt1 <-read.csv(infile1,header=F
#                ,skip=1
#                ,sep=","
#                ,quot='"'
#                , col.names=c(
#                  "YEAR",
#                  "MONTH",
#                  "DATE",
#                  "SITE",
#                  "TRANSECT",
#                  "QUAD",
#                  "SIDE",
#                  "VIS",
#                  "SP_CODE",
#                  "SIZE",
#                  "COUNT",
#                  "AREA",
#                  "SCIENTIFIC_NAME",
#                  "COMMON_NAME",
#                  "TAXON_KINGDOM",
#                  "TAXON_PHYLUM",
#                  "TAXON_CLASS",
#                  "TAXON_ORDER",
#                  "TAXON_FAMILY",
#                  "TAXON_GENUS",
#                  "GROUP",
#                  "SURVEY",
#                  "MOBILITY",
#                  "GROWTH_MORPH"), check.names=TRUE)
#
# unlink(infile1)
# write.csv(dt1,
#           file = file.path("data",
#                            "raw_data",
#                            "LTER_SBC",
#                            "raw_LTER_SBC_fish.csv"),
#           row.names = F)
# rm(dt1)

#upload raw, downloaded data to Google Drive folder. If it already exists in the Google Drive folder, it will overwrite it. So make sure people know if you're going to run this! 

# shared_drive_id <- "0AAQ1XOtdPVI_Uk9PVA"
# 
# drive_auth()
# 
# #get folder id 
# drive_get(c("raw_data", "LTER_SBC"),
#           shared_drive = as_id(shared_drive_id))
# 
# folderid <- "1Na6D9e-N6BvXjiGhqCv3UH1sX0NcUzgl"
# 
# drive_put(media = file.path(
#   "data",
#   "raw_data",
#   "LTER_SBC",
#   "raw_LTER_SBC_fish.csv"),
#   path = as_id(folderid))


#since data is now downloaded you can work starting directly here on the same .csv as everyone else and will never have to re-download

##START HERE after loading functions up top: load data -------

#manually download data from Drive - I cannot get this function to work and we need to move on. Download the data into the matching folder. 

data <- read.csv(file = file.path("data",
                                  "raw_data",
                                  "LTER_SBC",
                                  "raw_LTER_SBC_fish.csv"))

# checks --------------------

#get generic output on data structures and unique values for each - this you can keep! It's set up so that it will name it based on your unique site value you named earlier. 

summarytools::view(summarytools::dfSummary(data),
                   file = file.path("data",
                                    "metadata",
                                    paste0(dataset, 
                                           "_datasummary.html")))



#here is where I also throw in questions about things I don't know AND also don't fit an obvious "block" of work like dates, species, spatial. What are the different survey methods? I made a note in the metadata that there are technically two different ones.

## duplicates -----

#no duplicate rows found (summary output). If there were, definitely remove them here first before expanding any data for counts/abundance.

## counts/abundance -----

#This is the most important part of data to have and understand so it makes sense to start with it here. 

#-99999 for COUNT means the value was not recorded or not available. 0 is defined in the metadata as "The number of individuals of the same size counted", but in reality these counts were likely done with checklists and it means that these fish weren't counted, so they should be dropped. The final structure of our data should be EACH row is one observation. 

data %>% 
  filter(COUNT == "-99999" |
           COUNT == 0) %>% 
  dplyr::count()

#this helps to show that our data looks pretty inflated row-wise, when in actuality we don't have that many observations. Almost 90% of the rows are 0 or missing. 

data <- data %>% 
  filter(COUNT != "-99999" &
           COUNT != 0)
  
#final step for counts is that we want ONE ROW to equal ONE FISH. We can do this relatively easily using some nice dplyr tools. 

data <- data %>% 
  uncount(COUNT,
          .remove = T) #the default is to remove the count column after it "expands" since the count won't necessarily represent anything anymore

## dates ------

#here is where I would create a YMD column if necessary if the date was in any other format. 

#Years : 25 (2000-2024)

#Month: ~98% of data comes from July or August

data <- data %>% 
  mutate(DATE = ymd(DATE))

## sites and spatial info --------

#there is site, transect and quad. Do we need to keep each transect and quad? Is everything getting lumped together? 

#there are 11 unique "sites" (these are just subsites!)

#Transect: unique numbers for each site? Dig into further:

data %>% 
  distinct(SITE,TRANSECT) %>% 
  arrange(SITE, TRANSECT) #so not all sites have all transects. Is there a better map we need to reference or is this important? 

#Quadrat: somewhat same thing as transect. Do we need this detailed spatial information or can we disregard? Used for benthic survey method only. Describes location along transect (from data metadata)

#do we want to include lat and long? 

#final decision for now: drop transect and quadrat because they are basically just subsite "replicates". The only thing we'll need to consider is how/if that affects our sampling effort. 

data <- data %>% 
  select(!c(TRANSECT, QUAD))

##sample effort ------------------------------------

#across years lumped (no transects)

#this is back to base R, but the output is really appealing 

tapply(data$YEAR, list(data$SITE), timeseries)

#we can look at the same thing visually 

data %>% 
  distinct(YEAR, SITE) %>% 
  ggplot(aes(x = as.factor(YEAR),
             y = SITE,
             color = as.factor(SITE))) +
  geom_point(show.legend = F) +
  labs(x = "",
       y = "Site")

#finalize intermediate data / drop columns -----

# drop any other irrelevant columns 

#I save this for LAST in case there is anything I want to look at or investigate in other columns along the way. 

#I'll drop any obvious columns by looking at the data summary output. Here are the things we know that we need: date, species, counts/sizes, survey methods, and any sort of spatial information for now (transect, quadrat, etc). 

data <- data %>% 
  select(!c(GROUP, MOBILITY, 
            GROWTH_MORPH, VIS, SIDE, 
            starts_with("TAXON"),
            YEAR, MONTH, SURVEY))

#Note I threw YEAR and MONTH because we already had a DATE column with this info. We can always mutate these columns later after the data is harmonized. 

#Here, you'll want to rename any columns you already fit to have our required column naming conventions

intermediate.names()

intermediate <- data %>% 
  rename(SUBSITE = SITE,
         SCI_NAME = SCIENTIFIC_NAME)


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



  
  