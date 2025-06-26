#_________________________________
#IEP LODI - Annual fish surveys  
# SCALES/ SSECR                  
# Jeremy Collings  
# R Version: 4.4.1 (2024-06-14) -- "Race for Your Life"
#_________________________________

# setup ---------------------

rm(list = ls())

#create directory for LTER summary output and basic graphs

ifelse(!dir.exists(file.path("data", "metadata")), 
       dir.create(file.path("data", "metadata")), 
       FALSE)

#set "site name" based on the name of your raw data folder and what the output should look like for naming convention, This step is critical!  

dataset <- "IEP_LODI"

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

#you will need to change this for your own data 

data1 <- read.csv(file = file.path("data",
                                  "raw_data",
                                  "IEP_LODI",
                                  "2018-2020_USFWS_EFISH_data.csv"))
data1$TagCode <- as.character(data1$TagCode) # gonna drop this anyway but it's giving trouble for merging

data2 <- read.csv(file = file.path("data",
                                   "raw_data",
                                   "IEP_LODI",
                                   "2021-2024_USFWS_EFISH_data.csv"))

data <- bind_rows(data1, data2)

# checks --------------------

#get generic output on data structures and unique values for each - this you can keep! It's set up so that it will name it based on your unique site value you named earlier. 

summarytools::view(summarytools::dfSummary(data),
                   file = file.path("data",
                                    "metadata",
                                    paste0(dataset, 
                                           "_datasummary.html")))



#here is where I also throw in questions about things I don't know AND also don't fit an obvious "block" of work like dates, species, spatial. What are the different survey methods? I made a note in the metadata that there are technically two different ones.

## duplicates -----

data[duplicated(data),] # just a bunch of entries with NAs for OrganismID's... all from the same EventID
data <- data[!duplicated(data),]

## counts/abundance -----

data <- data %>% 
  filter(!is.na(Count)) %>%
  uncount(Count,
          .remove = T)

#This is the most important part of data to have and understand so it makes sense to start with it here. 

#-99999 for COUNT means the value was not recorded or not available. 0 is defined in the metadata as "The number of individuals of the same size counted", but in reality these counts were likely done with checklists and it means that these fish weren't counted, so they should be dropped. The final structure of our data should be EACH row is one observation. 

# data %>% 
#   filter(COUNT == "-99999" |
#            COUNT == 0) %>% 
#   dplyr::count()

# JC: I don't think this applies to the IEP datasets

#this helps to show that our data looks pretty inflated row-wise, when in actuality we don't have that many observations. Almost 90% of the rows are 0 or missing. 

# data <- data %>% 
#   filter(COUNT != "-99999" &
#            COUNT != 0)

#final step for counts is that we want ONE ROW to equal ONE FISH. We can do this relatively easily using some nice dplyr tools. 

# data <- data %>% 
#   uncount(COUNT,
#           .remove = T) #the default is to remove the count column after it "expands" since the count won't necessarily represent anything anymore

## dates ------

data <- data %>%
  mutate(YEAR = year(SampleDate))


## sites and spatial info --------

data <- data %>%
  mutate(SUBSITE = events$StationCode[match(data$EventID, events$EventID)])

##sample effort ------------------------------------

effort <- read.csv(file = file.path("data",
                                    "raw_data",
                                    "IEP_YOLO",
                                    "effort1.csv"))

data <- data %>%
  mutate(GEAR = events$GearCode[match(data$EventID, events$EventID)])

data <- data %>% left_join(effort, "EventID")

#across years lumped (no transects)

#this is back to base R, but the output is really appealing 

timeseries <- function(x){length(unique(x))} #function can can count unique years for each station

tapply(data$YEAR, list(data$SUBSITE), timeseries)

#we can look at the same thing visually 

data %>% 
  distinct(YEAR, SUBSITE) %>% 
  ggplot(aes(x = as.factor(YEAR),
             y = SUBSITE,
             color = as.factor(SUBSITE))) +
  geom_point(show.legend = F) +
  labs(x = "",
       y = "Site")

## taxonomy ------------------------------------

taxa <- read.csv("data/raw_data/IEP_YOLO/taxonomy1.csv")

data <- data %>%
  mutate(SCI_NAME = taxa$Taxa[match(data$OrganismCode, taxa$OrganismCode)], 
         COMMON_NAME = taxa$CommonName[match(data$OrganismCode, taxa$OrganismCode)])

# Environmental Data -----

envir <- events %>%
  mutate(year = year(mdy(events$SampleDate))) %>%
  group_by(year, StationCode) %>%
  summarise(mean_temp = mean(WaterTemp, na.rm = TRUE), 
            mean_DO = mean(DO, na.rm = TRUE))

names(envir) <- c("YEAR", "SUBSITE", "mean_temp", "mean_DO")

data <- merge(data, envir, by = c("YEAR", "SUBSITE"))
#finalize intermediate data -----

# drop any other irrelevant columns 

#I save this for LAST in case there is anything I want to look at or investigate in other columns along the way. 

#I'll drop any obvious columns by looking at the data summary output. Here are the things we know that we need: date, species, counts/sizes, survey methods, and any sort of spatial information for now (transect, quadrat, etc). 

data <- data %>% 
  mutate(SITE = rep("YOLO", n()), 
         SIZE = ForkLength, 
         EFFORT = case_when(
           is.na(TrapHours) ~ SeineVolume, 
           !is.na(TrapHours) ~ TrapHours
         ), 
         GEAR = case_when(
           is.na(TrapHours) ~ "seine", 
           !is.na(TrapHours) ~ "trap"
         )) %>%
  select(!c(OrganismID, FishGenID, OrganismCode, ForkLength, TotalLength, 
            Weight, Sex, RaceByLength, StageCode, Dead, Expression, 
            GeneticSample, FishIDComments, SeineLength, SeineWidth, 
            SeineDepth, SeineVolume, TrapStatus, TrapHours))

#Note I threw YEAR and MONTH because we already had a DATE column with this info. We can always mutate these columns later after the data is harmonized. 

#Here, you'll want to rename any columns you already fit to have our required column naming conventions

intermediate.names()

names(data) <- c("YEAR", "SUBSITE", "EVENT_ID", "DATE", "GEAR",
                 "SCI_NAME", "COMMON_NAME", "mean_daily_temp", 
                 "mean_daily_DO", "SITE", "SIZE", "EFFORT")

#this custom function should do the rest 

intermediate.prep(data)

#OPTIONAL VIZ: Play around with subsites by changing to True! The default is false. 

plot.top5(intermediate, species_col = "SCI_NAME", subsite = F)

plot.presence(intermediate, 
              species_col = "COMMON_NAME",
              subsite = F)

plot.speciesaccum(intermediate,
                  species_col = "SP_CODE",
                  subsite = F)

