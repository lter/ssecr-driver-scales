#_________________________________
# Keretz, K.R., Kraus, R.T., Schmitt, J.D., and Dufour, M.R., 2025, Lake Erie Fish Community Data, 2013-2024: U.S. Geological Survey data release
# Source:  https://doi.org/10.5066/P1YPU5VN.
# SCALES/ SSECR                  
# Bethany Williams 
# R version R version 4.5.1 (2025-06-13 ucrt)
#_________________________________

# SETUP ---------------

rm(list = ls())

## LOAD PACKAGES AND FUNCTIONS --------------

#install.packages("librarian")

librarian::shelf(supportR, tidyverse, summarytools, datacleanr, lterdatasampler,
                 cowplot, gt, vegan, neonUtilities, splitstackshape,readxl)

source(file = file.path("scripts",
                        "functions.R"))

source(file = file.path("scripts",
                        "viz_ideas.R"))


#create directories for project if they don't already exist

intermediate.directories()


# set "site name" based on the name of your raw data folder and what the output should look like for naming convention, This step is critical!  

dataset <- "USGS_ERIE"

# PART #1: FISH  ---------------------

## LOAD FISH DATA ---------------------

# make sure to double check above you've set the "dataset" object to the name of the subfolder where this data is going to live! 

# loading fish dataset (don't use expanded lengths as its bootstrap data)

data<-read_csv("data/raw_data/USGS_ERIE/WB_LengthWeight_2013_2024.csv")


data<-data%>% rename(COMMON_NAME=species)
length(unique(data$COMMON_NAME))

#load in species list to get scientific names

species<-read_csv("data/raw_data/USGS_ERIE/WB_Species.csv")
species<-species%>% rename(COMMON_NAME=common_name)

data<-merge(data,species,by="COMMON_NAME")

#remove stocked species
#info from great lakes fishery commission 
#https://fsis.glfc.org/stocking/events_list/?lake=ER
#stocked but not in our dataset
#brook trout
#brown trout
#chinook salmon
#coho salmon
#lake herring (cisco)

#stocked and in our dataset
#lake sturgeon
#lake trout
#rainbow trout
#muskellenge
#walleye
#yellow perch
filter<-c("Lake Sturgeon", "Lake Trout","Rainbow Trout", "Muskellunge","Walleye","Yellow Perch")
data<-data%>% filter(!COMMON_NAME %in%filter)


# CHECKS --------------------

#get generic output on data structures and unique values for each - this you can keep! 
# It's set up so that it will name it based on your unique site value you named earlier. 

summarytools::view(summarytools::dfSummary(data),
                   file = file.path("data",
                                    "metadata",
                                    paste0(dataset, 
                                           "_datasummary.html")))

## SAMPLING EFFORT --------------------

fish_count<-read_csv("data/raw_data/USGS_ERIE/WB_Catch_2013_2024.csv")
fish_count<-fish_count%>% rename(COMMON_NAME=species)
#only keep all, not subsetted by age and remove species that weren't caught
fishcount2<-subset(fish_count,life_stage=="ALL"&count>0)
#also remove the stocked fish
fishcount2<-fishcount2%>% filter(!COMMON_NAME %in%filter)
fishcount2<-fishcount2%>%select(serial,area_m2,year)
fishcount2<-unique(fishcount2)

#add in effort from count data file

data2<-merge(data,fishcount2,by=c("serial","year"))
## FINALIZE FISH ---------------


# at the end of the fish section, we should have:
# DATE, SITE/SUBSITE, SP_CODE, SIZE, SCIENTIFIC_NAME, COMMON_NAME (not for NEON stuff), YEAR, EFFORT
data2<-data2%>% mutate(DATE=make_date(year,month,day))
fish <- data2 %>% 
  dplyr::rename(SP_CODE = taxonomic_serial_number,
                SCI_NAME = scientific_name,
                SIZE = tl_mm,
                EFFORT = area_m2) %>% 
  mutate(YEAR = year(DATE)) %>% 
  select(DATE, SP_CODE, SIZE, SCI_NAME, YEAR, EFFORT)


# PART #2: TEMP ------

## CALCULATE TEMP VARIABLES --------------------
sampling<-read_csv("data/raw_data/USGS_ERIE/WB_WaterQuality_2013_2024.csv")

sampling<- sampling%>%mutate(SampleDate=(make_date(year,month,day))) # convert to date

# VARIABLES
# A) annual average temperature (rational: growing season avg would require identifying growing season at each site)
# B) annual mean daily max temperature
# C) annual mean daily min temperature

temp_final <- sampling %>% 
  group_by(year, SampleDate) %>% 
  reframe(mean_daily_temp = mean(temp_mean, na.rm = T),
          mean_max_temp = max(temp_mean, na.rm = T),
          mean_min_temp = min(temp_mean, na.rm = T))  %>% 
  filter(!mean_daily_temp == "NaN") %>%
  ungroup() %>%
  group_by(year) %>%
  reframe(mean_daily_temp = mean(mean_daily_temp, na.rm = T),
          mean_max_temp = mean(mean_max_temp, na.rm = T),
          mean_min_temp = mean(mean_min_temp,na.rm = T)) # get variables A, B, C

ggplot(sampling, aes(x=SampleDate, y = temp_mean,color=season)) +
  geom_point()


temp_final$year <- as.numeric(temp_final$year)
temp_final$year <- temp_final$year + 1 # offset year before joining to fish data


#PART #3: DO ------

## CALCULATE DO VARIABLES --------------------

# VARIABLES
# D) annual average DO (might scratch?)
# E) annual mean daily DO
# F) annual mean min DO

daily_DO <- sampling %>% 
  group_by(year, SampleDate) %>% 
  reframe(mean_daily_DO = mean(do_ppm_mean, na.rm = T),
          mean_min_DO = min(do_ppm_mean, na.rm = T)) # get daily mean & min

daily_DO <- daily_DO %>% 
  filter(!mean_daily_DO == "NaN") %>%
  filter(!mean_min_DO == "Inf") %>%
  group_by(year) %>% 
  reframe(mean_daily_DO = mean(mean_daily_DO, na.rm = T),
          mean_min_DO = mean(mean_min_DO, na.rm = T))  # variables E & F 

annual_DO <- sampling %>% 
  group_by(year) %>%
  reframe(annual_avg_DO = mean(do_ppm_mean, na.rm = T))

DO_final <- merge(daily_DO, annual_DO, by = "year", all = T)

ggplot(sampling, aes(x=SampleDate, y = do_ppm_mean)) +
  geom_point() 

DO_final <- DO_final %>% 
  filter(!year==2016) # drop years with missing data

DO_final$year<- as.numeric(DO_final$year)
DO_final$year <- DO_final$year + 1 # offset year before joining to fish data

#finalize environmental data 

enviro_final <- temp_final %>%
  merge(DO_final, by=c("year"), all = T) # use merge not join--join drops years if temp or DO missing for year
enviro_final<-enviro_final%>%rename(YEAR=year)
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
