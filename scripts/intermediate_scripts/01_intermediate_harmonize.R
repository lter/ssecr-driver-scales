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
                 cowplot, gt, corrplot,
                 vegan, neonUtilities, ggcorrplot, emmeans, ggpubr, gridExtra)

source(file = file.path("scripts",
                        "functions.R"))

source(file = file.path("scripts",
                        "viz_ideas.R"))

## LOAD DATA ----

#First, download the entire clean intermediate_data folder on Google Drive. For now this is just called "intermediate_data". 

harmonized <- list.files(path = file.path("data", 
                                           "intermediate_data"),
                          pattern = "\\.csv$", 
                         full.names = TRUE) %>%
  set_names() %>% 
    purrr::map_dfr(read_csv,.id="file_name") %>% # Reads and binds all csvs
  mutate(file_name = basename(file_name))%>%
  rename(SITE = file_name)

harmonized$SITE<-str_remove(harmonized$SITE,'_intermediate.csv')
  
unique(harmonized$SITE)

#latest update to sites is that we are splitting up the MCR and SBC sites for LTER. 

harmonized <- harmonized %>%
  mutate(MIDSITE = case_when(
    str_detect(SUBSITE, "Backreef") ~ "LTER_MCR_Backreef",
    str_detect(SUBSITE, "Fringing Reef") ~ "LTER_MCR_FringingReef",
    str_detect(SUBSITE, "Forereef") ~ "LTER_MCR_Forereef",
    TRUE ~ SITE)) %>% 
  relocate(MIDSITE, .after = SITE)

unique(harmonized$MIDSITE)

#Bring in taxon list (first from NEON then can append LTER in later)

taxon <- read.csv(file = file.path("data",
                                   "NEON_fish_taxonomy.csv"))

taxon <-  taxon %>% 
  select(taxonID, acceptedTaxonID, scientificName, vernacularName, taxonRank)

## NEON STEP 1: CHECK AGAINST OFFICIAL TAXONOMIC LIST FOR TYPOS/MISMATCHES ---- 

#filter only NEON 

NEON_data <- harmonized %>% 
  filter(grepl('NEON', SITE))

#check it picked up only NEON sites and first check for obvious mismatches or typos

unique(NEON_data$SITE)

setdiff(NEON_data$SCI_NAME,
        taxon$scientificName) #all scientific names match

setdiff(NEON_data$SP_CODE,
        taxon$acceptedTaxonID) #all codes match 

#now check our data for any fish not at a species or subspecies level - in the taxonomic guide this will be helpful to check the taxonRank.

## NEON STEP 2: FILTER FOR ONLY SUBSPECIES & SPECIES ---- 

#pull our data that does not have a species or subspecies taxon rank

ranks <- taxon %>% 
  filter(taxonRank != "subspecies") %>% 
  filter(taxonRank != "species")

#this creates a character string of all the scientific names we have in our data that is what we SHOULD drop (anything not a species or subspecies)
drop_table <- NEON_data %>% 
  filter(SCI_NAME %in% ranks$scientificName) %>% 
  distinct(SCI_NAME) %>% 
  pull()

#now investigate - how much of our data would we actually be dropping?

NEON_data %>% 
  filter(SCI_NAME %in% drop_table) %>% #5148 rows
  summarize(proportion = (nrow(.)/nrow(NEON_data))*100) 

#only 0.783% of our overall data for NEON needs to be dropped! How about by site?

total_rows_site <- NEON_data %>% 
  group_by(SITE) %>% 
  count() %>% 
  rename(total_rows = `n`)

drop_rows_site <- NEON_data %>% 
  filter(SCI_NAME %in% drop_table) %>% 
  group_by(SITE) %>% 
  count() %>% 
  rename(drop = `n`)

#this is the final table that will tell us what the proportion of data is for EACH site that we would be dropping. Flag ones > 10%. 
final_drop_table <- total_rows_site %>% 
  left_join(drop_rows_site,
            by = "SITE") %>% 
  mutate(proportion = (drop/total_rows)*100) %>% 
  arrange(desc(proportion))

final_drop_table #looks like right now only one of concern is NEON_CRAM.

## NEON STEP 3: DROP DATA BASED ON TAXON FILTERING ------

NEON_data <- NEON_data %>% 
  filter(!SCI_NAME %in% ranks$scientificName)

## NEON STEP 4: "RARE" SPECIES ------

#at each site, figure out if there are any species that have occurred only 1 or 2 times. We will want to drop these. 

#one way to visualize this is with a matrix 

species_counts <- as.data.frame(tapply(NEON_data$YEAR, list(NEON_data$SCI_NAME, NEON_data$SITE), timeseries))

NEON_data %>% 
  group_by(SITE, SCI_NAME) %>% 
  summarize(n_years = n_distinct(YEAR)) #this gives us how many years each species appears in the data 

rare_drops <- NEON_data %>% 
  group_by(SITE, SCI_NAME) %>% 
  summarize(n_years = n_distinct(YEAR)) %>% 
  filter(n_years < 3) %>% 
  select(!n_years) %>% 
  mutate(combo = paste(SITE,"_",SCI_NAME))

#How much data would that be dropping?

NEON_data %>% 
  mutate(combo = paste(SITE,"_",SCI_NAME)) %>% 
  filter(combo %in% rare_drops$combo) %>% 
  summarise(proportion = (nrow(.)/nrow(NEON_data))*100) #in total it's only about 0.332% of our data that we would have to drop, but what about at each SITE?

#Investigate same question but do proportions by site 

total_rows_rare <- NEON_data %>% 
  group_by(SITE) %>% 
  count() %>% 
  rename(total_rows = `n`)

drop_rows_rare <- NEON_data %>% 
  mutate(combo = paste(SITE,"_",SCI_NAME)) %>% 
  filter(combo %in% rare_drops$combo) %>% 
  group_by(SITE) %>% 
  count() %>% 
  rename(drop = `n`)

#this is the final table that will tell us what the proportion of data is for EACH site that we would be dropping. Flag ones > 10%. 
final_drop_table_rare <- total_rows_rare %>% 
  left_join(drop_rows_rare,
            by = "SITE") %>% 
  mutate(proportion = (drop/total_rows)*100) %>% 
  arrange(desc(proportion))

final_drop_table_rare #looks like right now only one of concern is NEON_POSE.


## NEON STEP 5: DROP DATA BASED ON SPECIES FREQUENCY/RARE SPECIES ----

NEON_data <- NEON_data %>% 
  mutate(combo = paste(SITE,"_",SCI_NAME)) %>% 
  filter(!combo %in% rare_drops$combo) %>% 
  select(!combo)

##APRIL UPDATE: DROP FROM SP'S GOOGLE DRIVE NOTES 

#These are sites that Sierra picked out to drop based on environmental variables not being available for > 5 years 

## NEON STEP 6: DROP DATA WITH <5 YEARS OF ENVIRONMENTAL DATA & FINAL DROPS

site_drops <- c("NEON_CARI",
                "NEON_PRLA",
                "NEON_PRPO",
                "NEON_CUPE",
                "NEON_GUIL",
                "NEON_LIRO",
                "NEON_TOOK")

NEON_data <- NEON_data %>% 
  filter(!(SITE %in% site_drops))

#there are also a couple sites where we're only dropping some of the variables:

#drop DO from NEON CRAM AND NEON LECO
NEON_data <- NEON_data %>%
  mutate(across(contains("DO"),
                ~ if_else(SITE %in% c("NEON_CRAM", "NEON_LECO"), NA_real_, .)))
#
# #drop temp from NEON KING
#
NEON_data <- NEON_data %>%
  mutate(across(contains("temp"),
                ~ if_else(SITE %in% c("NEON_KING"), NA_real_, .)))


#save as Rds to use in model scripts and PDF viz

saveRDS(NEON_harmonized,
        file = file.path("data",
                         "NEON_harmonized.Rds"))

#FINAL JOINT HARMONIZATION ----

NEON_harmonized <- NEON_data

## LTER STEP 1: CHECK AGAINST OFFICIAL TAXONOMIC LIST FOR TYPOS/MISMATCHES ---- 

#filter only LTER

LTER_data <- harmonized %>% 
  filter(grepl('LTER', SITE))

unique(LTER_data$MIDSITE)

#may have to do this site by site (got this from MCR metadata)

MCR_taxon <- read.csv(file = file.path("data",
                                       "MCR_LTER_Fish_Species_List.csv"))

MCR_taxon <- MCR_taxon %>% 
  select(speciesbinomial, commonname) %>% 
  rename(COMMON_NAME = commonname,
         SCI_NAME = speciesbinomial)

#NTL site (got this from Zach)

NTL_taxon <- read.csv(file = file.path("data",
                                       "NTL_taxon.csv"))

#need to change the string structure of all of these latin names. right now it's lowercase_lowercase and we need it to be "Uppercase lowercase"

NTL_taxon <- NTL_taxon %>% 
  select(!spp_code) %>% 
  rename(COMMON_NAME = common_name,
         SCI_NAME = latin_name) %>% 
  mutate(SCI_NAME = str_replace_all(SCI_NAME, "_", " ") %>%
           str_to_lower() %>%
           str_to_title())

#SBC site (got this from online metadata)

SBC_taxon <- read.csv(file = file.path("data",
                                       "SBC_LTER_species.csv")) %>% 
  select(SCIENTIFIC_NAME,
         COMMON_NAME) %>% 
  rename(SCI_NAME = SCIENTIFIC_NAME)

#now join all three together 

LTER_taxon <- rbind(SBC_taxon,
      MCR_taxon,
      NTL_taxon)

#now do a look at differences in taxon

setdiff(LTER_data$SCI_NAME, LTER_taxon$SCI_NAME) #85 differences to take care of - woof 

unique(LTER_data$SCI_NAME)

#how much data at each site are unidentified species? 

LTER_data %>%
  group_by(MIDSITE) %>% 
  summarise(
    row_count = sum(str_detect(SCI_NAME, 
                                regex("unidentified", 
                                      ignore_case = TRUE))),
    percent_unidentified = mean(str_detect(SCI_NAME, 
                                           regex("unidentified", 
                                                 ignore_case = TRUE))) * 100)
#good to drop, should be around 1000 rows 

LTER_data <- LTER_data %>%
  filter(!str_detect(SCI_NAME, regex("unidentified", 
                                     ignore_case = TRUE)) | 
           is.na(SCI_NAME)) #leave NAs in for now 

setdiff(LTER_data$SCI_NAME, LTER_taxon$SCI_NAME) #drops it down to 49 differences 

## LTER STEP 2: FILTER FOR ONLY SUBSPECIES & SPECIES ---- 

#pull our data that does not have a species or subspecies taxon rank

LTER_data %>%
  group_by(MIDSITE) %>% 
  summarise(
    row_count = sum(str_detect(SCI_NAME, 
                               regex("spp.", 
                                     ignore_case = TRUE))),
    percent_unidentified = mean(str_detect(SCI_NAME, 
                                           regex("spp.", 
                                                 ignore_case = TRUE))) * 100)

#something to potentially flag: 7% of data from VCR was "spp." -- investigate more. It appears that VCR uses spp. but in their common name has the specifics 

LTER_data %>% 
  filter(MIDSITE == "LTER_VCR") %>% 
  filter(str_detect(SCI_NAME, "spp.")) %>% 
  distinct(SCI_NAME, COMMON_NAME) #these are all really common and there's actually only 3. I would go in and manually replace these or drop them. 

#first manually replace anchovy names, then I think we'll have to drop Pipefish since we don't know species specifically. 

LTER_data$SCI_NAME[which(LTER_data$COMMON_NAME == "Bay Anchovy")] <- "Anchoa mitchilli"

LTER_data$SCI_NAME[which(LTER_data$COMMON_NAME == "American Anchovy")] <- "Engraulis mordax"

#now when we run this again the VCR percentage should go down. Important to note still that over 4% of all VCR data that we're dropping is Pipefish. 
LTER_data %>%
  group_by(MIDSITE) %>% 
  summarise(
    row_count = sum(str_detect(SCI_NAME, 
                               regex("spp."))),
    percent_unidentified = mean(str_detect(SCI_NAME, 
                                           regex("spp."))) * 100)

#double check that it is only picking up the right part of the spp. character string

LTER_data %>% 
  filter(str_detect(SCI_NAME, "spp.")) %>% 
  distinct(SCI_NAME, COMMON_NAME) #so it's just the pipefish that are going away

#should drop less than 1000 rows

LTER_data <- LTER_data %>%
  filter(!str_detect(SCI_NAME, regex("spp.", 
                                    ignore_case = TRUE)) | 
           is.na(SCI_NAME)) #leave NAs in for now 

#now do the same thing for "sp."

LTER_data %>%
  group_by(MIDSITE) %>% 
  summarise(
    row_count = sum(str_detect(SCI_NAME, 
                               regex("sp\\.", 
                                     ignore_case = TRUE))),
    percent_unidentified = mean(str_detect(SCI_NAME, 
                                           regex("sp\\.", 
                                                 ignore_case = TRUE))) * 100)

#good to drop all these, all super low percentages! Should drop less than 300.

LTER_data <- LTER_data %>%
  filter(!str_detect(SCI_NAME, regex("sp\\.", 
                                     ignore_case = TRUE)) | 
           is.na(SCI_NAME)) #leave NAs in for now 

setdiff(LTER_data$SCI_NAME, LTER_taxon$SCI_NAME) #drops it down to 44 differences 

## LTER STEP 3: COMMON NAMES AT LTER NTL SITE -----


#Note this is a slightly different step compared to NEON because at LTER there are no scientific names, only common. So we'll have to merge those first, then look for difference as the next taxonomic step.
