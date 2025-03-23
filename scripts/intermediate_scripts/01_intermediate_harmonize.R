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
                                           "intermediate_data"),
                          pattern = "\\.csv$", 
                         full.names = TRUE) %>%
  set_names() %>% 
    purrr::map_dfr(read_csv,.id="file_name") %>% # Reads and binds all csvs
  mutate(file_name = basename(file_name))%>%
  rename(SITE = file_name)

harmonized$SITE<-str_remove(harmonized$SITE,'_intermediate.csv')
  
unique(harmonized$SITE)

#Bring in taxon list (first from NEON then can append LTER in later)

taxon <- read.csv(file = file.path("data",
                                   "NEON_fish_taxonomy.csv"))

taxon <-  taxon %>% 
  select(taxonID, acceptedTaxonID, scientificName, vernacularName, taxonRank)
  
## CHECK AGAINST OFFICIAL TAXONOMIC LIST FOR TYPOS/MISMATCHES ---- 

#filter only NEON for now 

NEON_data <- harmonized %>% 
  filter(grepl('NEON', SITE))

#check it picked up only NEON sites and first check for obvious mismatches or typos

unique(NEON_data$SITE)

setdiff(NEON_data$SCI_NAME,
        taxon$scientificName) #all scientific names match

setdiff(NEON_data$SP_CODE,
        taxon$acceptedTaxonID) #all codes match 

#now check our data for any fish not at a species or subspecies level - in the taxonomic guide this will be helpful to check the taxonRank.

## FILTER FOR ONLY SUBSPECIES & SPECIES ---- 

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

## DROP DATA ------

NEON_data <- NEON_data %>% 
  filter(!SCI_NAME %in% ranks$scientificName)


# VIZ LOOPS ----- 

