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

##MUTATE STRINGS TO CREATE MIDSITES -----

harmonized <- harmonized %>%
  mutate(MIDSITE = case_when(
    str_detect(SUBSITE, "Backreef") ~ "LTER_MCR_Backreef",
    str_detect(SUBSITE, "Fringing Reef") ~ "LTER_MCR_FringingReef",
    str_detect(SUBSITE, "Forereef") ~ "LTER_MCR_Forereef",
    TRUE ~ SITE)) %>% 
  relocate(MIDSITE, .after = SITE)

#do the same for NTL and each of the different lakes: 

harmonized %>% 
  filter(SITE == "LTER_NTL") %>% 
  distinct(SUBSITE)

#list of all lakes
# 1 AL  = Allequash Lake
# 2 BM = Big Muskellunge Lake
# 3 FI = Fish Lake
# 4 ME  = Lake Mendota
# 5 MO  = Lake Monona
# 6 SP  = Sparkling Lake
# 7 TR  = Trout Lake
# 8 WI = Lake Wingra

harmonized <- harmonized %>%
  mutate(MIDSITE = case_when(
    SITE == "LTER_NTL" & str_detect(SUBSITE, "AL") ~ "LTER_NTL_Allequash Lake",
    SITE == "LTER_NTL" & str_detect(SUBSITE, "BM") ~ "LTER_NTL_Big Muskellunge Lake",
    SITE == "LTER_NTL" & str_detect(SUBSITE, "FI") ~ "LTER_NTL_Fish Lake",
    SITE == "LTER_NTL" & str_detect(SUBSITE, "ME") ~ "LTER_NTL_Lake Mendota",
    SITE == "LTER_NTL" & str_detect(SUBSITE, "MO") ~ "LTER_NTL_Lake Monona",
    SITE == "LTER_NTL" & str_detect(SUBSITE, "SP") ~ "LTER_NTL_Sparkling Lake",
    SITE == "LTER_NTL" & str_detect(SUBSITE, "TR") ~ "LTER_NTL_Trout Lake",
    SITE == "LTER_NTL" & str_detect(SUBSITE, "WI") ~ "LTER_NTL_Lake Wingra",
    TRUE ~ MIDSITE)) %>% 
  relocate(MIDSITE, .after = SITE)

#last thing to do for LTER - SBC! Each SBC site is going to get their own midsite:

harmonized %>% 
  filter(SITE == "LTER_SBC") %>% 
  distinct(SUBSITE)

harmonized <- harmonized %>%
  mutate(MIDSITE = case_when(
    (SITE == "LTER_SBC" & SUBSITE == "AQUE") ~ "LTER_SBC_AQUE",
    (SITE == "LTER_SBC" & SUBSITE == "MOHK") ~ "LTER_SBC_MOHK",
    (SITE == "LTER_SBC" & SUBSITE == "CARP") ~ "LTER_SBC_CARP",
    (SITE == "LTER_SBC" & SUBSITE == "ABUR") ~ "LTER_SBC_ABUR",
    (SITE == "LTER_SBC" & SUBSITE == "GOLB") ~ "LTER_SBC_GOLB",
    (SITE == "LTER_SBC" & SUBSITE == "NAPL") ~ "LTER_SBC_NAPL",
    (SITE == "LTER_SBC" & SUBSITE == "IVEE") ~ "LTER_SBC_IVEE",
    (SITE == "LTER_SBC" & SUBSITE == "AHND") ~ "LTER_SBC_AHND",
    (SITE == "LTER_SBC" & SUBSITE == "BULL") ~ "LTER_SBC_BULL",
    (SITE == "LTER_SBC" & SUBSITE == "SCDI") ~ "LTER_SBC_SCDI",
    (SITE == "LTER_SBC" & SUBSITE == "SCTW") ~ "LTER_SBC_SCTW",
    TRUE ~ MIDSITE)) %>% 
  relocate(MIDSITE, .after = SITE)

#in total that means we have 41 midsites to look at 

length(unique(harmonized$MIDSITE))

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

length(unique(NEON_data$MIDSITE)) #18 unique midsites

setdiff(NEON_data$SCI_NAME,
        taxon$scientificName) #all scientific names match

setdiff(NEON_data$SP_CODE,
        taxon$acceptedTaxonID) #all codes match 

length(unique(NEON_data$SCI_NAME))

#now check our data for any fish not at a species or subspecies level - in the taxonomic guide this will be helpful to check the taxonRank.

## NEON STEP 2: FILTER FOR ONLY SUBSPECIES & SPECIES ---- 

#We want to filter out in our data anything that is not at the species or subspecies level. What I'm going to do is build a table from the taxon list that is NOT subspecies or species, then see what matches in our data (to then take out).

ranks <- taxon %>% 
  filter(taxonRank != "subspecies") %>% 
  filter(taxonRank != "species")

#this creates a character string of all the scientific names we have in our data that is what we SHOULD drop (anything not a species or subspecies)

drop_table <- NEON_data %>% 
  filter(SCI_NAME %in% ranks$scientificName) %>% 
  distinct(SCI_NAME) %>% 
  pull()

length(drop_table)

#now investigate - how much of our data would we actually be dropping?

NEON_data %>% 
  filter(SCI_NAME %in% drop_table) %>% #5148 rows
  summarize(proportion = (nrow(.)/nrow(NEON_data))*100) 

#only 0.783% of our overall data for NEON needs to be dropped! How about by site?

#this calculates the total number of rows for each NEON site 

total_rows_midsite <- NEON_data %>% 
  group_by(MIDSITE) %>% 
  count() %>% 
  rename(total_rows = `n`)

#this is the number of rows in each NEON site that we would drop based on taxon rank

drop_rows_midsite <- NEON_data %>% 
  filter(SCI_NAME %in% drop_table) %>% 
  group_by(MIDSITE) %>% 
  count() %>% 
  rename(drop = `n`)

#this is the final table that will tell us what the proportion of data is for EACH site that we would be dropping. Flag ones > 10%. 

final_drop_table <- total_rows_midsite %>% 
  left_join(drop_rows_midsite,
            by = "MIDSITE") %>% 
  mutate(proportion = (drop/total_rows)*100) %>% 
  arrange(desc(proportion))

final_drop_table %>% 
  select(MIDSITE, proportion)

## NEON STEP 3: DROP DATA BASED ON TAXON FILTERING ------

NEON_data <- NEON_data %>% 
  filter(!SCI_NAME %in% ranks$scientificName)

## NEON STEP 4: "RARE" SPECIES ------

#at each site, figure out if there are any species that have occurred only 1 or 2 times. We will want to drop these. 

#one way to visualize this is with a matrix 

species_counts <- as.data.frame(tapply(NEON_data$YEAR, list(NEON_data$SCI_NAME, NEON_data$MIDSITE), timeseries))

NEON_data %>% 
  group_by(MIDSITE, SCI_NAME) %>% 
  summarize(n_years = n_distinct(YEAR)) #this gives us how many years each species appears in the data 

#so now drop the data if that n_years is less than 3 (so 1 or 2 years)

rare_drops <- NEON_data %>% 
  group_by(MIDSITE, SCI_NAME) %>% 
  summarize(n_years = n_distinct(YEAR)) %>% 
  filter(n_years < 3) %>% 
  select(!n_years) %>% 
  mutate(combo = paste(MIDSITE,"_",SCI_NAME))

rare_drops
#How much data would that be dropping?

NEON_data %>% 
  mutate(combo = paste(MIDSITE,"_",SCI_NAME)) %>% 
  filter(combo %in% rare_drops$combo) %>% 
  summarise(proportion = (nrow(.)/nrow(NEON_data))*100) #in total it's only about 0.332% of our data that we would have to drop, but what about at each SITE?

#Investigate same question but do proportions by site 

total_rows_rare <- NEON_data %>% 
  group_by(MIDSITE) %>% 
  count() %>% 
  rename(total_rows = `n`)

drop_rows_rare <- NEON_data %>% 
  mutate(combo = paste(MIDSITE,"_",SCI_NAME)) %>% 
  filter(combo %in% rare_drops$combo) %>% 
  group_by(MIDSITE) %>% 
  count() %>% 
  rename(drop = `n`)

#this is the final table that will tell us what the proportion of data is for EACH site that we would be dropping. Flag ones > 10%. 
final_drop_table_rare <- total_rows_rare %>% 
  left_join(drop_rows_rare,
            by = "MIDSITE") %>% 
  mutate(proportion = (drop/total_rows)*100) %>% 
  arrange(desc(proportion))

final_drop_table_rare %>% 
  select(MIDSITE, proportion) #looks like right now only one of concern is NEON_POSE. By concern what we mean is WHEN we drop the data, that amount of data is what is going to be dropped.

## NEON STEP 5: DROP DATA BASED ON SPECIES FREQUENCY/RARE SPECIES ----

NEON_data <- NEON_data %>% 
  mutate(combo = paste(MIDSITE,"_",SCI_NAME)) %>% 
  filter(!combo %in% rare_drops$combo) %>% 
  select(!combo)

#Update from Bethany: great catch - looks like some species are being double counted based on the structure of the species name - unsure which sites exactly these species are coming from, so let's find them first so that the "split up" final data is still correct.

doublecounts <- c("Lepomis cyanellus", "Ameiurus melas", "Ameiurus natalis", "catostomus commersonii", "cottus bairdii", "Etheostoma exile", "Etheostoma nigrum", "Lepomis gibbosus", "Lepomis macrochirus", "Luxilus cornutus", "Micropterus dolomieu", "Micropterus salmoides", "Notemigonus crysoleucas", "Notropis volucellus", "Perca flavescens", "Pimephales notatus", "Pseudocheilinus octotaenia", "Pseudocheilinus octotaenia (cf)")

NEON_data %>% 
  filter(SCI_NAME %in% doublecounts) %>% 
  distinct(SCI_NAME)

#now let's make every species name structured the same 

NEON_data %>% 
  distinct(SCI_NAME) #hmm, looks like they all match the correct format in the NEON data - issue might be in the LTER data? 

#let's check out the total species list we have so far for any last minute obvious drops I may have forgotten: 

NEON_species <- unique(NEON_data$SCI_NAME)

taxon %>% 
  filter(scientificName %in% NEON_species)

#from this list there are no obvious outliers or ones that are unidentified! 

## NEON STEP 6: DROPPING MIDSITES BASED ON YEARS OF AVAILABLE ENV DATA ----

NEON_do_years_table <- NEON_data %>% 
  filter(!is.na(annual_avg_DO)) %>% 
  distinct(MIDSITE, YEAR) %>%
  count(MIDSITE, name = "years_with_do_data")

NEON_temp_years_table <- NEON_data %>% 
  filter(!is.na(mean_daily_temp)) %>% 
  distinct(MIDSITE, YEAR) %>%
  count(MIDSITE, name = "years_with_temp_data")

NEON_env_table <- NEON_do_years_table %>% 
  left_join(NEON_temp_years_table, by = "MIDSITE") %>% 
  arrange(desc(years_with_do_data))

#we ideally need both rows to have at least 5 years of data - what issues does that cause us if we drop those? 

NEON_env_table %>% 
  filter(years_with_do_data < 5 |
           years_with_temp_data < 5)

#let's make a graph that goes with this to show everyone 

#graph showing which years have BOTH temp and DO 

NEON_data %>% 
  distinct(MIDSITE, YEAR) %>% 
  ggplot(aes(x = YEAR,
             y = MIDSITE)) +
  geom_point(color = "black") + 
  geom_point(data = (NEON_data %>% 
               filter(!is.na(annual_avg_DO)) %>% 
                 filter(!is.na(mean_daily_temp))),
             aes(color = MIDSITE),
             show.legend = F,
             size = 3) +
  theme_bw()


#how many sites and how much data will we drop if we drop these problem sites? 

problem_sites <- NEON_env_table %>% 
  filter(years_with_do_data < 5 |
           years_with_temp_data < 5) %>% 
  pull(MIDSITE)

NEON_data %>% 
  filter(MIDSITE %in% problem_sites) %>% 
  summarise(proportion = (nrow(.)/nrow(NEON_data))*100) #90% of our data would be dropped if we kept ONLY sites that had BOTH env variables > 5 years 

NEON_data %>% 
  filter(!(MIDSITE %in% problem_sites)) %>% 
  count() #we would only have 64982 rows if we kept ONLY sites that had BOTH env variables > 5 years 

#instead, let's rock with Jeremy's idea of keeping sites if at LEAST one site has at least 5 years of env data (just switch code to &)

problem_sites <- NEON_env_table %>% 
  filter(years_with_do_data < 5 &
           years_with_temp_data < 5) %>% 
  pull(MIDSITE)

#now instead we are only dropping 6 sites: 

# NEON_CUPE
# NEON_GUIL
# NEON_PRPO
# NEON_LIRO
# NEON_PRLA
# NEON_TOOK 

NEON_data %>% 
  filter(MIDSITE %in% problem_sites) %>% 
  summarise(proportion = (nrow(.)/nrow(NEON_data))*100) #I mean shoot, we're still dropping 83.8% of our data. 

NEON_data %>% 
  filter(!(MIDSITE %in% problem_sites)) %>% 
  count() #we would only have 105,106 rows if we kept ONLY sites that had one or the other env variables for 5 years. 


NEON_data <- NEON_data %>%
  filter(!(SITE %in% problem_sites))

#NEON additional drops:

#here's a list from the data notes of additional drops we'll need to make:
#CARI: drop site
#KING: drop temp at site 
#LECO: drop DO at site


#there are also a couple sites where we're only dropping some of the variables:

#drop DO NEON LECO
NEON_data <- NEON_data %>%
  mutate(across(contains("DO"),
                ~ if_else(SITE %in% c("NEON_LECO"), NA_real_, .)))

# drop temp from NEON KING
NEON_data <- NEON_data %>%
  mutate(across(contains("temp"),
                ~ if_else(SITE %in% c("NEON_KING"), NA_real_, .)))

#drop CARI site entirely

NEON_data %>% 
  filter(SITE != "NEON_CARI")

#FINAL NEON JOINT HARMONIZATION ----

NEON_harmonized <- NEON_data

#check all species latin name structures:

NEON_harmonized <- NEON_harmonized %>% 
  mutate(SCI_NAME = str_to_sentence(SCI_NAME))

#save as Rds to use in model scripts and PDF viz and also as a .csv for Jeremy potentially

saveRDS(NEON_harmonized,
        file = file.path("data",
                         "clean_data",
                         "NEON_harmonized.Rds"))

write.csv(NEON_harmonized,
          file = file.path("data",
                           "clean_data",
                           "NEON_harmonized.csv"))
#FINAL NEON OUTPUTS ------

#species list ranked by commonality (how many sites are they at)

NEON_species_list <- NEON_harmonized %>% 
  group_by(SCI_NAME) %>% 
  summarise(n_midsites = n_distinct(MIDSITE)) %>% 
  arrange(-n_midsites)

write.csv(NEON_species_list,
          file = file.path("data",
                           "clean_data",
                           "NEON_specieslist.csv"))

#final list of midsites plus the number of years of fish data, env data, and how many unique species are at each site 

NEON_harmonized_summary <- NEON_harmonized %>% 
  group_by(MIDSITE) %>% 
  summarise("Unique Species" = n_distinct(SCI_NAME),
            "Years of Fish Data" = n_distinct(YEAR)) %>% 
  left_join(NEON_env_table, by = "MIDSITE") %>% 
  arrange(desc(`Unique Species`)) %>% 
  rename("Years of Temp Data" = years_with_temp_data,
         "Years of DO Data" = years_with_do_data)
  
write.csv(NEON_harmonized_summary,
          file = file.path("data",
                           "clean_data",
                           "NEON_harmonized_summary.csv"))


## LTER STEP 1: READ IN TAXON LISTS ---- 

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
         SCI_NAME = speciesbinomial) %>% 
  add_column(ORIGIN = "MCR")

#NTL site (got this from Zach)

NTL_taxon <- read.csv(file = file.path("data",
                                       "NTL_taxon.csv"))

#need to change the string structure of all of these latin names. right now it's lowercase_lowercase and we need it to be "Uppercase lowercase". Update: for common name in order to match the NTL data we want all uppercase, no spaces. 

NTL_taxon <- NTL_taxon %>% 
  select(!spp_code) %>% 
  rename(COMMON_NAME = common_name,
         SCI_NAME = latin_name) %>% 
  mutate(SCI_NAME = str_replace_all(SCI_NAME, "_", " ") %>%
           str_to_lower() %>%
           str_to_title()) %>% 
  mutate(COMMON_NAME = str_replace_all(COMMON_NAME, "_", "") %>%
           str_to_upper()) %>% 
  add_column(ORIGIN = "NTL")

#SBC site (got this from online metadata)

SBC_taxon <- read.csv(file = file.path("data",
                                       "SBC_LTER_species.csv")) %>% 
  select(SCIENTIFIC_NAME,
         COMMON_NAME) %>% 
  rename(SCI_NAME = SCIENTIFIC_NAME) %>% 
  add_column(ORIGIN = "SBC")

#now join all three together 

LTER_taxon <- rbind(SBC_taxon,
      MCR_taxon,
      NTL_taxon)

LTER_taxon <- distinct(LTER_taxon)

#now do a look at differences in taxon

setdiff(LTER_data$SCI_NAME, LTER_taxon$SCI_NAME) #85 differences to take care of - woof 

## LTER STEP 2: COMMON NAMES AT LTER NTL SITE -----

  #Note this is a slightly different step compared to NEON because at LTER there are no scientific names, only common. So we'll have to merge those first, then look for difference as the next taxonomic step.
  
  #first take out all NTL data (we'll add it back in once it's cleaned)
  
  LTER_NTL <- LTER_data %>% 
  filter(SITE == "LTER_NTL")

#take out NTL here and the rbind() later 
LTER_data <- LTER_data %>% 
  filter(SITE != "LTER_NTL")

#now let's take care of cleaning NTL first 

LTER_NTL <- LTER_NTL %>% 
  select(-SCI_NAME) %>%  # Drop the original SCI_NAME
  left_join(LTER_taxon %>% 
              select(COMMON_NAME, SCI_NAME), by = "COMMON_NAME") %>%
  relocate(SCI_NAME, .before = COMMON_NAME)

###LTER STEP 2B: REMOVING THE STOCK FISH -----

#BW caught / made a good point about NEON stocking their lakes - not an issue it turns out for NEON, but IS an issue for the LTER NTL site where they stock their lakes. Let's look at the species affected (we're just going to remove them for now):

stocked <- read.csv(file = file.path("data",
                                     "raw_data",
                                     "LTER_NTL",
                                     "NTL_stockedfish.csv"))

#change stocked quick to match our data because it's honestly the easiest workaround: 

stocked <- stocked %>% 
  filter(SUBSITE != "CRYSTAL LAKE") %>% 
  mutate(MIDSITE = case_when(
    str_detect(SUBSITE, "SPARKLING LAKE") ~ "LTER_NTL_Sparkling Lake",
    str_detect(SUBSITE, "TROUT LAKE") ~ "LTER_NTL_Trout Lake",
    str_detect(SUBSITE, "ALLEQUASH LAKE") ~ "LTER_NTL_Allequash Lake",
    str_detect(SUBSITE, "BIG MUSKELLUNGE LAKE") ~ "LTER_NTL_Big Muskellunge Lake",
    str_detect(SUBSITE, "FISH LAKE") ~ "LTER_NTL_Fish Lake",
    str_detect(SUBSITE, "LAKE MENDOTA") ~ "LTER_NTL_Lake Mendota",
    str_detect(SUBSITE, "LAKE WINGRA") ~ "LTER_NTL_Lake Wingra",
    str_detect(SUBSITE, "LAKE MONONA") ~ "LTER_NTL_Lake Monona",
    TRUE ~ SUBSITE)) %>% 
  relocate(MIDSITE, .after = SUBSITE) %>% 
  mutate(combo = paste(MIDSITE, COMMON_NAME, sep = "_"))

#now let's see how much of our data is from stocked fish:

LTER_NTL %>% 
  mutate(combo_main = paste(MIDSITE, COMMON_NAME, sep = "_")) %>% 
  filter(combo_main %in% stocked$combo) %>% 
  summarize(proportion = (nrow(.)/nrow(LTER_NTL))*100)

#about 8.96% of our data from the LTER_NTL comes from stocked fish. How about by lake? 

LTER_NTL %>% 
  mutate(combo_main = paste(MIDSITE, COMMON_NAME, sep = "_")) %>% 
  group_by(MIDSITE) %>% 
  summarize(total = n(),
    matched = sum(combo_main %in% stocked$combo),
    proportion = (matched / total) * 100) %>% 
  arrange(desc(proportion))
  
#we have some issues that can be dealt with LATER for these LTER lakes: the big 4 lakes Muskellunge, Mendota, Trout, Allequash all have over 10% of their data is stocked fish 

#KEEP FOR NOW UNTIL WE DISCUSS WITH ZACH

#now we can add the LTER_NTL data back into the LTER_data! 

LTER_data <- LTER_data %>% 
  bind_rows(LTER_NTL)

#done with getting all the names at least in the right order!~ 

##LTER STEP 3: CHECK AGAINST OFFICIAL TAXONOMIC LIST FOR TYPOS/MISMATCHES ---- 

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

#Need to change the "Pseudocheilinus octotaenia (cf)"

LTER_data$SCI_NAME[which(LTER_data$COMMON_NAME == "Pseudocheilinus octotaenia (cf)")] <- "Pseudocheilinus octotaenia"


## LTER STEP 4: FILTER FOR ONLY SUBSPECIES & SPECIES ---- 

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

## LTER STEP 5: "RARE" SPECIES ------

#at each site, figure out if there are any species that have occurred only 1 or 2 times. We will want to drop these. 

#one way to visualize this is with a matrix 

species_counts <- as.data.frame(tapply(LTER_data$YEAR, list(LTER_data$SCI_NAME, LTER_data$MIDSITE), timeseries))

LTER_data %>% 
  group_by(MIDSITE, SCI_NAME) %>% 
  summarize(n_years = n_distinct(YEAR)) #this gives us how many years each species appears in the data 

rare_drops_LTER <- LTER_data %>% 
  group_by(MIDSITE, SCI_NAME) %>% 
  summarize(n_years = n_distinct(YEAR)) %>% 
  filter(n_years < 3) %>% 
  select(!n_years) %>% 
  mutate(combo = paste(MIDSITE,"_",SCI_NAME))

#How much data would that be dropping?

LTER_data %>% 
  mutate(combo = paste(MIDSITE,"_",SCI_NAME)) %>% 
  filter(combo %in% rare_drops_LTER$combo) %>% 
  summarise(proportion = (nrow(.)/nrow(LTER_data))*100) #in total it's only about 0.539% of our data that we would have to drop, but what about at each SITE?

#Investigate same question but do proportions by site 

total_rows_rare_LTER <- LTER_data %>% 
  group_by(MIDSITE) %>% 
  count() %>% 
  rename(total_rows = `n`)

drop_rows_rare_LTER <- LTER_data %>% 
  mutate(combo = paste(MIDSITE,"_",SCI_NAME)) %>% 
  filter(combo %in% rare_drops_LTER$combo) %>% 
  group_by(MIDSITE) %>% 
  count() %>% 
  rename(drop = `n`)

#this is the final table that will tell us what the proportion of data is for EACH site that we would be dropping. Flag ones > 10%. 
final_drop_table_rare_LTER <- total_rows_rare_LTER %>% 
  left_join(drop_rows_rare_LTER,
            by = "MIDSITE") %>% 
  mutate(proportion = (drop/total_rows)*100) %>% 
  arrange(desc(proportion))

final_drop_table_rare_LTER 

#we'd be dropping 12.1% of the data at LTER_SBC_ABUR, just FYI

#drop the rare species 

## LTER STEP 6: DROP DATA BASED ON SPECIES FREQUENCY/RARE SPECIES ----

LTER_data <- LTER_data %>% 
  mutate(combo = paste(MIDSITE,"_",SCI_NAME)) %>% 
  filter(!combo %in% rare_drops_LTER$combo) %>% 
  select(!combo)

#now let's see where we're at:

setdiff(LTER_data$SCI_NAME, LTER_taxon$SCI_NAME) #down to 20 differences!!!

##LTER STEP 7: INVESTIGATING MISC. DIFFERENCES -----

#character strings of misc. diff. sci names 

misc <- setdiff(LTER_data$SCI_NAME, LTER_taxon$SCI_NAME)

#let's see what sites they're coming from: 

LTER_data %>% 
  filter(SCI_NAME %in% misc) %>%
  distinct(MIDSITE) #interesting only issues with MCR and VCR 

#"Acanthurus nigroris"is a blue lined surgeonfish - the other sci name that matches the data is "Acanthurus bleekeri"

LTER_data$SCI_NAME[which(LTER_data$SCI_NAME == "Acanthurus nigroris")] <- "Acanthurus bleekeri"

#"Hyporhamphus meeki" is MISSING from the taxon list but it is a valid species.

#"Anchoa mitchilli" is a Bay Anchovy (the only anchovy on the taxon list for whatever reason is the Indian Anchovy). Keep this species, it is valid. 

#"Centropristis striata" is a Black Sea Bass - valid species, just not in taxon list for SBC.

#""Micropogonias undulatus" is the Atlantic Croaker and is a valid species - not on VCR's taxon list. 

#"Gobiidae" is a family of gobies - will need to drop since not specific enough 

LTER_data <- LTER_data %>% 
  filter(SCI_NAME != "Gobiidae")

#"Eucinostomus argenteus" is a Silver Mojarra - valid species. Not on VCR's taxon list

#"Orthopristis chrysoptera" valid species (pigfish). Not in VCR's species list 

#"Lagodon rhomboides" Pinfish is a valid species. Not on VCR's species list.

#"Hippocampus erectus" is a seahorse - valid species not on VCR's list.

#"Archosargus probatocephalus" is Sheepshead - a valid species just not on VCR's list. 

#Bairdiella chrysoura is a silver perch - valid just not in VCR's list.

#"Menidia menidia" is Atlantic silverside - valid just on on VCR's list

#"Leiostomus xanthurus" is a spot/spot croaker - not on VCR list 

#"Chilomycterus schoepfii" is burrfish - valid just not on VCR list

#Mugilidae - family of fish so drop 

LTER_data <- LTER_data %>% 
  filter(SCI_NAME != "Mugilidae")

#"Paralichthys dentatus" is a Summer flounder just not on VCR's species list 

#"Tautoga onitis" is a Tautog just not on VCR's species list 

#Syngnathus is the family of pipefish - drop 

LTER_data <- LTER_data %>% 
  filter(SCI_NAME != "Syngnathus")

#"Teleostei"

#this is just the Latin group for fish - which rows have this?

LTER_data %>% 
  filter(SCI_NAME == "Teleostei")

#this was VCR's way of marking unidentified fish! Drop. 

LTER_data <- LTER_data %>% 
  filter(SCI_NAME != "Teleostei")

#should be the last one! 

#let's check the species list 

#BW caught the Gibbonsia that snuck in there 

LTER_data <- LTER_data %>% 
  filter(SCI_NAME != "Gibbonsia")

## LTER STEP 8: DROPPING MIDSITES BASED ON YEARS OF AVAILABLE ENV DATA ----

LTER_do_years_table <- LTER_data %>% 
  filter(!is.na(annual_avg_DO)) %>% 
  distinct(MIDSITE, YEAR) %>%
  count(MIDSITE, name = "years_with_do_data")

#something looks off here because hypothetically the DO data should only exist at 2 sites: AQUE and MOHK. FOR NOW (May 13) just drop the other data and replace with NA, but need to figure out why earlier intermediate function isn't dropping those 

LTER_data <- LTER_data %>%
  mutate(across(
    .cols = contains("DO"),
    .fns = ~ if_else(
      SITE == "LTER_SBC" & !(MIDSITE %in% c("LTER_SBC_MOHK", "LTER_SBC_AQUE")),
      NA_real_,
      .)))

#now make the DO table again

LTER_do_years_table <- LTER_data %>%
  group_by(MIDSITE, YEAR) %>%
  summarize(has_data = any(!is.na(annual_avg_DO)), 
            .groups = "drop") %>%
  group_by(MIDSITE) %>%
  summarize(years_with_do_data = sum(has_data), 
            .groups = "drop")

LTER_temp_years_table <- LTER_data %>% 
  filter(!is.na(mean_daily_temp)) %>% 
  distinct(MIDSITE, YEAR) %>%
  count(MIDSITE, name = "years_with_temp_data")

LTER_env_table <- LTER_temp_years_table %>% 
  left_join(LTER_do_years_table, by = "MIDSITE") %>% 
  arrange(desc(years_with_do_data))

LTER_env_table #each site we have more than enough data and more than 5 years! This is EXCEPT for MCR which we have NO DO DATA FOR 

#let's make a graph that goes with this to show everyone 

#graph showing which years have BOTH temp and DO 

LTER_data %>% 
  distinct(MIDSITE, YEAR) %>% 
  ggplot(aes(x = YEAR,
             y = MIDSITE)) +
  geom_point(color = "black") + 
  geom_point(data = (LTER_data %>% 
                       filter(!is.na(annual_avg_DO)) %>% 
                       filter(!is.na(mean_daily_temp))),
             aes(color = MIDSITE),
             show.legend = F,
             size = 3) +
  theme_bw()


#overall we still don't have "problem" sites because we at least have temp for MCR and the other SBC sites that don't have matching DO 

#getting rid of double counts by making sure all the species have same proper Latin structure 

LTER_data <- LTER_data %>% 
  mutate(SCI_NAME = str_to_sentence(SCI_NAME))

#FINAL LTER JOINT HARMONIZATION ----

LTER_harmonized <- LTER_data

#save as Rds to use in model scripts and PDF viz and also as a .csv for Jeremy potentially

saveRDS(LTER_harmonized,
        file = file.path("data",
                         "clean_data",
                         "LTER_harmonized.Rds"))

write.csv(LTER_harmonized,
          file = file.path("data",
                           "clean_data",
                           "LTER_harmonized.csv"))

#FINAL LTER OUTPUTS ------

#species list ranked by commonality (how many sites are they at)

LTER_species_list <- LTER_harmonized %>% 
  group_by(SCI_NAME) %>% 
  summarise(n_midsites = n_distinct(MIDSITE)) %>% 
  arrange(-n_midsites) %>% 
  arrange(SCI_NAME)

write.csv(LTER_species_list,
          file = file.path("data",
                           "clean_data",
                           "LTER_specieslist.csv"))

#final list of midsites plus the number of years of fish data, env data, and how many unique species are at each site 

LTER_harmonized_summary <- LTER_harmonized %>% 
  group_by(MIDSITE) %>% 
  summarise("Unique Species" = n_distinct(SCI_NAME),
            "Years of Fish Data" = n_distinct(YEAR)) %>% 
  left_join(LTER_env_table, by = "MIDSITE") %>% 
  arrange(desc(`Unique Species`)) %>% 
  rename("Years of Temp Data" = years_with_temp_data,
         "Years of DO Data" = years_with_do_data)

write.csv(LTER_harmonized_summary,
          file = file.path("data",
                           "clean_data",
                           "LTER_harmonized_summary.csv"))

# IEP STEP 1: Check sufficient taxonomic resolution -----

IEP_data <- harmonized %>% 
  filter(MIDSITE == "IEP_YOLO")

IEP_data %>%
  select(SCI_NAME) %>%
  unique() %>% c() # 49 taxa, all seem to be at species level


# IEP STEP 2: Drop rare species -----

IEP_data %>% 
  group_by(MIDSITE, SCI_NAME) %>% 
  summarize(n_years = n_distinct(YEAR)) %>%
  ggplot(aes(x = n_years)) + 
  geom_histogram()

IEP_data %>% 
  group_by(MIDSITE, SCI_NAME) %>% 
  summarize(n_years = n_distinct(YEAR)) %>%
  filter(n_years < 3) # just four species with one occurrance

rare_drops <- IEP_data %>% 
  group_by(MIDSITE, SCI_NAME) %>% 
  summarize(n_years = n_distinct(YEAR)) %>%
  filter(n_years < 3) %>% 
  ungroup() %>%
  select(SCI_NAME) %>% 
  unlist()

IEP_data <- IEP_data %>%
  filter(!SCI_NAME %in% rare_drops)

#double check sci.name structure

IEP_data <- IEP_data %>% 
  mutate(SCI_NAME = str_to_sentence(SCI_NAME))

#FINAL DATA JOINT HARMONIZATION ----

final_harmonized <- NEON_harmonized %>% 
  bind_rows(LTER_harmonized) %>%
  bind_rows(IEP_data)

final_harmonized %>% 
  group_by(SCI_NAME) %>% 
  summarise(n_midsites = n_distinct(MIDSITE)) %>% 
  arrange(-n_midsites) %>% 
  arrange(SCI_NAME)

#save as Rds to use in model scripts and PDF viz

saveRDS(final_harmonized,
        file = file.path("data",
                         "clean_data",
                         "final_harmonized.Rds"))

write.csv(final_harmonized,
          file = file.path("data",
                           "clean_data",
                           "final_harmonized.csv"))

#FINAL FULL HARMONIZED OUTPUTS ------

#species list ranked by commonality (how many sites are they at)

final_species_list <- final_harmonized %>% 
  group_by(SCI_NAME) %>% 
  summarise(n_midsites = n_distinct(MIDSITE)) %>% 
  arrange(-n_midsites) %>% 
  arrange(SCI_NAME)

write.csv(final_species_list,
          file = file.path("data",
                           "clean_data",
                           "final_specieslist.csv"))

#final list of midsites plus the number of years of fish data, env data, and how many unique species are at each site 

#first make final harmonized env table 

NEON_env_table <- NEON_env_table %>% 
  filter(!(MIDSITE %in% problem_sites))

final_env_table <- LTER_env_table %>% 
  rbind(NEON_env_table) 

final_harmonized_summary <- final_harmonized %>% 
  group_by(MIDSITE) %>% 
  summarise("Unique Species" = n_distinct(SCI_NAME),
            "Years of Fish Data" = n_distinct(YEAR)) %>% 
  left_join(final_env_table, by = "MIDSITE") %>% 
  arrange(desc(`Unique Species`)) %>% 
  rename("Years of Temp Data" = years_with_temp_data,
         "Years of DO Data" = years_with_do_data)

write.csv(final_harmonized_summary,
          file = file.path("data",
                           "clean_data",
                           "final_harmonized_summary.csv"))
