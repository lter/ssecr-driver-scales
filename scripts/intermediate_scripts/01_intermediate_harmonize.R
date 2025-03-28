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
                 vegan, neonUtilities, corrplot, emmeans)

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

## DROP DATA BASED ON TAXON FILTERING ------

NEON_data <- NEON_data %>% 
  filter(!SCI_NAME %in% ranks$scientificName)

## "RARE" SPECIES ------

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


##DROP DATA BASED ON SPECIES FREQUENCY ----

NEON_data <- NEON_data %>% 
  mutate(combo = paste(SITE,"_",SCI_NAME)) %>% 
  filter(!combo %in% rare_drops$combo) %>% 
  select(!combo)
  
# VIZ LOOP DEMOS ----- 

#create demo site just to make sure visuals work 

### Correlation matrix of predictor variables -----

demo <- NEON_data %>% 
  filter(SITE == "NEON_ARIK")

## Correlation matrix of all predictor variables 

predictor_cor <- demo %>% 
  select(SIZE, mean_daily_DO:mean_min_temp) %>% 
  cor(use = "complete.obs")

corrplot(predictor_cor,
         method = "color", 
         addCoef.col="black", 
         order = "AOE", 
         number.cex=.75,
         type = "upper",
         tl.cex = .75,
         tl.col = "black",
         diag = F,
         tl.srt = 45)

### Size ~ Year colored by species -----

demo %>% 
ggplot(aes(x=YEAR,y=SIZE, color = SCI_NAME))+
  geom_point(alpha = .25) +
  theme_cowplot() +
  theme(legend.position = "none")

### mean daily temp ~ year -----

demo %>% 
ggplot(aes(x=YEAR, y=mean_daily_temp))+
  geom_point(color="darkred") +
  ylab("MEAN DAILY TEMP") +
  scale_x_continuous(labels = function(x) x -1) +
  theme_cowplot()

### average annual DO ~ year -----

demo %>% 
ggplot(aes(x=YEAR, y=annual_avg_DO))+
  geom_point(color="navy") + 
  ylab("ANNUAL AVG. DO") +
  scale_x_continuous(labels = function(x) x -1) +
  theme_cowplot()

### individual level ~ temperature ------

demo %>% 
ggplot(aes(x=mean_daily_temp, y=SIZE))+
  geom_point(aes(color = SCI_NAME), alpha = .25) +
  geom_smooth(aes(color = SCI_NAME), method=lm,se=F)+
  geom_smooth(color = "black", linetype = "dashed", method=lm,se=F)+
  xlab("MEAN DAILY TEMP") +
  theme_cowplot() +
  theme(legend.position = "none")

### individual level ~ temperature model accounting for DO ------

do_temp_size_model <- lm(log(SIZE) ~ mean_daily_temp * 
                           annual_avg_DO, demo)

predict_size_temp <- data.frame(emmeans(do_temp_size_model, 
                                        ~mean_daily_temp * annual_avg_DO, 
                                        at=list(mean_daily_temp=c(15.5, 16, 16.5,17, 17.5))), 
                                type = "response")

ggplot(predict_size_temp,aes(x=mean_daily_temp, y=emmean))+
  geom_line(color = "darkred", linewidth =1) +
  geom_ribbon(data = predict_size_temp, aes(ymin=lower.CL, ymax=upper.CL), alpha = .25, fill = "darkred") +
  geom_ribbon(data = predict_size_temp, aes(ymin=lower.CL, ymax=upper.CL), alpha = .25, fill = "darkred") +
  xlab("MEAN DAILY TEMP") +
  ylab("SIZE") +
  theme_cowplot() +
  theme(legend.position = "none")

### individual level ~ DO ------

demo %>% 
ggplot(aes(x=annual_avg_DO, y=SIZE))+
  geom_point(aes(color = SCI_NAME), alpha = .25)+
  geom_smooth(aes(color = SCI_NAME), method=lm,se=F)+
  geom_smooth(color = "black", linetype = "dashed", method=lm,se=F)+
  xlab("ANNUAL AVG. DO") +
  theme_cowplot() +
  theme(legend.position = "none")

### individual level ~ DO model accounting for temperature ------

predict_size_do <- data.frame(emmeans(do_temp_size_model, ~mean_daily_temp * annual_avg_DO, 
                                      at=list(annual_avg_DO=c(8.5, 9, 9.5, 10, 10.5)), 
                                      type = "response"))

ggplot(predict_size_do,aes(x=annual_avg_DO, y=response))+
  geom_line(color = "navy", linewidth =1) +
  geom_ribbon(data = predict_size_do, aes(ymin=lower.CL, ymax=upper.CL), alpha = .25, fill = "navy") +
  geom_ribbon(data = predict_size_do, aes(ymin=lower.CL, ymax=upper.CL), alpha = .25, fill = "navy") +
  xlab("ANNUAL AVG. DO") +
  ylab("SIZE") +
  theme_cowplot() +
  theme(legend.position = "none")

### population level skip for now until effort is more ironed out 

##all of those graphs work so now build into a loop 

NEON_data %>% 
  select(SITE, SIZE, mean_daily_DO:mean_min_temp) %>% 
  group_by(SITE) %>% 
  summarise(across(everything(), ~sum(is.na(.))))

for(site in unique(NEON_data$SITE)) {
  
  predictor_cor <- NEON_data %>% 
    filter(SITE == site) %>% 
    select(SIZE, mean_daily_DO:mean_min_temp) %>% 
    cor(use = "complete.obs")
  
  corrplot(predictor_cor,
           method = "color", 
           addCoef.col="black", 
           order = "AOE", 
           number.cex=.75,
           type = "upper",
           tl.cex = .75,
           tl.col = "black",
           diag = F,
           tl.srt = 45)
  
}
