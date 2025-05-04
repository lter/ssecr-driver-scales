
# Script to take harmonized dataset and convert it to individual, population and community level datasets

library(tidyverse)
library(vegan)

final_harmonized <- readRDS(file.path("data","final_harmonized.Rds"))
                            
final_harmonized <- final_harmonized %>%
  mutate(TYPE = case_when(
    grepl("NEON|NTL", MIDSITE) ~ "AQUATIC",
    grepl("MCR|SBC|VCR", MIDSITE) ~ "MARINE",
  ))

clim_dat <- final_harmonized %>%
  group_by(YEAR, MIDSITE, SUBSITE, TYPE) %>%
  # get one number per SUBSITE (no variation with year-subsite)
  summarise(mean_daily_DO = mean(mean_daily_DO),
            mean_daily_temp = mean(mean_daily_temp), 
            mean_min_DO = mean(mean_min_DO), 
            mean_min_temp = mean(mean_min_temp),
            mean_max_temp = mean(mean_max_temp)) %>%
  # now get means at MIDSITE level
  ungroup() %>%
  group_by(YEAR, MIDSITE) %>%
  summarise(mean_daily_DO = mean(mean_daily_DO, na.rm = TRUE),
            mean_daily_temp = mean(mean_daily_temp, na.rm = TRUE), 
            mean_min_DO = mean(mean_min_DO, na.rm = TRUE), 
            mean_min_temp = mean(mean_min_temp, na.rm = TRUE),
            mean_max_temp = mean(mean_max_temp, na.rm = TRUE)) %>%
  # now scale them to temporal deviations from MIDSITE mean
  ungroup() %>%
  group_by(MIDSITE) %>%
  mutate(scaled_mean_daily_DO = scale(mean_daily_DO),
         scaled_mean_daily_temp = scale(mean_daily_temp), 
         scaled_mean_min_DO = scale(mean_min_DO), 
         scaled_mean_min_temp = scale(mean_min_temp),
         scaled_mean_max_temp = scale(mean_max_temp))

full_dat <- final_harmonized %>%
  left_join(clim_dat, by = c("MIDSITE", "YEAR"))

write.csv(full_dat, file.path("data","ind_dat.csv"))

# getting population counts
pop_dat <- full_dat %>%
  distinct(MIDSITE, DATE, 
           scaled_mean_daily_DO, scaled_mean_daily_temp, EFFORT) %>%
  crossing(distinct(full_dat, SCI_NAME)) %>%
  left_join(full_dat %>%
              count(MIDSITE, DATE, SCI_NAME, EFFORT, TYPE, name = "count"), 
            by = c("MIDSITE", "DATE", "SCI_NAME", "EFFORT")) %>%
  mutate(TYPE = case_when(
    grepl("NEON|NTL", MIDSITE) ~ "AQUATIC",
    grepl("MCR|SBC|VCR", MIDSITE) ~ "MARINE",
  )) %>%
  mutate(count = replace_na(count, 0)) %>%
  mutate(CPUE = ifelse(count == 0, 0, count/EFFORT)) %>%
  ungroup() %>%
  group_by(MIDSITE, DATE, 
           scaled_mean_daily_DO, scaled_mean_daily_temp, 
           SCI_NAME, TYPE) %>%
  summarise(count = sum(count), 
            CPUE = sum(CPUE)) %>%
  ungroup() %>%
  group_by(MIDSITE, SCI_NAME) %>%
  filter(any(count != 0)) %>%
  ungroup()

write.csv(pop_dat, file.path("data","pop_dat.csv"))

# getting community metrics
com_dat <- pop_dat %>%
  mutate(YEAR = year(DATE)) %>%
  group_by(MIDSITE) %>%
  mutate(min.count = min(table(.$YEAR))) %>%
  group_by(MIDSITE, DATE, TYPE, 
           scaled_mean_daily_DO, scaled_mean_daily_temp) %>%
  summarise(diversity = diversity(CPUE), 
            total = sum(CPUE), 
            richness = n_distinct(SCI_NAME[which(CPUE > 0)]), 
            evenness = diversity(CPUE)/log(n_distinct(SCI_NAME[which(CPUE > 0)])))

write.csv(com_dat, file.path("data","com_dat.csv"))


