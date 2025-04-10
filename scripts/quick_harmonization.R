library(tidyverse)
library(vegan)

# QUICK AND DIRTY GENERATION OF HARMONIZED IND, POP, + COM DATASETS

# Bring data in

folder = "intermediate_data"
files = list.files(
  path = folder,
  pattern = ".*csv$",
  ignore.case = T,
  full.names = T
)
data = lapply(files, read.csv)
names(data) = c("MCR", "SBC", "VCR", "ARIK", "CARI", "CRAM", 
                "CUPE", "GUIL", "HOPB", "KING", "LECO", "LEWI", 
                "LIRO", "MAYF", "MCDI", "POSE", "PRIN", "PRLA", 
                "PRPO", "TOOK", "WALK")
full_dat <- bind_rows(data, .id = "Site1")

# filtering out low abundance spp (need to refine filtering)
full_dat2 <- full_dat %>% 
  mutate(sampling_event = paste(DATE, SUBSITE, EFFORT)) %>%
  group_by(SCI_NAME) %>%
  filter(n() > 100) %>%
  ungroup()

# scaling climate data
clim_dat <- full_dat2 %>%
  group_by(Site1, YEAR, SUBSITE) %>%
  summarise(mean_daily_DO = mean(mean_daily_DO), 
            mean_daily_temp = mean(mean_daily_temp)) %>%
  ungroup() %>%
  group_by(Site1) %>%
  mutate(DO_scaled = scale(mean_daily_DO), 
         temp_scaled = scale(mean_daily_temp)) %>%
  select(Site1, YEAR, SUBSITE, DO_scaled, temp_scaled)

full_dat2 <- full_dat2 %>%
  left_join(clim_dat, by = c("Site1", "YEAR", "SUBSITE"))

# getting population counts
pop_dat <- full_dat2 %>%
  distinct(Site1, DATE, DO_scaled, temp_scaled) %>%
  crossing(distinct(full_dat2, SCI_NAME)) %>%
  left_join(full_dat2 %>%
              count(Site1, DATE, SCI_NAME, name = "count"), 
            by = c("Site1", "DATE", "SCI_NAME")) %>%
  mutate(count = replace_na(count, 0))

# getting community metrics
com_dat <- pop_dat %>%
  group_by(Site1, DATE, DO_scaled, temp_scaled) %>%
  summarise(diversity = diversity(count), 
            total = sum(count))
  
# saving
saveRDS(full_dat2, file.path("data", "ind_dat.RDS"))
saveRDS(pop_dat, file.path("data", "pop_dat.RDS"))
saveRDS(com_dat, file.path("data", "com_dat.RDS"))
