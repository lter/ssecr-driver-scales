#Overlapping species comparison - how do the same species at different sites compare in their responses?
# SCALES/ SSECR                  
# Allie Case  
# R Version: 4.4.1 (2024-6-14) -- "Race for your life"
#_________________________________

# setup ---------------------

rm(list = ls())


#install.packages("librarian")

librarian::shelf(supportR, tidyverse, summarytools, 
                 datacleanr, lterdatasampler,
                 cowplot, gt,
                 vegan,readxl,GGally)

#load and select data -----------

#first let's load in all the harmonized data:

data <- read.csv(file = file.path("data",
                                  "clean_data",
                                  "final_harmonized.csv"))

#now here's what we're looking for: species that occur at more than one midsite - I lied. Let's change this to site. 

site_overlaps <- data %>% 
  group_by(SCI_NAME) %>% 
  distinct(SITE) %>% 
  count(SCI_NAME) %>% 
  filter(`n` > 1) %>% 
  arrange(-`n`)

#28 species that are across at LEAST two different sites. 

all_overlaps <- data %>% 
  filter(SCI_NAME %in% site_overlaps$SCI_NAME) 

#now let's just try pulling the same exploratory viz as the PDFs and comparing across sites 

for(species in unique(all_overlaps$SCI_NAME)) {
  plot1 <- all_overlaps %>%
    filter(SCI_NAME == species) %>%
    ggplot(aes(x = SITE, y = SIZE, color = SITE)) +
    geom_boxplot(alpha = .25) +
    theme_cowplot() +
    theme(legend.position = "none",
          axis.text.x = element_text(
            angle = 45,
            vjust = 1,
            hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = bquote("Size of " * italic(.(species)) * " across sites"))
  
  print(plot1)
}

#so really there isn't much overlap species wise ACROSS sites (different story for midsites)

#check responses anyways: 

for(species in unique(all_overlaps$SCI_NAME)){
  plot2 <- all_overlaps %>% 
    filter(SCI_NAME == species) %>% 
    ggplot(aes(x=annual_avg_DO, y= SIZE, color = SITE)) +
    geom_point(aes(color = SITE), alpha = .05,
               show.legend = F)+
    geom_smooth(aes(color = SITE), method=lm,se =F,
                show.legend = T) +
    geom_smooth(color = "black", linetype = "dashed", 
                method=lm,se=F)+
    xlab("ANNUAL AVG. DO") +
    theme_cowplot() +
    labs(title = bquote("Size of " * italic(.(species)) * " versus DO across sites"))
  
  print(plot2)
}

### individual level ~ Temperature ------

for(species in unique(all_overlaps$SCI_NAME)){
  plot3 <- all_overlaps %>% 
    filter(SCI_NAME == species) %>% 
    ggplot(aes(x=mean_daily_temp, y= SIZE, color = SITE)) +
    geom_point(aes(color = SITE), alpha = .05,
               show.legend = F)+
    geom_smooth(aes(color = SITE), method=lm,se =F,
                show.legend = T) +
    geom_smooth(color = "black", linetype = "dashed", 
                method=lm,se=F)+
    xlab("Mean Daily Temp") +
    theme_cowplot() +
    labs(title = bquote("Size of " * italic(.(species)) * " versus mean temp across sites"))
  
  print(plot3)
}

