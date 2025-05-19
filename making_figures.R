
library(tidyverse)
library(GGally)
library(readxl)

MI <- readRDS(file.path("RDSs", "mean_ind_effects_df.RDS"))
MP <- readRDS(file.path("RDSs", "mean_pop_effects_df.RDS"))
MD <- readRDS(file.path("RDSs", "mean_div_effects_df.RDS"))
MT <- readRDS(file.path("RDSs", "mean_tot_effects_df.RDS"))
SI <- readRDS(file.path("RDSs", "species_ind_effects_df.RDS"))
SP <- readRDS(file.path("RDSs", "species_pop_effects_df.RDS"))
site.df <- read_xlsx("allsites_coords.xlsx")

# Mean Effects ------------------------------------------------------------

site.df <- site.df %>%
  rename("site" = "MIDSITE") %>%
  mutate(site = sub("^[^_]*_", "", site)) %>%
  mutate(site = sub(" ", "_", site))


MI <- MI %>%
  filter(site != "SBC") %>%
  merge(site.df, by = "site") %>%
  mutate(sig = lower > 0 | upper < 0)

MP <- MP %>%
  filter(site != "SBC") %>%
  merge(site.df, by = "site") %>%
  mutate(sig = lower > 0 | upper < 0)

MD <- MD %>%
  filter(site != "SBC") %>%
  merge(site.df, by = "site") %>%
  mutate(sig = lower > 0 | upper < 0)

MT <- MT %>%
  filter(site != "SBC") %>%
  merge(site.df, by = "site") %>%
  mutate(sig = lower > 0 | upper < 0)

## Individual -------------------------------------------------------------

MI %>%
  ggplot(aes(x = fct_reorder(site, as.numeric(factor(Habitat))),
                             y = median, ymin = lower, ymax = upper, 
             color = var, alpha = sig)) + 
  geom_pointrange(position = position_dodge(width= .5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, size = 10), 
        legend.position = "none") + 
  ylab("Effect") + xlab("Site") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_alpha_manual(values = c(0.25, 1))

ggsave(file.path("Figures", "Mean Effects", "Sites_By_Habitat", "Individual.pdf"), 
       width = 6, height = 5, units = "in")


MI %>%
  ggplot(aes(x = fct_reorder(site, Lat),
             y = median, ymin = lower, ymax = upper, 
             color = var, alpha = sig)) + 
  geom_pointrange(position = position_dodge(width= .5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, size = 10), 
        legend.position = "none") + 
  ylab("Effect") + xlab("Site") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_alpha_manual(values = c(0.25, 1))

ggsave(file.path("Figures", "Mean Effects", "Sites_By_Latitude", "Individual.pdf"), 
       width = 6, height = 5, units = "in")

## Population -------------------------------------------------------------

MP %>%
  ggplot(aes(x = fct_reorder(site, as.numeric(factor(Habitat))), 
             y = median, ymin = lower, ymax = upper, 
             color = var, alpha = sig)) + 
  geom_pointrange(position = position_dodge(width= .5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, size = 10), 
        legend.position = "none") + 
  ylab("Effect") + xlab("Site") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_alpha_manual(values = c(0.25, 1))

ggsave(file.path("Figures", "Mean Effects", "Sites_By_Habitat", "Population.pdf"), 
       width = 6, height = 5, units = "in")

MP %>%
  ggplot(aes(x = fct_reorder(site, Lat), 
             y = median, ymin = lower, ymax = upper, 
             color = var, alpha = sig)) + 
  geom_pointrange(position = position_dodge(width= .5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, size = 10), 
        legend.position = "none") + 
  ylab("Effect") + xlab("Site") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_alpha_manual(values = c(0.25, 1))

ggsave(file.path("Figures", "Mean Effects", "Sites_By_Latitude", "Population.pdf"), 
       width = 6, height = 5, units = "in")

## Community --------------------------------------------------------------

### Diversity -------------------------------------------------------------

MD %>%
  ggplot(aes(x = fct_reorder(site, as.numeric(factor(Habitat))), 
             y = median, ymin = lower, ymax = upper, 
             color = var, alpha = sig)) + 
  geom_pointrange(position = position_dodge(width= .5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, size = 10), 
        legend.position = "none") + 
  ylab("Effect") + xlab("Site") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_alpha_manual(values = c(0.25, 1))

ggsave(file.path("Figures", "Mean Effects", "Sites_By_Habitat", "Diversity.pdf"), 
       width = 6, height = 5, units = "in")

MD %>%
  ggplot(aes(x = fct_reorder(site, Lat), 
             y = median, ymin = lower, ymax = upper, 
             color = var, alpha = sig)) + 
  geom_pointrange(position = position_dodge(width= .5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, size = 10), 
        legend.position = "none") + 
  ylab("Effect") + xlab("Site") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_alpha_manual(values = c(0.25, 1))

ggsave(file.path("Figures", "Mean Effects", "Sites_By_Latitude", "Diversity.pdf"), 
       width = 6, height = 5, units = "in")

### Total CPUE ------------------------------------------------------------

MT %>%
  ggplot(aes(x = fct_reorder(site, as.numeric(factor(Habitat))), 
             y = median, ymin = lower, ymax = upper, 
             color = var, alpha = sig)) + 
  geom_pointrange(position = position_dodge(width= .5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, size = 10), 
        legend.position = "none") + 
  ylab("Effect") + xlab("Site") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_alpha_manual(values = c(0.25, 1))

ggsave(file.path("Figures", "Mean Effects", "Sites_By_Habitat", "Total.pdf"), 
       width = 6, height = 5, units = "in")

MT %>%
  ggplot(aes(x = fct_reorder(site, Lat), 
             y = median, ymin = lower, ymax = upper, 
             color = var, alpha = sig)) + 
  geom_pointrange(position = position_dodge(width= .5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, size = 10), 
        legend.position = "none") + 
  ylab("Effect") + xlab("Site") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_alpha_manual(values = c(0.25, 1))

ggsave(file.path("Figures", "Mean Effects", "Sites_By_Latitude", "Total.pdf"), 
       width = 6, height = 5, units = "in")

### All Collapsed ---------------------------------------------------------

bind_rows(list(ind = MI, pop = MP, tot = MT, div = MD), .id = "scale") %>%
  mutate(scale = factor(scale, levels = c("ind", "pop", "tot", "div"))) %>%
  ggplot(aes(x = scale, y = median, ymin = lower, ymax = upper, 
             color = var, alpha = sig, group = site)) + 
  geom_pointrange(position = position_jitterdodge(jitter.width = .25, 
                                                  dodge.width = .5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_classic(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0), 
        legend.position = "none") + 
  ylab("Effect") + xlab("Response Variable") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_alpha_manual(values = c(0.15, 1)) + 
  scale_x_discrete(labels = c("Size", "CPUE", "Summed CPUE", "Diversity"))

ggsave(file.path("Figures", "Mean Effects", "All.pdf"), 
       width = 6, height = 5, units = "in")

bind_rows(list(ind = MI, pop = MP, tot = MT, div = MD), .id = "scale") %>%
  group_by(scale, var) %>%
  summarise(all = sum(sig == TRUE)/n(), 
            pos = sum(sig == TRUE & median > 0)/n(), 
            neg = sum(sig == TRUE & median < 0)/n())

# Species-Specific Effects ------------------------------------------------

bind_rows(list(ind = SI, pop = SP), .id = "scale") %>%
  mutate(sig = lower > 0 | upper < 0) %>%
  group_by(scale, var) %>%
  summarise(all = sum(sig == TRUE)/n(), 
            pos = sum(sig == TRUE & median > 0)/n(), 
            neg = sum(sig == TRUE & median < 0)/n())

## Individual -------------------------------------------------------------

for(i in unique(SI$site[which(SI$var == "temp")])){
  p <- SI %>%
    filter(var == "temp" & site == i) %>%
    ggplot(aes(x = median)) + 
    geom_histogram() + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    theme_classic(base_size = 15) + 
    ggtitle(i) + xlab("Median Estimated Effect on Size") + ylab("Frequency")
  plot(p)
  ggsave(file.path("Figures", "Species Effect Histograms", 
                   "Individual", "Temperature", 
                   paste("Individual_Histogram_", i,
                         ".pdf", sep = "")), 
         width = 6, height = 5, units = "in")
}

for(i in unique(SI$site[which(SI$var == "DO")])){
  p <- SI %>%
    filter(var == "DO" & site == i) %>%
    ggplot(aes(x = median)) + 
    geom_histogram() + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    theme_classic(base_size = 15) + 
    ggtitle(i) + xlab("Median Estimated Effect on Size") + ylab("Frequency")
  plot(p)
  ggsave(file.path("Figures", "Species Effect Histograms", 
                   "Individual", "DO", 
                   paste("Individual_Histogram_", i,
                         ".pdf", sep = "")), 
         width = 6, height = 5, units = "in")
}

## Population -------------------------------------------------------------

for(i in unique(SP$site[which(SP$var == "temp")])){
  p <- SP %>%
    filter(var == "temp" & site == i) %>%
    ggplot(aes(x = median)) + 
    geom_histogram() + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    theme_classic(base_size = 15) + 
    ggtitle(i) + xlab("Median Estimated Effect on Size") + ylab("Frequency")
  plot(p)
  ggsave(file.path("Figures", "Species Effect Histograms", 
                   "Population", "Temperature", 
                   paste("Population_Histogram_", i,
                         ".pdf", sep = "")), 
         width = 6, height = 5, units = "in")
}

for(i in unique(SP$site[which(SP$var == "DO")])){
  p <- SP %>%
    filter(var == "DO" & site == i) %>%
    ggplot(aes(x = median)) + 
    geom_histogram() + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    theme_classic(base_size = 15) + 
    ggtitle(i) + xlab("Median Estimated Effect on Size") + ylab("Frequency")
  plot(p)
  ggsave(file.path("Figures", "Species Effect Histograms", 
                   "Population", "DO", 
                   paste("Population_Histogram_", i,
                         ".pdf", sep = "")), 
         width = 6, height = 5, units = "in")
}

# Effect Size Comparisons -------------------------------------------------

## Full Plot --------------------------------------------------------------

merged_df <- merge(MI[,c("site", "var", "median")], 
      MP[,c("site", "var", "median")], 
      by = c("site", "var")) %>%
  merge(., MD[,c("site", "var", "median")],  by = c("site", "var")) %>%
  merge(., MT[,c("site", "var", "median")],  by = c("site", "var"))
names(merged_df) <- c("Site", "Var.", "Ind.", "Pop.", "Div.", "Tot.")

lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, ...)
  p
}

ggpairs(merged_df[,-1], aes(color = Var.), 
        lower = list(continuous = wrap(lowerFn))) + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_fill_manual(name = "Variable", labels = c("DO", "Temp."), 
                   values = c("#48A9A6", "#D4B484")) + 
  theme_classic(base_size = 15)

ggsave(file.path("Figures", "Corr", "Full_Corr.pdf"), 
       width = 8, height = 7, units = "in")


## Ind v.s. Pop -----------------------------------------------------------

merge(SI, SP, by = c("s", "site", "var"),
      suffixes = c(".ind", ".pop")) %>%
  ggplot(aes(x = median.ind, y = median.pop,
             color = var, fill = var)) + 
  geom_point(size = 2, alpha = .5) + 
  theme_classic(base_size = 15) + xlab("Effects on Size") + 
  ylab("Effects on CPUE") + 
  geom_smooth(method = 'lm',
              fullrange = TRUE, linetype = "dashed") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_fill_manual(name = "Variable", labels = c("DO", "Temp."), 
                    values = c("#48A9A6", "#D4B484"))

ggsave(file.path("Figures", "Corr", "Ind_vs_PopS.pdf"), 
       width = 6, height = 5, units = "in")

merge(MI, MP, by = c("site", "var"),
      suffixes = c(".ind", ".pop")) %>%
  ggplot(aes(x = median.ind, y = median.pop,
             color = var, fill = var)) + 
  geom_point(size = 2, alpha = .5) + 
  theme_classic(base_size = 15) + xlab("Effects on Size") + 
  ylab("Effects on CPUE") + 
  geom_smooth(method = 'lm',
              fullrange = TRUE, linetype = "dashed") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_fill_manual(name = "Variable", labels = c("DO", "Temp."), 
                    values = c("#48A9A6", "#D4B484"))

ggsave(file.path("Figures", "Corr", "Ind_vs_PopM.pdf"), 
       width = 6, height = 5, units = "in")

## Pop v.s. Div -----------------------------------------------------------

merge(MP, MD, by = c("site", "var"), 
      suffixes = c(".pop", ".div")) %>%
  ggplot(aes(x = median.pop, y = median.div,
             color = var, fill = var)) + 
  geom_point(size = 2, alpha = .5) + 
  theme_classic(base_size = 15) + xlab("Effects on CPUE") + 
  ylab("Effects on Diversity") + 
  geom_smooth(method = 'lm', 
              fullrange = TRUE, linetype = "dashed") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_fill_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484"))

ggsave(file.path("Figures", "Corr", "Pop_vs_Div.pdf"), 
       width = 6, height = 5, units = "in")

## Pop v.s. Tot -----------------------------------------------------------

merge(MP, MT, by = c("site", "var"), 
      suffixes = c(".pop", ".tot")) %>%
  ggplot(aes(x = median.pop, y = median.tot,
             color = var, fill = var)) +  
  geom_point(size = 2, alpha = .5) + 
  theme_classic(base_size = 15) + xlab("Effects on CPUE") + 
  ylab("Effects on Total CPUE") + 
  geom_smooth(method = 'lm',
              fullrange = TRUE, linetype = "dashed") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_fill_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484"))

ggsave(file.path("Figures", "Corr", "Pop_vs_Tot.pdf"), 
       width = 6, height = 5, units = "in")

## Div v.s. Tot -----------------------------------------------------------

merge(MD, MT, by = c("site", "var"), 
      suffixes = c(".div", ".tot")) %>%
  ggplot(aes(x = median.div, y = median.tot,
             color = var, fill = var)) + 
  geom_point(size = 2, alpha = .5) + 
  theme_classic(base_size = 15) + xlab("Effects on Diversity") + 
  ylab("Effects on Total CPUE") + 
  geom_smooth(method = 'lm',
              fullrange = TRUE, linetype = "dashed") + 
  scale_color_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484")) + 
  scale_fill_manual(name = "Variable", labels = c("DO", "Temp."), 
                     values = c("#48A9A6", "#D4B484"))

ggsave(file.path("Figures", "Corr", "Div_vs_Tot.pdf"), 
       width = 6, height = 5, units = "in")

