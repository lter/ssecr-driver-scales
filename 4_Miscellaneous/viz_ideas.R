#viz ideas 

librarian::shelf(supportR, tidyverse, summarytools, 
                 datacleanr, lterdatasampler,
                 cowplot, gt,
                 vegan, lterpalettefinder)

##Top 5 species at each subsite ------

plot.top5 <- function(data, species_col, subsite = FALSE) {
  
  dataset <- dataset
  
  palette <- c(suppressMessages(palette_find("LTER")[1:4]), suppressMessages(palette_find("LTER")[6]))
  if(subsite) { 
for(i in subsites) {
 
   subsites <- unique(intermediate$SUBSITE)
  
  codes <- intermediate %>%
    mutate(YEAR = year(DATE)) %>%
    filter(SUBSITE == i) %>%
    # Using .data[[species_col]] to refer to the column
    dplyr::count(.data[[species_col]]) %>%
    arrange(desc(n)) %>%
    head(5) %>%
    pull(.data[[species_col]])
  
  temp <- intermediate %>%
    filter(.data[[species_col]] %in% codes) %>%
    mutate(YEAR = year(DATE))
  
  plots <- temp %>%
    filter(SUBSITE == i) %>% 
    group_by(YEAR, .data[[species_col]]) %>%
    summarise(speciesabundance = n()) %>%
    ggplot(aes(
      x = YEAR,
      y = speciesabundance,
      group = .data[[species_col]],
      color = .data[[species_col]]
    )) +
    geom_line(lwd = 1) +
    labs(title = paste0("Top 5 species across time at ", dataset, " ", i)) +
    theme_classic() +
    scale_color_manual(values = palette) +
    labs(color = "Species",
         y = "Abundance",
         x =  "") +
    theme(plot.title = element_text(hjust = 0.5),
               legend.position = "top")
    
  
  print(plots)
  
}
  }
  else{
      
      codes <- intermediate %>%
        mutate(YEAR = year(DATE)) %>%
        dplyr::count(.data[[species_col]]) %>%
        arrange(desc(n)) %>%
        head(5) %>%
        pull(.data[[species_col]])
      
      temp <- intermediate %>%
        filter(.data[[species_col]] %in% codes) %>%
        mutate(YEAR = year(DATE))
      
      plot <- temp %>%
        group_by(YEAR, .data[[species_col]]) %>%
        summarise(speciesabundance = n()) %>%
        ggplot(aes(
          x = YEAR,
          y = speciesabundance,
          group = .data[[species_col]],
          color = .data[[species_col]]
        )) +
        geom_line(lwd = 1) +
        labs(title = paste0("Top 5 species across time at ", dataset)) +
        theme_classic() +
        scale_color_manual(values = palette) +
        labs(color = "Species",
             y = "Abundance",
             x =  "") +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "top")
      
      
      print(plot)
      
      print("Subsite processing skipped. Only singular site plotted")
    }
    
  }

##species presence/absence over time ------

plot.presence <- function(data, species_col, subsite = FALSE) {
  dataset <- dataset
  
  if (subsite) {
    for (i in subsites) {
      subsites <- unique(intermediate$SUBSITE)
      
      plots <- intermediate %>%
        filter(SUBSITE == i) %>%
        mutate(YEAR = year(DATE)) %>%
        group_by(YEAR, .data[[species_col]]) %>%
        distinct(.data[[species_col]]) %>%
        ggplot(aes(
          x = as.factor(YEAR),
          y = .data[[species_col]],
          color = .data[[species_col]]
        )) +
        geom_point(show.legend = F) +
        labs(
          x = "",
          y = "Species Names",
          title = paste0("Species Presence/Absence over time at ", dataset, " ", i)
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      
      
      print(plots)
      
    }
  }
  else{
    {
      plot <- intermediate %>%
        mutate(YEAR = year(DATE)) %>%
        group_by(YEAR, .data[[species_col]]) %>%
        distinct(.data[[species_col]]) %>%
        ggplot(aes(x = as.factor(YEAR), 
                   y = .data[[species_col]], 
                   color = .data[[species_col]])) +
        geom_point(show.legend = F) +
        labs(
          x = "",
          y = "Species Names",
          title = paste0("Species Presence/Absence over time at ", dataset)
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      
      print(plot)
      
      print("Subsite processing skipped. Only singular site plotted")
    }
  }
}

# species accumulation curves -----

plot.speciesaccum <- function(data, species_col, subsite = FALSE) {
  dataset <- dataset
  
  if (subsite) {
    for (i in subsites) {
      subsites <- unique(intermediate$SUBSITE)
      
      temp <- intermediate %>%
        filter(SUBSITE == i) %>%
        mutate(YEAR = year(DATE)) %>%
        group_by(YEAR) %>%
        select(YEAR, .data[[species_col]]) %>%
        distinct(.data[[species_col]]) %>%
        mutate(presence = 1) %>%
        pivot_wider(
          names_from = "COMMON_NAME",
          values_from = presence,
          values_fill = list(presence = 0)
        ) %>%
        arrange(YEAR) %>%
        ungroup()
      
      # Remove the YEAR column for specaccum
      community_matrix <- temp %>% select(-YEAR)
      
      # Run specaccum on the community matrix
      result <- specaccum(community_matrix, method = "collector")
      
      # Add YEAR back to the results
      result.df <- data.frame(Time = result$sites, Species = result$richness) %>%
        mutate(YEAR = temp$YEAR)
      
      result.df
      
      plots <- result.df %>%
        ggplot(aes(x = YEAR, y = Species)) +
        geom_line() +
        labs(
          y = "Species Richness (# of unique species)",
          x = " ",
          title = paste0("Species richness over time at ", dataset, " ", i)
        ) +
        theme_bw()
      
      print(plots)
    }
  }
  else{
    {
      temp <- intermediate %>%
        mutate(YEAR = year(DATE)) %>%
        group_by(YEAR) %>%
        select(YEAR, .data[[species_col]]) %>%
        distinct(.data[[species_col]]) %>%
        mutate(presence = 1) %>%
        pivot_wider(
          names_from = .data[[species_col]],
          values_from = presence,
          values_fill = list(presence = 0)
        ) %>%
        arrange(YEAR) %>%
        ungroup()
      
      # Remove the YEAR column for specaccum
      community_matrix <- temp %>% select(-YEAR)
      
      # Run specaccum on the community matrix
      result <- specaccum(community_matrix, method = "collector")
      
      # Add YEAR back to the results
      result.df <- data.frame(Time = result$sites, Species = result$richness) %>%
        mutate(YEAR = temp$YEAR)
      
      result.df
      
      plot <- result.df %>%
        ggplot(aes(x = YEAR, y = Species)) +
        geom_line() +
        labs(
          y = "Species Richness (# of unique species)",
          x = " ",
          title = paste0("Species richness over time at ", dataset)
        ) +
        theme_bw()
      print(plot)
      
      print("Subsite processing skipped. Only singular site plotted")
    }
  }
}
  

  
