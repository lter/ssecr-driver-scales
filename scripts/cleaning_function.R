
#intermediate.names() -----
intermediate.names <- function() {
  return("Required column names: DATE, SUBSITE, SP_CODE, SCI_NAME, COMMON_NAME, SIZE, AREA")
  
}

#intermediate.prep() ------

intermediate.prep <- function(intermediate) {
  # Define the required columns
  required_columns <- c("DATE", "SUBSITE", "SP_CODE", "SCI_NAME", "COMMON_NAME", "SIZE", "AREA")
  
  # Identify missing columns
  missing_columns <- setdiff(required_columns, names(intermediate))
  
  # Add missing columns and fill with NA using tidyverse methods
  if (length(missing_columns) > 0) {
    # Create a tibble with NA values for missing columns
    na_tibble <- tibble(!!!set_names(
      replicate(length(missing_columns), NA, simplify = FALSE),
      missing_columns
    ))
    
    # Add the new columns using bind_cols
    intermediate <- bind_cols(intermediate, na_tibble)
  }
  
  # Organize columns in the specified order
  intermediate <- intermediate %>%
    select(all_of(required_columns))
  
  # Create the file path
  file_path <- file.path("data", 
                         "raw_data",
                         dataset,
                         paste0(dataset, "_intermediate.csv"))
  
  # Write the dataset to a .csv file
  write.csv(intermediate, 
            file = file_path, 
            row.names = FALSE)
  
  return("You may now add the intermediate .csv to Google Drive")
  
}

#scales.directories

intermediate.directories <- function() {
  # create all potential non-existent directories within scales github project
  
  ifelse(!dir.exists(file.path("data", "metadata")), 
         dir.create(file.path("data", "metadata")), 
         FALSE)
  
  ifelse(!dir.exists(file.path("data", "raw_data")), 
         dir.create(file.path("data", "raw_data")), 
         FALSE)
  
  ifelse(!dir.exists(file.path("data", "raw_data", "LTER_SBC")), 
         dir.create(file.path("data", "raw_data", "LTER_SBC")), 
         FALSE)
}

#timeseries function

timeseries <- function(x){length(unique(x))} #function can can count unique years for each station


