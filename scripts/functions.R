
#intermediate.names() -----
intermediate.names <- function() {
  return("Required column names: DATE, SUBSITE, SP_CODE, SCI_NAME, COMMON_NAME, SIZE, AREA")
  
}

#intermediate.prep ------

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

#intermediate.directories -----

intermediate.directories <- function() {
  # Define all directory paths
  dirs <- c(
    file.path("data", "metadata"),
    file.path("data", "raw_data"),
    file.path("data", "raw_data", "LTER_SBC"),
    file.path("data", "raw_data", "NEON_WALK"),
    file.path("data", "raw_data", "NEON_PRPO"),
    file.path("data", "raw_data", "IEP_LODI"),
    file.path("data", "raw_data", "LTER_NTL"),
    file.path("data", "raw_data", "LTER_ARC"),
    file.path("data", "raw_data", "IEP_YOLO"),
    file.path("data", "raw_data", "LTER_MCR"),
    file.path("data", "raw_data", "LTER_VCR"),
    file.path("data", "raw_data", "NEON_CRAM"),
    file.path("data", "raw_data", "NEON_HOPB"),
    file.path("data", "raw_data", "NEON_MAYF"),
    file.path("data", "raw_data", "NEON_POSE"),
    file.path("data", "raw_data", "NEON_ARIK"),
    file.path("data", "raw_data", "NEON_CARI"),
    file.path("data", "raw_data", "NEON_KING"),
    file.path("data", "raw_data", "NEON_LECO"),
    file.path("data", "raw_data", "NEON_LEWI"),
    file.path("data", "raw_data", "NEON_MCDI"),
    file.path("data", "raw_data", "NEON_PRIN"),
    file.path("data", "raw_data", "NEON_PRLA"),
    file.path("data", "raw_data", "NEON_PRFO"),
    file.path("data", "raw_data", "NEON_CUPE"),
    file.path("data", "raw_data", "NEON_GUIL"),
    file.path("data", "raw_data", "NEON_LIRO"),
    file.path("data", "raw_data", "NEON_TOOK"),
    file.path("data", "raw_data", "NEON_WLOU")
  )
  
  # Track if any directory is created
  created_any <- FALSE
  
  # Check and create directories if needed
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir)
      message(paste("Directory", dir, "created."))
      created_any <- TRUE
    }
  }
  
  # If no directories were created, print a summary message
  if (!created_any) {
    message("All directories already exist.")
  }
}


#time series ------

timeseries <- function(x){length(unique(x))} #function can can count unique years for each station. Easiest way is to use it with tapply(). Ex:

#tapply(data$YEAR, list(data$SITE), timeseries)

#download NEON -----

neon_download <- neon_download <- function(site, dpID, dataset) {
  savepath <- file.path("data", "raw_data", dataset)
  
  # Check if there's a subfolder starting with "files"
  files_folder <- list.dirs(savepath, full.names = TRUE, recursive = FALSE)
  
  if (length(files_folder) == 0) {
    # Proceed with the download if no other subfolders exist
    zipsByProduct(
      dpID = dpID,
      site = site,
      package = "basic",
      check.size = TRUE,
      include.provisional = FALSE,
      savepath = savepath
    )
    message("Download complete for ", site)
  } 
  else {
    message("Data for ", site, " already exists. Skipping download.")
  }
}
  

#stack NEON ------

#download NEON -----

neon_stack <- function(folder) {
  savepath <- file.path("data", "raw_data", dataset, folder)
  
  # Check if there are any .zip files within subfolders
  zip_files <- list.files(
    path = savepath, 
    pattern = "\\.zip$")
  
  if (length(zip_files) != 0) {
    # Proceed with the stack if there are .zip files
    stackByTable(filepath = file.path("data", 
                                      "raw_data", 
                                      dataset, 
                                      folder, sep = ""))
    message("Stacking complete for ", dataset)
  } 
  else {
    message("Stacking for ", dataset, " already completed. Skipping stack.")
  }
}
