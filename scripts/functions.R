
#intermediate.names() -----
intermediate.names <- function() {
  return("Required column names: DATE, SUBSITE, SP_CODE, SCI_NAME, COMMON_NAME, SIZE, YEAR, EFFORT, mean_daily_DO, mean_min_DO, annual_avg_DO, mean_daily_temp, mean_max_temp, mean_min_temp")
  
}

#intermediate.prep ------

intermediate.prep <- function(intermediate) {
  # Define the required columns
  required_columns <- c("DATE", "SUBSITE", "SP_CODE", "SCI_NAME", "COMMON_NAME", "SIZE", "YEAR", "EFFORT", "mean_daily_DO", "mean_min_DO", "annual_avg_DO", "mean_daily_temp", "mean_max_temp", "mean_min_temp")
  
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

neon_download <- function(site, dpID, dataset, data_type) {
  # Define the main savepath for the site
  savepath <- file.path("data", "raw_data", dataset)
  
  # Create a subfolder for the specific dataset type (e.g., "fish" or "enviro")
  dataset_folder <- file.path(savepath, data_type)
  
  # Check if the dataset subfolder exists
  if (!dir.exists(dataset_folder)) {
    # Create the subfolder if it doesn't exist
    dir.create(dataset_folder, recursive = TRUE)
  }
  
  # Check if there's a subfolder starting with "files"
  files_folder <- list.dirs(dataset_folder, full.names = TRUE, recursive = FALSE)
  
  if (length(files_folder) == 0) {
    # Proceed with the download if no other subfolders exist
    zipsByProduct(
      dpID = dpID,
      site = site,
      package = "basic",
      check.size = TRUE,
      include.provisional = FALSE,
      savepath = dataset_folder
    )
    message("Download complete for ", site, " (", data_type, ")")
  } 
  else {
    # If files exist, delete them and proceed with the download
    unlink(files_folder, recursive = TRUE)  # Delete the existing files/folders
    zipsByProduct(
      dpID = dpID,
      site = site,
      package = "basic",
      check.size = TRUE,
      include.provisional = FALSE,
      savepath = dataset_folder
    )
    message("Existing data for ", site, " (", data_type, ") overwritten. Download complete.")
  }
}
  

#stack NEON ------

neon_stack <- function(dataset, folder, data_type) {
  # Define the main savepath using dataset
  savepath <- file.path("data", "raw_data", dataset)
  
  # Create a subfolder for the specific data_type (e.g., "fish" or "enviro")
  dataset_folder <- file.path(savepath, data_type, folder)
  
  # Check if there are any .zip files within the folder
  zip_files <- list.files(
    path = dataset_folder, 
    pattern = "\\.zip$", 
    full.names = TRUE
  )
  
  if (length(zip_files) != 0) {
    # Proceed with the stack if there are .zip files
    stackByTable(filepath = dataset_folder)
    message("Stacking complete for ", dataset, " (", data_type, ")")
  } 
  else {
    message("Stacking for ", dataset, " (", data_type, ") already completed. Skipping stack.")
  }
}

