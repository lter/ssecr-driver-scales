#_________________________________
#Google shareed drive project setup
# SCALES/ SSECR                  
# Allie Case   
# R Version: 4.4.2 (2024-10-31) -- "Pile of Leaves"
#_________________________________

# setup ---------------------

#need to define directory structure 

shared_drive_id <- "0AAQ1XOtdPVI_Uk9PVA"

# Create top-level "data" folder in the shared drive
data_folder <- drive_mkdir("data", path = as_id(shared_drive_id))

dirs <- c(
  file.path("data", "metadata"),
  file.path("data", "raw_data"),
  file.path("data", "intermediate_data"),
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

create_nested_folders <- function(dirs, shared_drive_id) {
  folder_map <- list()  # Store folder IDs
  new_folders <- c()  # Track newly created folders
  
  # Ensure the "data" folder exists first
  data_folder <- drive_ls(path = as_id(shared_drive_id), pattern = "^data$", type = "folder")
  if (nrow(data_folder) == 0) {
    data_folder <- drive_mkdir("data", path = as_id(shared_drive_id))
    new_folders <- c(new_folders, "data")
  } else {
    data_folder <- data_folder[1, ]  # Use existing folder
  }
  folder_map[["data"]] <- data_folder$id
  
  # Iterate through the directories
  for (dir in dirs) {
    parent <- dirname(dir)
    folder_name <- basename(dir)
    
    # Get the parent folder ID
    parent_id <- folder_map[[parent]]
    
    # Check if the folder already exists
    existing_folder <- drive_ls(path = as_id(parent_id), pattern = paste0("^", folder_name, "$"), type = "folder")
    
    if (nrow(existing_folder) == 0) {
      # Folder does not exist, create it
      new_folder <- drive_mkdir(folder_name, path = as_id(parent_id))
      folder_map[[dir]] <- new_folder$id
      new_folders <- c(new_folders, dir)
    } else {
      # Folder exists, store its ID
      folder_map[[dir]] <- existing_folder$id
    }
  }
  
  # Return message
  if (length(new_folders) > 0) {
    message("Added directories: ", paste(new_folders, collapse = ", "))
  } else {
    message("No directories added")
  }
}

create_nested_folders(dirs = dirs, shared_drive_id = shared_drive_id)

