##Google Drive tutorial walkthrough ------

## https://lter.github.io/scicomp/tutorial_googledrive-pkg.html


# Install packages
# install.packages(c("googledrive", "httpuv"))

# Load them
library(googledrive)

#enter email address here

googledrive::drive_auth(email = "caseallie16@gmail.com")

neon_hopb_zip <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1hWuEzA-QJgNFuSZXZjSL6jtaUjBbBHNZ")) %>% 
  dplyr::filter(name == "NEON_count-fish.zip")


#our google drive link: https://drive.google.com/drive/u/0/folders/0AAQ1XOtdPVI_Uk9PVA

googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AAQ1XOtdPVI_Uk9PVA"))
