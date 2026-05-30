################################################################################
# Title: Subsetting BirdNET
# BirdNet R workflow, Script 1 of 5
# Author: Will Harrod
# Date Created: 2026-05-01
################################################################################

################################################################################
# 1) Preparation ###############################################################
################################################################################

# Clear environments
rm(list = ls())

# Load packages
library(tidyverse)
library(birdnetR)
library(tuneR)
library(fs)

# Set Seed
set.seed(27606)

# Path to wd
wd <- "D:/Bird_Anuran_ARUs"

# Set wd
setwd(wd)

################################################################################
# 2) Inspect data ##############################################################
################################################################################

# 2.1) Recordings by site ------------------------------------------------------

# Find all the ARU folders
aru_dirs <- list.dirs(wd)
aru_dirs <- aru_dirs[2:length(aru_dirs)]

# How many ARUs?
length(aru_dirs)

# View the ARUs
aru_dirs

# Find the number of recordings made by each ARU
rec_counts <- aru_dirs %>% 
  set_names() %>% 
  map_int(~length(list.files(.x))) %>% 
  enframe(name = "path", value = "rec.count") %>% 
  mutate(site.id = str_extract(path, "[A-Z]{2}\\d{2}")) %>% 
  select(path, site.id, rec.count)


# View the counts by ARU
print(rec_counts, n = Inf)

# 2.2) days per recording ------------------------------------------------------

# Add storage columns to the tibble
rec_counts <- rec_counts %>% 
  mutate(start.date = as.Date(NA),
         end.date = as.Date(NA),
         total.days = NA)

# Loop through the files and count he first, last, and total days 
for(i in 1:nrow(rec_counts)){

  # Define an ARU
  aru <- rec_counts$path[i]
  
  # List the recordings made by that ARU
  recordings <- list.files(aru)
  
  # Isolate the days 
  days <- str_extract(recordings, "_\\d{8}_") %>% 
    str_remove_all("_") %>% 
    ymd()
  
  # Find the first and last dates
  rec_counts$start.date[i] <- ymd(min(days))
  rec_counts$end.date[i] <- ymd(max(days))
  rec_counts$total.days[i] <- max(days) - min(days) + 1
  
}

# View the result
print(rec_counts, n = Inf)

# which recorder had the fewest days?
min_days_dat <- rec_counts %>% 
  filter(total.days == min(rec_counts$total.days)) 
min_days_dat


################################################################################
# 3) Subset data ###############################################################
################################################################################

# Path to wd
wd <- "D:"

# Set wd
setwd(wd)

# List the minimum days 
min_days <- min_days_dat$total.days
min_days

# Create a folder for subsetted data
out_dir <- "D:/Subsetted_ARU_Data"
dir_create(out_dir, recurse = TRUE)

# How many ARUs?
n_arus <- nrow(rec_counts)

# Loop through the recordings and randomly sample based on the fewest number of days 
for(i in 1:n_arus){ 

  # Define an ARU
  aru_path <- rec_counts$path[i]
  aru <- rec_counts$site.id[i]
  
  # Create a new folder for that aru
  dir_create(paste0("D:/Subsetted_ARU_Data/", aru), recurse = TRUE)
  
  # List the recordings made by that ARU
  recordings <- tibble(audio.files = list.files(aru_path))
  
  # Isolate the days 
  recordings <- recordings %>% 
    mutate(day = str_extract(audio.files, "_\\d{8}_")) %>% 
    mutate(day = str_remove_all(day, "_"))
  
  # Find the unique days 
  unq_days <- recordings %>% 
    distinct(day) 

  # subset a few of days 
  slct_days <- unq_days %>% 
    slice_sample(n = min_days) %>% 
    pull(day)
  
  # List the recordings that took place during those days
  slct_recs <- recordings %>% 
    filter(day %in% slct_days) %>% 
    pull(audio.files)
  
  # List all of the files
  all_wavs <- dir_ls(
    path = aru_path,
    recurse = TRUE,
    regexp = "\\.[Ww][Aa][Vv]$",
    type = "file"
  )
  
  # Filter the ones that were selected 
  slct_wavs <- all_wavs[basename(all_wavs) %in% slct_recs]
  
 # Copy those files 
  file_copy(
    path = slct_wavs,
    new_path = path(paste0("D:/Subsetted_ARU_Data/", aru), basename(slct_wavs)),
    overwrite = TRUE
  )
  
} # End loop over the ARUs
