# ################################################################################
# Title: Seleting a subset of BirdNet classifications to mannually validate 
# BirdNet R workflow, Script 4 
# Author: Will Harrod
# Date Created: 2026-05-25
################################################################################

################################################################################
# 1) Prep ######################################################################
################################################################################

# 1.1) Add packages and data ---------------------------------------------------

# Clear environments
rm(list = ls())

# Install packages 
# install.packages("tidyverse", "fs", "seewave", "tuneR", "warbleR")

# Load Packages
library(tidyverse)
library(fs)
library(warbleR)
library(seewave)
library(tuneR)

# Set Seed
set.seed(27606)

# File directories (Change these for your device) ------------------------------

# Name of the directory that holds your validation data 
bn_dir <- "D:/BirdNet_Output/Full_Site"

# Path to save your new .wav output files 
# output_dir <- "C:/NCSU/Data/BirdNet_Validations"
output_dir <-  "D:/BirdNet_Validations"

# Directory for the summary
csv_dir <-  "C:/Users/wdharrod/Documents/Coding/BirdNet_GUI-master/Data"

# File names (Change these for your device) -----------------------------------

# Name of your output validation file 
output_summary_file <- "birdnet_validations.csv"

# Birdnet classification csv (output from previous script) 
birdnet_file <- "preliminary_BirdNET_Results.csv"

# ARU metadata (locations and other info for your ARUs)
metadata_file <- "aru_locations.csv"

# ------------------------------------------------------------------------------

# set a minimum confidence threshold (can change or keep ar BirdNET Minimum) ----
min_conf <- 0.25

# Add the classif data
classif_raw <- read.csv(path(bn_dir, birdnet_file)) %>% select(-X, -X.1) 
# View
glimpse(classif_raw)

# Clean the classification Data
classif <-  classif_raw %>% 
  ###### COMMENT OUT LATER ##############
  # Only look at Birds 
  filter(class %in% c("Aves", "Anthropogenic")) |> 
  ##########################################
  # Arrange by AUR ID then by date then by time
  arrange(Plot, Date, Rec.Hour, Rec.Min, Start.sec) %>% 
  # Convert date to a date
  mutate(Date = ymd(Date)) %>%
  # Remove low confidence scores
  filter(Confidence >= min_conf) |> 
  # Convert from Linux to Windows 
  mutate(File = str_replace(File, "/media/will/Timbermill", "D:")) |> 
  mutate(File = str_replace(File, "Interior_Forest/", "")) |> 
  mutate(File = str_replace(File, "Reference_Edge/", "")) |> 
  mutate(File = str_replace(File, "Turbines_01_08/", "")) |>  
  mutate(File = str_replace(File, "Turbines_09_16/", "")) |> 
  mutate(File = str_remove(File, "_D3"),
         BirdNet.Name = str_replace(basename(File), "-", "_")) 
# select(-X) 
glimpse(classif)

# How many species?
classif %>%
  count(Species) %>% 
  arrange(-n)

# Select the number of species with at least that many recordings (can comment out) ----
species_list <- classif %>% 
  count(Species) %>%
  arrange(Species) %>% 
  pull(Species)

# ...Or do this manually (can comment out) ----
# species_list <- c(
#   "Pine Warbler", 
#   "Engine", 
#   "Pickerel Frog", 
#   "Field Sparrow", 
#   "Brimley's Chorus Frog",
#   "Common Yellowthroat",
#   "Hooded Warbler",
#   "Swainson's Warbler",
#   "Chuck-will's-widow",
#   "Spring Peeper"
#   )

# View
print(species_list)

# Filter the whole dataset to only include those species 
classif_common <- classif %>% 
  filter(Species %in% species_list) %>% 
  # (Optional) Remove specific species
  filter(!Species %in% c("Song Sparrow", "Meadowlark")) 
# View
glimpse(classif_common)

# Add in the ARU metadata 
aru_info <- read.csv(path("Data", metadata_file))
glimpse(aru_info)

# 1.2) Prepare data for stratified random sampling -----------------------------

# # View confidence scores by species (comment out since this plot takes a while to make)
# classif_common %>%
#   ggplot() +
#   geom_histogram(aes(x = Confidence), col = "lightblue", fill = "lightblue") +
#   scale_x_continuous(limits = c(min_conf, 1)) +
#   theme_classic() +
#   facet_wrap(~Species)

# Define confidence bin breaks
conf_breaks <- c(0.5, 0.75, 1)

# Define weights for each break
break_weights <- c(0.25, 0.25, 0.5)

# Number of breaks
nbreaks <- length(conf_breaks)
nbreaks 

# add a column for confidence breaks 
classif_weighted <- classif_common %>% 
  # Break weights
  mutate(Weight = case_when(Confidence <= conf_breaks[1] ~ break_weights[1],
                            Confidence > conf_breaks[1] & Confidence <= conf_breaks[2] ~ break_weights[2],
                            Confidence > conf_breaks[2] ~ break_weights[3],
                            TRUE ~ NA))

# View
count(classif_weighted, Species, Weight)
glimpse(classif_weighted)

################################################################################################
# 2) Select random stratified rows to validate from across difference confidence levels
################################################################################################

# 2.1) Make folders to store the recordings to validate ------------------------

# Create directories for each species ----
# NOTE: This will erase all audio files currently in the there ----
for(s in 1:length(species_list)){

  # Define a species
  species_file <- species_list[s]
  
  # Transform the species name into a file name
  species_file <- str_remove_all(species_file, "'")
  species_file <- str_replace_all(species_file, " ", "_")
  species_file <- str_replace_all(species_file, "-", "_")
  
  # Path to the folder containing that species audio
  species_folder <- paste0(output_dir, "/", species_file)
  
  # Clear the previous version of the folder 
  if (dir.exists(species_folder)) {
    unlink(species_folder, recursive = TRUE)
  }
  
  # Create a new folder for each species
  dir.create(path = species_folder, recursive = TRUE)
}

# 2.2) Prepare to subsample ----------------------------------------------------

# Define how many birds I want to validate 
n_sample <- 150

# Define the rows to validate
class_to_valid <- classif_weighted %>%
  # Arrange in a useful order
  arrange(Plot, Date, Rec.Hour, Rec.Min, Start.sec) %>% 
  # Add a column for whether or not each row is a true positive
  mutate(True.Positive = NA,
         New.File = NA) %>%
  # Prepare date and time for the file reader
  mutate(Date = str_remove_all(string =  as.character(Date), pattern = "-")) %>%
  # Group  by species and site
  group_by(Species, Plot) %>%
  # Select random classifications to validate
  slice_sample(n = n_sample, replace = FALSE, weight_by = Weight) %>%
  ungroup() %>% 
  # Resample down to the same number for each species
  group_by(Species) %>% 
  slice_sample(n = n_sample, replace = FALSE) %>% 
  ungroup() %>% 
  select(-Weight)

# Number of validations per species and ARU
class_to_valid %>%
  count(Species, Plot) %>%
  arrange(Species, Plot) %>%
  print(n = Inf)

# Number of validations by species 
class_to_valid %>%
  count(Species) %>%
  print(n = Inf)

# Total number to validate
nrow(class_to_valid)

# Number of sites with at least one validations by species
class_to_valid %>%
  count(Plot, Species) %>%
  count(Species) %>%
  print(n = Inf)

# Start a species counter
species_count <- 0

#Define the number of species 
n_species <- length(species_list)

#View
glimpse(class_to_valid)

# 2.3) Subsampling loop --------------------------------------------------------
  
# Loop over the files I need to validate and convert them to wav
for(i in 1:nrow(class_to_valid)){

  # Define and reset a counter for numbered files from the same recording
  rec_count <- 1
  rec_count_pad <- str_pad(rec_count, width = 3, pad = "0")
  
  # pull out some information from the recording
  aru_id <- class_to_valid$Plot[i]
  aru_file <- class_to_valid$File[i]
  conf_val_tmp <- as.character(round(class_to_valid$Confidence[i], 2))
  conf_val <- ifelse(str_length(conf_val_tmp) < 2, paste0(conf_val_tmp, "0"), conf_val_tmp)
 
  # Start time 
  start_sec_tmp <- class_to_valid$Start.sec[i]
  start_sec <- ifelse(start_sec_tmp == 0, 0, start_sec_tmp - 1)
  # End time (note the 5 second length)
  end_sec_tmp <- start_sec + 5
  end_sec <- ifelse(end_sec_tmp >= 3600, 3600, end_sec_tmp)
  species_file <- class_to_valid$Species[i]
  
  # Transform the species name into a file name
  species_file <- str_remove_all(species_file, "'")
  species_file <- str_replace_all(species_file, " ", "_")
  species_file <- str_replace_all(species_file, "-", "_")
  
  # Same with the ARU name
  aru_id <- str_remove(basename(aru_file), "\\.wav$")
  aru_id <- str_replace(aru_id, "-", "_") 

 # Read in the audio file
  wav <- readWave(aru_file,
                  from = start_sec,
                  to = end_sec,
                  units = "seconds",
                  header = FALSE)
  
  # Rename the file 
  new_wav_name <- paste0(conf_val, "_",
                         aru_id, 
                         "_", 
                         rec_count_pad,
                         ".wav")
  
  # Output destination for the new wav
  new_wav_path <- path(output_dir, species_file, new_wav_name)
  
  # Check if that file exists 
  while(file.exists(new_wav_path)) {
    # Increase the recording counter until it's a new file name
    rec_count <- rec_count + 1
    rec_count_pad <- str_pad(rec_count, width = 3, pad = "0")
    # Rename the file
    new_wav_path <- path(output_dir, species_file, 
                         paste0(conf_val, "_",
                                aru_id, "_", 
                                rec_count_pad, "_",
                                ".wav"))
  }       
  
  # Save the new wav
  writeWave(wav, new_wav_path)
  
  # Link the name of the new file to the data
  class_to_valid$New.File[i] <- new_wav_path
  
  # After finishing each species 
  if(i %% n_sample == 0){
    # Increase the species counter 
    species_count <- species_count + 1
    # Reset the recording counter 
    # Let me know
    message("Created 5s recordings to validate for ", class_to_valid$Species[i], 
            " 🐦🐸. Species ", species_count, " out of ", n_species)
  }
  
}

# 2.5) View and save reesults --------------------------------------------------

# View 
glimpse(class_to_valid)
class_to_valid |> count(Species)
  

# Save as a .csv
class_to_valid %>%
  arrange(Species) %>%
  rowid_to_column() %>%
  # Save as a csv
  write.csv(path(csv_dir, output_summary_file), 
            row.names = FALSE)
# Done :)
