# ################################################################################
# Title: BirdNET post-validation processing
# BirdNet R workflow, Script 5 of 5
# Author: Will Harrod
# Date Created: 2026-05-29
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

# 1.2) File directories (Change these for your device) -------------------------

# Directory where the BirdNet validations are housed
bn_dir <- "C:/NCSU/Data/BirdNet_Validations"

# Directory where the validation summaries are held
csv_dir <-  "Data"

# 1.3) Add data  and make a species lists --------------------------------------

# Add the birdnet validation summary data 
vld_sum <-  read_csv(path(csv_dir, "birdnet_validations.csv"))
# View
glimpse(vld_sum)

# View the species
vld_sum %>% count(Common.name) %>% print(n = Inf)

# Make a list of species that were validated
vld_sp <- c(
  "Black-and-white Warbler",
  "Brown-headed Nuthatch",
  "Brimley's Chorus Frog",
  "Common Yellowthroat",
  "Eastern Towhee",
  "Field Sparrow",
  "Hooded Warbler",
  # "Pickerel Frog",
  "Pine Warbler",
  "Ovenbird",
  "Southern Toad",
  "Spring Peeper",
  "White-eyed Vireo"
  # "Wild Turkey"
  )
# Or use all species (comment out)
# vld_sp <- vld_sum %>% distinct(Common.name) %>% pull(Common.name)

# Number of species
n_species <- length(vld_sp)

# Select only those species 
vld_sum_sp <-  vld_sum %>% 
  filter(Common.name %in% vld_sp) %>% 
  # Add a file basename column
  mutate(Base.Name = basename(New.File),
         True.Positive = NA_integer_) 

# View
glimpse(vld_sum_sp)
vld_sum_sp %>% count(Common.name)

################################################################################
# 2) Clean and combine data ####################################################
################################################################################

# 2.2) Prepare validation data  ------------------------------------------------

# Make a blank version for the loop
vld_tbl <- vld_sum_sp %>% 
  slice_head(n = 0)
# View
glimpse(vld_tbl)

# Convert species names to the file names 
sp_file <- str_remove_all(vld_sp, "'")
sp_file <- str_replace_all(sp_file, " ", "_")
sp_file <- str_replace_all(sp_file, "-", "_")
sp_file

# 2.2) Get the validation results from the BirdNet auto-generated folders ------

# Extract the true and false positive info for each species
for(i in 1:n_species){ # Start loop

# Pick a species
sgl_sp <-  vld_sp[i]
sgl_sp_file <- sp_file[i]
sgl_vld_tbl <- vld_sum_sp %>% filter(Common.name == sgl_sp)

# List the true and false positive paths 
f_pos_dir <- path(bn_dir, sgl_sp_file, "Negative")
t_pos_dir <- path(bn_dir, sgl_sp_file, "Positive")

# Find the false positive recordings 
if(dir_exists(f_pos_dir)){
  f_pos <- tibble(Base.Name = basename(dir_ls(f_pos_dir)),
                      True.Positive = 0)
} else{
  f_pos <- tibble(Base.Name  = NA_character_,
                  True.Positive = NA_integer_)
}

# Find the true positive recordings 
if(dir_exists(t_pos_dir)){
  t_pos <-  tibble(Base.Name = basename(dir_ls(t_pos_dir)),
                    True.Positive = 1)
} else{
  t_pos <- tibble(Base.Name = NA_character_,
                  True.Positive = NA_integer_)
}

# Combine the true and false results 
vld_results <- bind_rows(t_pos, f_pos) %>% 
  drop_na()

# Join these results with the main validation table
sgl_vld_tbl <- sgl_vld_tbl %>% 
  rows_update(vld_results, by = "Base.Name", unmatched = "ignore") 

# Combine with other species
vld_tbl <- vld_tbl %>% 
  bind_rows(sgl_vld_tbl)
} # End loop

# View
glimpse(vld_tbl)

# Save the results
write.csv(vld_tbl, path(csv_dir, "birdnet_validations_anotated.csv"), row.names = FALSE)

################################################################################
# 3) View validations results ##################################################
################################################################################

# Read the annotated validation results back in
vld_tbl <-  read.csv(path(csv_dir, "birdnet_validations_anotated.csv"))

# View again
glimpse(vld_tbl)

# Results by species
vld_tbl %>% count(Common.name, True.Positive)

# Drop NA's and convert to 1's and 0's 
vld_tbl <-  vld_tbl %>% 
  drop_na(True.Positive)
glimpse(vld_tbl)

# Plot distributions for true and false positives by confidancee score
conf_dist_plot <- vld_tbl %>%
  mutate(True.Positive = factor(True.Positive, levels = c(0, 1))) %>% 
  ggplot() +
  geom_density(aes(x = Confidence, fill = True.Positive), alpha = 0.5) +
  scale_fill_manual(values = c("firebrick4", "steelblue4")) +
  theme_classic() +
  facet_wrap(~Common.name)
# View
conf_dist_plot


################################################################################
# 4) Logistic regression to determine an occupancy threshold ###################
################################################################################

# Define an acceptable true positive probability
p <- 0.95 

# Define classifier sensitivity (not sure what this is yet)
sensitivity <- 1

# Minimum BirdNET Confidence score
min_conf <- 0.25

# Transform confidence scores back to the logit scale
vld_tbl <- vld_tbl %>% 
  mutate(
    Confidence_Clean = pmin(pmax(Confidence, 0.0001), 0.9999),
    logit.Conf.Score = log(Confidence_Clean / (1 - Confidence_Clean)) / sensitivity
    )
# View
glimpse(vld_tbl)
hist(vld_tbl$logit.Conf.Score)

# Make an object to store the species and appropriate thresholds 
log_reg_tbl <- tibble(Common.name = vld_sp,
                      Threshold = NA)
# View
log_reg_tbl

# Run the logistic regression for each species
for(i in 1:n_species){

# Pick a single species
sgl_sp <- vld_sp[i] 
sgl_vld_tbl <- vld_tbl %>%
  select(Common.name, True.Positive, logit.Conf.Score) %>% 
  filter(Common.name == sgl_sp)

# Run the logistic regression
log_reg <- glm(True.Positive ~ logit.Conf.Score, family = 'binomial', data = sgl_vld_tbl)

# Summarize the model
log_reg_sum <- broom::tidy(log_reg)

# Extract the intercept and slope
Beta0 <-  log_reg_sum$estimate[1]
Beta.Conf <- log_reg_sum$estimate[2]

# Calculate the threashold for that species 
logit_threshold <- (log(p/(1 - p))-Beta0) / Beta.Conf

# Convert back to the confidence score scale
conf_threshold <- 1 / (1 + exp(-logit_threshold))

# Add to the table
log_reg_tbl$Threshold[i] <- conf_threshold

}

# View
log_reg_tbl

# Combine 0 and 1 thresholds where 100% of validations were true positives 
log_reg_tbl <- log_reg_tbl %>% 
  mutate(Threshold = case_when(Threshold < min_conf | Threshold > 0.99 ~ min_conf,
                               .default = Threshold))
# View with the changes
log_reg_tbl

# Save 
write.csv(log_reg_tbl, path(csv_dir, "BirdNet_Thresholds.csv"))

################################################################################
# 5) Filter the full dataset based on my thresholds ############################
################################################################################

# Read in the full dataset for the species that I have validated
classif_full <- read.csv(path(csv_dir, "preliminary_BirdNET_Results.csv")) %>% 
  select(-X) %>% 
  filter(Common.name %in% vld_sp) %>% 
  # Join with the thresholds
  left_join(log_reg_tbl, by = "Common.name")

# View the full dataset before applying the threshold
glimpse(classif_full)
classif_full %>% count(Common.name) 

# Apply the filter
classif_thresh <- classif_full %>% 
  filter(Confidence >= Threshold)

# View the changes
glimpse(classif_thresh)
classif_thresh %>% count(Common.name) 

# Save 
write.csv(classif_thresh, path(csv_dir, "Preliminary_BirdNET_Results_Filtered.csv"))
