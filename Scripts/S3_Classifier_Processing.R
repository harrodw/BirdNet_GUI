# ################################################################################
# Title: CLeaning BirdNET output
# BirdNet R workflow, Script 3 of 5
# Author: Will Harrod
# Date Created: 2026-05-25
################################################################################

################################################################################
# 1) Prep ######################################################################
################################################################################

# Add packages
library(tidyverse)
library(fs)

# Set Seed
set.seed(27606)

# Define the working directory as the folder that holds the BirdNET csv's
wd <- "C:/NCSU/Disertation_Code/BirdNet_GUI/Data/subsetted_ARU_Data"

# Viw the folders in that working directory
sites <- dir_ls(wd, regexp = "[A-Z]{2}\\d{2}")
sites

# How many sites
n_sites <- length(sites)
n_sites

# View the contents of one folder
bn_results_s1 <- dir_ls(sites[1], regexp = "results.csv")
bn_results_s1

# Read in the first output csv (might be empty)
bn_results <- read.csv(bn_results_s1[1]) %>% 
  tibble() %>% 
  rename(Start.sec = Start..s.,
         End.sec = End..s.) %>% 
  mutate(across(.cols = c(Scientific.name, Common.name, File),
                .fns = ~ as.character(NA))) %>% 
  mutate(across(.cols = c(Start.sec, End.sec, Confidence),
                .fns = ~ as.numeric(NA))) %>% 
  slice_head(n = 1)

# View
bn_results
glimpse(bn_results)

################################################################################
# 2) Data Cleaning ##############################################################
################################################################################

# Loop through the ARU files to combine them all 
for(i in 1:n_sites){

  # Define an ARU
  site <- sites[i]
  
  # List the summary csv's in that folder
  class_sums <- dir_ls(site, regexp = "results.csv")
  
  # Count the number of csv's in that folder
  n_csv <- length(class_sums)
  
  # Progress message 
  message("Combinging classification summaries for aru ", i, 
          " out of ", n_sites, ". ", n_csv, " recordings ")
  
  # Loop through these and combine them 
  for(j in 1:n_csv){
    
    # Define a single summary
    class_sum <- read.csv(class_sums[j]) %>% 
      tibble() %>% 
      rename(Start.sec = Start..s.,
             End.sec = End..s.) %>% 
      mutate(across(.cols = c(Scientific.name, Common.name, File),
                    .fns = ~ as.character(.x))) %>% 
      mutate(across(.cols = c(Start.sec, End.sec, Confidence),
                    .fns = ~ as.numeric(.x))) 
    
    # Combine that with the other recording summaries
    bn_results <- bind_rows(bn_results, class_sum)
    
  }
}

# View the full combined BirdNET classifier output
glimpse(bn_results)
slice_head(bn_results)

# How many distinct species?
bn_results %>% 
  count(Scientific.name) %>% 
  nrow()

# Add in the possible species list 
psb_species <- read.csv("Data/possible_species.csv") %>% 
  tibble() %>% 
  mutate(Scientific.name = paste(genus, species)) %>% 
  rename(Common.name = common_name)
# View
glimpse(psb_species)

# List of anthropogenic sounds
anthro_sounds <- c("Engine", "Gun", "Human non-vocal", "Human vocal", "Power tools")

# Combine with the Birdnet Results
bn_results_taxon <- left_join(bn_results, psb_species, 
                              by = c("Scientific.name", "Common.name")) %>% 
  rename(Latin.name = Scientific.name) %>% 
  mutate(class = case_when(Latin.name == "Dog" ~ "Mammalia",
                           Latin.name %in% anthro_sounds ~ "Anthropogenic",
                           TRUE ~ class),
         order = case_when(Latin.name == "Dog" ~ "Carnivora",
                           Latin.name %in% anthro_sounds ~ "Anthropogenic",
                           TRUE ~ order))  %>% 
  arrange(class, order, Common.name) %>% 
  # Extract information from the file names
  mutate(
         Plot = factor(str_extract(File, pattern = "[A-Z]{2}\\d{2}")),
         Date = ymd(str_extract(File, pattern = "\\d{8}")),
         Time = str_extract(File, pattern = "\\d{6}.wav")
         ) %>% 
  mutate(
         Time = str_remove_all(Time, pattern = ".wav"),       # Raw time
         Plot.Type = factor(str_remove_all(Plot, "\\d{2}"))   # Plot Type
         ) %>% 
  mutate(Rec.Hour = as.numeric(str_sub(Time, start = 1, end = 2)), # Start Hour
         Rec.Min = as.numeric(str_sub(Time, start = 3, end = 4))   # Recording Minute
         ) %>% 
  # Reorder columns and remove unnessesary ones
  select(
         Plot, Plot.Type , Date, Rec.Hour, Rec.Min, Start.sec, End.sec,
         Common.name, Confidence, Latin.name, class, order, File
         )


# View
glimpse(bn_results_taxon)
print(bn_results_taxon, n = 30)

# Save the full results
bn_results_taxon %>% write.csv("Data/preliminary_BirdNET_Results.csv")

# Summarize by species
species_sums <- bn_results_taxon %>% 
group_by(class, order, Latin.name, Common.name) %>% 
  reframe(Latin.name, Common.name, Count = n(), mean.Conf = mean(Confidence)) %>% 
  distinct()
  
# View
glimpse(species_sums)
species_sums %>% 
  arrange(-Count) %>% 
  print(n = Inf)

# Save the summary
write.csv(species_sums, "Data/bn_naive_species_summary.csv")

################################################################################
# 3) Plots ######################################################################
################################################################################


# Naive number of species by site
site_sums <- bn_results_taxon %>% 
  distinct(Plot, Plot.Type, Common.name) %>% 
  group_by(Plot, Plot.Type) %>% 
  reframe(Plot, Plot.Type, Species.Count = n()) %>% 
  distinct()

# View
glimpse(site_sums)
print(site_sums, n = Inf)

# Plot of species by site
site_sums %>% 
  ggplot() +
  geom_col(aes(x = Plot, y = Species.Count, col = Plot.Type, fill = Plot.Type)) +
  theme_classic()

# Plot of species by plot type
site_sums %>% 
  ggplot() +
  geom_boxplot(aes(x = Plot.Type, y = Species.Count)) +
  theme_classic()

# View a specific species
bn_results_taxon %>% 
  filter(Common.name == "Green Frog")
