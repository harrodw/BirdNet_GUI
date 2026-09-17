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
wd <- "/media/will/Timbermill/BirdNet_Output"

# Viw the folders in that working directory
sites <- dir_ls(wd, regexp = "[A-Z]{2}\\d{2}")
sites

# How many sites
n_sites <- length(sites)
n_sites

# View the contents of one folder
bn_results_s1 <- dir_ls(sites[1], regexp = "\\.txt$")
bn_results_s1

# Read in the first output csv (might be empty)
bn_results_s1r1 <- bn_results_s1[1] |> 
  read_tsv(show_col_types = FALSE) |> 
  tibble() |> 
  rename(Start.sec = `Begin Time (s)`,
         End.sec = `End Time (s)`,
         Species = `Common Name`,
         File = `Begin Path`) |> 
  select(Species, Confidence, Start.sec, End.sec, File) |> 
  slice_head(n = 1) 

# View
bn_results_s1r1
glimpse(bn_results_s1r1)

################################################################################
# 2) Data Cleaning ##############################################################
################################################################################

# Storage objects
bn_results <- tibble()

# Loop through the ARU files to combine them all
for(i in 1:n_sites){

  # Define an ARU
  site <- sites[i]

  # List the summary csv's in that folder
  bn_results_site <- dir_ls(site, regexp = "\\.txt$")

  # Count the number of csv's in that folder
  n_files <- length(bn_results_site)

  # Progress message
  message("Combinging classification summaries for aru ", i,
          " out of ", n_sites, ". ", n_files, " recordings ")

  # Loop through these and combine them
  for(j in 1:n_files){

    # Define a single summary
    class_sum <- bn_results_site[j] |>
      read_tsv(show_col_types = FALSE) |>
      tibble() |>
      rename(Start.sec = `Begin Time (s)`,
             End.sec = `End Time (s)`,
             Species = `Common Name`,
             File = `Begin Path`) |>
      select(Species, Confidence, Start.sec, End.sec, File)

    # Combine that with the other recording summaries
    bn_results <- bind_rows(bn_results, class_sum)

  }
}

# View the full combined BirdNET classifier output
glimpse(bn_results)
slice_head(bn_results)

# How many distinct species?
bn_results |> 
  count(Species) |> 
  nrow()

# Add in the possible species list 
psb_species <- read.csv("Data/possible_species.csv") |> 
  tibble() |> 
  mutate(Scientific.name = paste(genus, species)) |> 
  rename(Species = common_name,
         Latin.Name = Scientific.name)
# View
glimpse(psb_species)

# List of anthropogenic sounds
anthro_sounds <- c("Engine", "Gun", "Human non-vocal", "Human vocal", "Power tools")

# Combine with the Birdnet Results
bn_results_taxon <- left_join(bn_results, psb_species, 
                              by = c("Species")) |> 
  mutate(class = case_when(Latin.Name == "Dog" ~ "Mammalia",
                           Latin.Name %in% anthro_sounds ~ "Anthropogenic",
                           TRUE ~ class),
         order = case_when(Latin.Name == "Dog" ~ "Carnivora",
                           Latin.Name %in% anthro_sounds ~ "Anthropogenic",
                           TRUE ~ order),
         Date = ymd(Date))  |> 
  arrange(class, order, Species) |> 
  # Extract information from the file names
  mutate(
         Plot = factor(str_extract(File, pattern = "[A-Z]{2}\\d{2}")),
         Date = ymd(str_extract(File, pattern = "\\d{8}")),
         Time = str_extract(File, pattern = "\\d{6}.wav")
         ) |> 
  mutate(
         Time = str_remove_all(Time, pattern = ".wav"),       # Raw time
         Plot.Type = factor(str_remove_all(Plot, "\\d{2}"))   # Plot Type
         ) |> 
  mutate(Rec.Hour = as.numeric(str_sub(Time, start = 1, end = 2)), # Start Hour
         Rec.Min = as.numeric(str_sub(Time, start = 3, end = 4))   # Recording Minute
         ) |> 
  # Reorder columns and remove unnessesary ones
  select(
         Plot, Plot.Type , Date, Rec.Hour, Rec.Min, Start.sec, End.sec,
         Species, Confidence, Latin.Name, class, order, File
         )


# Drop the bird vocalizations before May 
bn_results_frog <-  bn_results_taxon |> filter(class %in% c("Amphibia", "Anthropogenic"))
glimpse(bn_results_frog)
bn_results_bird <-  bn_results_taxon |> filter(class == "Aves" & ymd(Date) >= ymd("2026-05-01"))
glimpse(bn_results_bird)

# Combine
bn_results_full <-  bind_rows(bn_results_bird, bn_results_frog)

# View
glimpse(bn_results_full)
print(bn_results_full, n = 30)

# Save the full results
bn_results_full |> write.csv("Data/preliminary_BirdNET_Results.csv")

# Summarize by species
species_sums <- bn_results_taxon |> 
group_by(class, order, Latin.name, Common.name) |> 
  reframe(Latin.name, Common.name, Count = n(), mean.Conf = mean(Confidence)) |> 
  distinct()
  
# View
glimpse(species_sums)
species_sums |> 
  arrange(-Count) |> 
  print(n = Inf)

# Save the summary
write.csv(species_sums, "Data/bn_naive_species_summary.csv")
