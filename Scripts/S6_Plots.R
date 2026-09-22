# ################################################################################
# Title: Plots
# BirdNet R workflow, Script 6
# Author: Will Harrod
# Date Created: 2026-08-27
################################################################################

# Add packages
library(tidyverse)
library(fs)

# Clear environments
rm(list = ls())

################################################################################
# 1) Plots ######################################################################
################################################################################

# Read in the data
bn_results <- read.csv("Data/preliminary_BirdNET_Results.csv") |> 
  select(-X) 

# View
glimpse(bn_results)

# Detection By Date
bn_results |> 
  ggplot() +
  geom_histogram(aes(x = ymd(Date)))

# Naive number of species by site
site_sums <- bn_results |> 
  distinct(Plot, Plot.Type, Species) |> 
  group_by(Plot, Plot.Type) |> 
  reframe(Plot, Plot.Type, Species.Count = n()) |> 
  distinct()

# View
glimpse(site_sums)
print(site_sums, n = Inf)

# Plot of species by site
site_sums |> 
  ggplot() +
  geom_col(aes(x = Plot, y = Species.Count, col = Plot.Type, fill = Plot.Type)) +
  theme_classic()

# Plot of species by plot type
site_sums |> 
  ggplot() +
  geom_boxplot(aes(x = Plot.Type, y = Species.Count)) +
  theme_classic()

# View a specific species
bn_results |> 
  filter(Species == "Indigo Bunting" & Plot.Type == "IF") |> 
  select(Species, Plot, Confidence, File)

# Number of distinct plots by type
n_plots <- bn_results |> 
  distinct(Plot, Plot.Type) |> 
  mutate(Plot.Name = case_when(Plot.Type == "IF" ~ "Interior Forest",
                               Plot.Type == "RE" ~ "Reference Edge",
                               Plot.Type == "TO" ~ "Turbine"
  )) |> 
  group_by(Plot.Name) |> 
  reframe(Plot.Name, n.Plots = n()) |> 
  distinct()
n_plots

################################################################################
# Plot of number of detection by species and plot type #########################
################################################################################

# Palette
wind_pal <- c(
 "Interior Forest" = "forestgreen",
 "Reference Edge" = "goldenrod3",
 "Turbine" = "darkorchid4")

# Species to ignore
ignore_species <- c("Swamp Sparrow", "Gray Treefrog", "Fowler's Toad", 
                    "Human non-vocal", "House Wren", "White-throated Sparrow",
                    "American Bullfrog")

bn_results |> 
filter(!Species %in% ignore_species) |> 
write.csv("D:/BirdNet_Output/Full_Site/preliminary_BirdNET_Results.csv")

# Plot of number of detection by species and plot type
bn_results |> 
  filter(!Species %in% ignore_species &
           class %in% c("Amphibia", "Aves") &
           Confidence >= 0.3) |> 
  mutate(Plot.Name = case_when(Plot.Type == "IF" ~ "Interior Forest",
                               Plot.Type == "RE" ~ "Reference Edge",
                               Plot.Type == "TO" ~ "Turbine"
                               )) |> 
  group_by(Species, Plot.Type) |> 
  reframe(Species, Plot.Type, Plot.Name, Plot.Count = n(), Mean.Conf = mean(Confidence)) |> 
  distinct() |> 
  group_by(Species) |> 
  reframe(Species,Plot.Type, Plot.Name, Total.Count = Plot.Count, Plot.Count, Mean.Conf) |> 
  arrange(-Total.Count) |> 
  filter(Total.Count > 1000) |> 
  mutate(Species = factor(Species)) |> 
  mutate(Species = fct_reorder(Species, Plot.Count, .fun = sum)) |> 
  ggplot(aes(x = Species, y = Total.Count, fill = Plot.Name)) +
  geom_col() +
  coord_flip() + 
  scale_fill_manual(values = wind_pal) +
  scale_color_manual(values = wind_pal) +
  scale_y_continuous() +
  theme_classic() +
  labs(y = "Number of Detections") +
  theme(axis.text.y = element_text(size = 7),
        axis.title.y = element_blank(),
        legend.title = element_blank()) 
  
################################################################################
# Proportion of sites with at least one detection ##############################
################################################################################

# Species Order
species_order <- bn_results |> 
  group_by(class, Species) |> 
  reframe(Total = n(), .groups = "drop") |> 
  arrange(desc(class), Species) |> 
  pull(Species)

# Proportion of sites with at least one detection
bn_results |> 
  filter(!Species %in% ignore_species &
           class %in% c("Amphibia", "Aves") &
           Confidence >= 0.3) |> 
  mutate(Plot.Name = case_when(Plot.Type == "IF" ~ "Interior Forest",
                               Plot.Type == "RE" ~ "Reference Edge",
                               Plot.Type == "TO" ~ "Turbine"
  )) |> 
  group_by(Species, Plot) |> 
  reframe(class, Species, Plot, Plot.Type, Plot.Name, n.Detections = n()) |> 
  distinct() |> 
  mutate(Present = case_when(n.Detections >= 10 ~ 1, TRUE ~ 0)) |> 
  group_by(Species, Plot.Type) |> 
  reframe(class, Species, Plot.Type, Plot.Name, n.Sites.Present = sum(Present)) |> 
  distinct() |> 
  left_join(n_plots, by = "Plot.Name") |> 
  mutate(Prop.Present = n.Sites.Present / n.Plots) |> 
  mutate(Species = factor(Species, levels = species_order)) |>
  ggplot(aes(x = Plot.Type, y = Prop.Present, fill = Plot.Name)) +
  geom_col() +
  scale_fill_manual(values = wind_pal) +
  scale_color_manual(values = wind_pal) +
  theme_classic() +
  labs(title = "Proportion of ARUs with at least 100 detections") +
  facet_wrap(~Species) +
  theme(axis.text.y = element_text(size = 7),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 6),
        legend.title = element_blank()) 


################################################################################
# Species detections by plot ###################################################
################################################################################

# View all the species
bn_results |> distinct(Species) |>  arrange(Species) |> pull(Species)

# Pick a Species
bird <- "Brown Thrasher"
total_dct <- bn_results |> filter(Species == bird) |> nrow()

# Make the plot
bn_results |> 
  filter(Species == bird & !Plot == "IF10") |> 
  mutate(Plot.Name = case_when(Plot.Type == "IF" ~ "Interior Forest",
                               Plot.Type == "RE" ~ "Reference Edge",
                               Plot.Type == "TO" ~ "Turbine"
  )) |> 
  group_by(Plot) |> 
  reframe(
          Species, Plot, Plot.Name, 
          Count = n(), 
          Mean.Conf = signif(mean(Confidence), digits = 2)
          ) |> 
  distinct() |>
  mutate(Plot.Name = fct_rev(factor(Plot.Name)),
         Max.Count = max(Count),
         Total.Count = sum(Count)) |> 
  ggplot() +
  geom_col(aes(x = Plot, y = Count, fill = Plot.Name)) +
  geom_text(aes(x = Plot, y = Count + 0.03*Max.Count, label = Mean.Conf)) +
  coord_flip() + 
  scale_fill_manual(values = wind_pal) +
  scale_color_manual(values = wind_pal) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous() +
  theme_classic() +
  labs(y = "Number of Detections",
       title = paste0(bird, ": ", total_dct, " Detections")) +
  theme(axis.text.y = element_text(size = 7),
        axis.title.y = element_blank(),
        legend.title = element_blank()) 
  

################################################################################
# Confidence scores by species #################################################
################################################################################

# Make the plot
bn_results |> 
  # Only Birds for Now
  filter(class == "Aves") |> 
  # Remove some species
  filter(!Species %in%c("American Crow", "Fish Crow", "Chuck-will's-widow", "Barred Owl", "Red-headed Woodpecker")) |> 
  ggplot() +
  geom_density(aes(x = Confidence), col = "lightblue", fill = "lightblue") + 
  theme_classic() +
  facet_wrap(~Species) +
  theme(axis.text.y = element_text(size = 7),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 6),
        legend.title = element_blank()) 
