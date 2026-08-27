# ################################################################################
# Title: Occupancy modeling using spOccupancy
# BirdNet R workflow, Script 6 
# Author: Will Harrod
# Date Created: 2026-05-29
################################################################################

################################################################################
# 1) Prep ######################################################################
################################################################################

# 1.1) Add packages and data ---------------------------------------------------

# Clear environments
rm(list = ls())

# Load Packages
library(tidyverse)
library(spOccupancy)
library(fs)
library(suncalc)
library(sf)

# Set Seed
set.seed(27606)

# 1.2) File directories (Change these for your device) -------------------------

# Directory where the BirdNet validations are housed
bn_dir <- "C:/NCSU/Data/BirdNet_Validations"

# Directory where the validation summaries are held
csv_dir <-  "Data"

# Directory for where the audio files are stored 
audio_dir <- "D:/Subsetted_ARU_Data"

# 1.3) Add the filtered BirdNET detections and other data ----------------------

# Add the birdnet validation summary data 
dct_raw <-  read_csv(path(csv_dir, "Preliminary_BirdNET_Results_Filtered.csv"))
# View
glimpse(dct_raw)
#counts by species
dct_raw %>% count(Common.name)

# List of species to include
species_list <- c(
                  # "Black-and-white Warbler",
                  "Brimley's Chorus Frog", 
                  # "Brown-headed Nuthatch", 
                  # "Common Yellowthroat", 
                  # "Eastern Towhee", 
                  # "Field Sparrow", 
                  # "Ovenbird", 
                  # "Pine Warbler", 
                  "Southern Toad",
                  "Spring Peeper"
                  # "White-eyed Vireo"
                  )

# Add the site meta data 
site_locs <- read.csv(path(csv_dir, "aru_locations.csv")) %>% 
  select(-Plot.Type) %>% 
  mutate(Plot = str_replace_all(Plot, "TE", "TO"))
# View
glimpse(site_locs)
distinct(site_locs, Plot)

# Join these with the detentions and remove unnecessary columns 
dct_loc <- dct_raw %>% 
  # Remove the temparary plot
  filter(Plot != "IF10") %>% 
  # Select only the species of interest
  filter(Common.name %in% species_list) %>% 
  mutate(File.Time = str_extract(basename(File), "\\d{6}\\.wav"),
         Num.Time = str_remove_all(File.Time, "\\.wav"),
         Date.Time = paste(as.character(Date), Num.Time),
         Rec.Time = ymd_hms(Date.Time),
         File.Name = basename(File)
         ) %>% 
  # Fix the rows from the daylight savings change
  mutate(
    Rec.Time = case_when(
    is.na(Rec.Time) & !is.na(Date.Time) ~ ymd_hms(Date.Time) + hours(1),
    TRUE ~ Rec.Time
    )) %>%
  mutate(Rec.Time = force_tz(Rec.Time, tzone = "America/New_York")) %>%
  # Calculate the recording time
  left_join(site_locs, by = "Plot") %>% 
  select(
    Common.name, Confidence, class, 
    Plot, Plot.Type,
    Date, Rec.Time, x, y, File.Name
    ) %>% 
  # Remove the window from the daylight savings split
  drop_na(Rec.Time)
  
# View
glimpse(dct_loc)

# See any NA rows
dct_loc %>% 
filter(is.na(Rec.Time)) %>% 
  select(File.Name, Date, Rec.Time)

# 1.4) Calculate sunrise -------------------------------------------------------

# Make a data frame for date and location at the center of the study area
sunrise_dat <- dct_loc %>% 
  reframe(date = Date,
          lat = mean(y),
          lon = mean(x)) %>% 
  distinct()
# View
glimpse(sunrise_dat)

# Calculate local sunrise and sunset times 
sunrise_times <- getSunlightTimes(
  data = sunrise_dat,                           
  keep = c("sunrise", "sunset"),
  tz = "America/New_York"
) %>% 
  rename(Date = date) %>% 
  select(-lat, -lon) %>% 
  distinct() %>% 
  arrange(Date) %>% 
  # Calculate 30 min before sunrise
  mutate(Window.Start = sunrise - minutes(35))
# View
glimpse(sunrise_times)

# Combine with the rest of the data 
dct_sunr <- dct_loc %>% 
  left_join(sunrise_times, by = "Date") %>% 
    # Ask if each point is within 5 minutes of 30 minutes before sunrise
  mutate(Morning.Survey = case_when(Rec.Time >= Window.Start ~ 1,
                                    Rec.Time < Window.Start ~ 0))
# View
glimpse(dct_sunr)

# Counts of surveys by morning vs not
dct_sunr %>% 
  distinct(Rec.Time, Morning.Survey) %>% 
  count(Morning.Survey)

# Counts of species by morning vs night
dct_sunr %>% 
  count(Common.name, Morning.Survey)

# View that for a single species
dct_sunr %>% 
  filter(Common.name == "Black-and-white Warbler") %>% 
  select(Rec.Time, sunrise, Window.Start, Morning.Survey, File.Name) %>% 
  print(n = 100)

################################################################################
# 2) Convert data to a format for spOccupancy ########################################
################################################################################

# 2.1) prepare detection matrix framework --------------------------------------
# Survey window: 1 day

# Add in the Raw detection Date
full_birdnet <-  read.csv(path(csv_dir, "Preliminary_BirdNET_Results.csv"))

# View
glimpse(full_birdnet)

# Clean
full_birdnet_clean <-  full_birdnet %>% 
  mutate(Date = ymd(Date)) %>% 
  select(Plot, Date)
#View again
glimpse(full_birdnet_clean)

# Find the missing surveys
vst_dates <- full_birdnet %>% 
  filter(Plot != "IF10") %>% 
  group_by(Plot) %>% 
  reframe(Plot, 
          Plot.Type,
          Start.Date = ymd(min(Date)),
          End.Date = ymd(max(Date))) %>% 
  distinct() %>% 
  mutate(Start.Date = case_when(Start.Date > ymd("2026-03-08") ~ ymd("2026-03-08"),
                                TRUE ~ Start.Date)) %>% 
  rowwise() %>% 
  mutate(
    Date = list(seq(Start.Date, End.Date, by = "1 day"))) %>%
  unnest(Date) %>%
  mutate(Date.Index = 1 + as.numeric(Date - Start.Date)) %>% 
  ungroup() %>% 
  select(Plot, Plot.Type, Date, Date.Index)
  
# View
vst_dates

# Widen sunrise data
dct_wide <-  dct_sunr %>% 
  mutate(Occupied = 1,
         Species = str_remove_all(Common.name, "'"),
         Species = str_replace_all(Species, " ", "."),
         Species = str_replace_all(Species, "-", ".")) %>% 
  select(Plot, Date, Species, Occupied) %>% 
  distinct() %>% 
  pivot_wider(names_from = Species, values_from = Occupied) 

# View
glimpse(dct_wide)

# Combine to see whether each species was at that plot during that visit
dct_vst_sp <- vst_dates %>% 
  left_join(dct_wide, by = c("Plot", "Date")) %>% 
  mutate(across(.cols = everything(), .fns = ~replace_na(., 0)))
# View
glimpse(dct_vst_sp)

# 2.2) Set up detection matrix dimentions ----------------------------------------------

# List the plots
plts <- vst_dates %>% distinct(Plot) %>% pull(Plot)
plts

# List the number of sites
n_plts <- length(plts)
n_plts

# List thle number of visits by plot
vst_count <- vst_dates %>% 
  group_by(Plot) %>% 
  reframe(Plot, n.Visits = max(Date.Index)) %>% 
  distinct()
vst_count

# List the maximum number of dates
n_dates_max <- max(vst_count$n.Visits)
n_dates_max

# List of anuran species
sp_list <- dct_sunr %>% 
  filter(class == "Amphibia") %>% # Optional, comment out
  distinct(Common.name) %>%
  pull(Common.name)
sp_list

# List the number of anuran species 
n_sp <- length(sp_list)
n_sp

# Plot level information 
plt_info <- vst_dates %>% 
  distinct(Plot, Plot.Type) %>% 
  left_join(site_locs, by = "Plot") %>% 
  mutate(Plot.Type = factor(
    Plot.Type, 
    levels = c("IF", "RE", "TO"))
    ) %>% 
  mutate(Plot.Type = as.numeric(Plot.Type))  
plt_info
  
# List of plot types
plt_trts <- plt_info %>% pull(Plot.Type)
plt_trts

# Number of plot types
n_trts <- max(unique(plt_trts))
n_trts

# Detection matrix storage object
dct_mtx <- array(data = NA, dim = c(n_sp, n_plts, n_dates_max))
dct_mtx

# Date storage object
day_mtx <- matrix(data = NA, nrow = n_plts, ncol = n_dates_max)
day_mtx

# 2.3) Fill in detection matrices ----------------------------------------------
# Loop over the matrix and fill in for each species
for(s in 1:n_sp){

  # Pick a single species
  sp <- sp_list[s]

  # Loop over plots 
  for(i in 1:n_plts){
    
    # Pick a plot
    plt <-  plts[i]
    
    # Find out how long that plot was surveyed
    svy_cnt_plt <- vst_count %>% 
      filter(Plot == plt) %>% 
      pull(n.Visits)
    
    # Filter detentions to a single species at a single plot
    dct_sp_plt <- dct_vst_sp %>% 
      filter(Plot == plt) %>% 
      select(4 + s) %>% 
      pull(1)
    
    # Assign them to the appropriate portion of the detection matrix
    dct_mtx[s, i, 1:svy_cnt_plt] <- dct_sp_plt
    
    # Pull out the dates
    day_plt <- dct_vst_sp %>% 
      mutate(Date.Index.scl = scale(Date.Index)[,1]) %>% 
      filter(Plot == plt) %>% 
      pull(Date.Index.scl)
    
    # Assign these to a covatiate matrix
    day_mtx[i, 1:svy_cnt_plt] <- day_plt
    
  }
}

# View
dct_mtx[1, ,]
day_mtx

# Combine site covs as a matrix
occ_covs <-  matrix(
  data = c(
    plt_trts
           ),
  byrow = FALSE
)
occ_covs

# Combine detection covs as a list
det_covs <- list(
  day_mtx
  )
det_covs

# Define coordinates
coords <- plt_info %>% 
  select(x, y) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_transform(crs = 32618) %>%
  tibble() %>% 
  mutate(geometry = str_remove_all(geometry, "[c),(]")) %>% 
  separate_wider_delim(
    cols = geometry, 
    delim = " ", 
    names = c("x", "y")
  ) %>% 
    select(x, y) %>% 
    mutate(across(.cols = everything(), .fns = ~ as.numeric(.))) %>% 
  as.matrix()
# View
coords

################################################################################
# 3) run spOccupancy ########################################
################################################################################

# Bundle the data 
dat_lst <- list(
  y = dct_mtx,
  occ.covs = occ_covs,
  det.covs = det_covs,
  coords = coords
)
dat_lst

# Define inits 
inits <- list(
  
)

# Define priors
priors <- list(
  alph.comm = ,
  beta.comm = ,
  alpha =  ,
  beta = , 
  tau.sq.beta = , 
  tau.sq.alpha = ,
  sigma.sq.psi = ,
  # sigma.sq.p = , 
  z = ,
  sigma.sq = , 
  phi = , 
  w = NA
)

# Run the model
occ_mod1 <- spMsPGOcc(
  occ.formula = NA,
  det.formula = NA,
  data = NA
)
