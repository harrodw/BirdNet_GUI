# ##############################################################################
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
library(MCMCvis)
library(fs)
library(suncalc)
library(sf)

# Set Seed
set.seed(27606)

# 1.2) File directories (Change these for your device) -------------------------

# Directory where the validation summaries are held
csv_dir <-  "/home/will/NCSU/R_Code/BirdNet_GUI/Data"

# Start Date for the bird surveys
start_date <- ymd("2026-05-01")

# 1.3) Add the filtered BirdNET detections and other data ----------------------

# Add the birdnet validation summary data 
dct_raw <-  read_csv(path(csv_dir, "preliminary_BirdNET_Results.csv"))
# View
glimpse(dct_raw)
#counts by species
dct_raw |> count(Species)

# Add the confidence threasholds 
conf_thresh_raw <- read_csv(path(csv_dir, "BirdNet_Thresholds.csv"))
glimpse(conf_thresh_raw)

# Clean the confidence thresholds
conf_thresh <-  conf_thresh_raw |> 
  select(`Common Name`, Threshold) |> 
  rename(Species = `Common Name`) |> 
  mutate(Species = str_remove_all(Species, "'"),
         Species = str_replace_all(Species, " ", "."),
         Species = str_replace_all(Species, "-", ".")) |>
  distinct(Species, Threshold) |>
  arrange(Species) |> 
  filter(!is.na(Threshold) & Threshold < 1.0)
glimpse(conf_thresh)

# Make a species list
sp_ls <- conf_thresh |> pull(Species)  

# Join these with the detentions, remove unnecessary columns, and add the confidence thresholds
dct_flt <- dct_raw |> 
  # Remove the temporary plot
  filter(Plot != "IF10") |> 
  # Remove early recordings (Birds Only)
  filter(Date >= start_date) |> 
  # Change Species names
  mutate(Species = str_remove_all(Species, "'"),
         Species = str_replace_all(Species, " ", "."),
         Species = str_replace_all(Species, "-", ".")) |>
  # Select only the species of interest
  filter(Species %in% sp_ls) |> 
  mutate(File.Time = str_extract(basename(File), "\\d{6}\\.wav"),
         Num.Time = str_remove_all(File.Time, "\\.wav"),
         Date.Time = paste(as.character(Date), Num.Time),
         Rec.Time = ymd_hms(Date.Time),
         File.Name = basename(File)
         ) |> 
  # Fix the rows from the daylight savings change
  mutate(
    Rec.Time = case_when(
    is.na(Rec.Time) & !is.na(Date.Time) ~ ymd_hms(Date.Time) + hours(1),
    TRUE ~ Rec.Time
    )) |> 
  mutate(Rec.Time = force_tz(Rec.Time, tzone = "America/New_York")) |>
  # Calculate the recording time
  select(
    Species, Confidence, class, 
    Plot, Plot.Type,
    Date, Rec.Time, File.Name
    ) |> 
  # Remove the window from the daylight savings split
  drop_na(Rec.Time) |> 
  # Combine with the confidence thresholds
  left_join(conf_thresh, by = "Species") |> 
  # Remove the recordings under the minimum theshold
  filter(Confidence >= Threshold)
  
# View
glimpse(dct_flt)

################################################################################
# 2) Convert data to a format for spOccupancy ##################################
################################################################################

# 2.1) prepare detection matrix framework --------------------------------------
# Survey window: 1 day

# View the Raw detection Data
glimpse(dct_raw)

# Clean
dct_raw_clean <-  dct_raw |> 
  mutate(Date = ymd(Date)) |> 
  select(Plot, Date)
#View again
glimpse(dct_raw_clean)

# Find the missing surveys
vst_dates <- dct_raw |> 
  filter(Plot != "IF10" & Date >= start_date) |> 
  group_by(Plot) |> 
  reframe(
    Plot, 
    Plot.Type,
    Date,
    Start.Date = min(Date),
    End.Date = max(Date)
    ) |> 
  distinct() |> 
  mutate(Surveyed = 1)
# View
glimpse(vst_dates)

# All dates in order at a specific plot
check_plot <- "IF01"
vst_dates |> 
  filter(Plot == check_plot) |> 
  arrange(Date) |> 
  print(n = Inf)

# Make a dataframe of all dates where each ARU was in the field (including those where it was turned off)
all_dpy_dts <- vst_dates |> 
  distinct(Plot, Plot.Type, Start.Date, End.Date) |> 
  rowwise() |>
  mutate(Date = list(seq(Start.Date, End.Date, by = "1 day"))) |> 
  unnest(Date) |> 
  mutate(Date.Index = as.numeric(as.factor(Date))) 
# View
glimpse(all_dpy_dts)

# Combine the data frames and see how many days each site was surveyed for 
date_idnx <- all_dpy_dts |> 
  left_join(vst_dates, by =c("Plot", "Plot.Type", "Date", "Start.Date", "End.Date")) |> 
  mutate(Surveyed = replace_na(Surveyed, 0)) |> 
  select(-Start.Date, - End.Date) |> 
  arrange(Plot, Date)

# View
glimpse(date_idnx)

# All dates in order at a specific site again
date_idnx |> 
  filter(Plot == check_plot) |> 
  arrange(Date) |> 
  print(n = Inf)

# Widen the detection data
dct_wide <-  dct_flt |>
  mutate(Occupied = 1,
         Species = str_remove_all(Species, "'"),
         Species = str_replace_all(Species, " ", "."),
         Species = str_replace_all(Species, "-", ".")) |>
  select(Plot, Date, Species, Occupied) |>
  distinct() |>
  pivot_wider(names_from = Species, values_from = Occupied) |>
  mutate(Plot = factor(Plot)) |> 
  arrange(Plot, Date)

# View
glimpse(dct_wide)

# Combine to see whether each species was at that plot during that visit
dct_vst_na <- date_idnx |>
  # Switch categories to factors
  mutate(Plot = factor(Plot),
         Plot.Index = as.numeric(Plot),
         Plot.Type = factor(Plot.Type, levels = c("IF", "RE", "TO"))) |>
  select(Plot, Plot.Type, Plot.Index, Date, Date.Index, Surveyed) |> 
  left_join(dct_wide, by = c("Plot", "Date"))
glimpse(dct_vst_na)

# Turn zeros to NA's after the maximum number of recordings
dct_vst <-  dct_vst_na |>
  mutate(across(.cols = 5:ncol(dct_vst_na), .fns = ~ case_when(Surveyed == 1 ~ replace_na(., 0), TRUE ~ NA))) |>
  select(1:6, sort(names(dct_vst_na)[7:ncol(dct_vst_na)]))
# View
glimpse(dct_vst)
# View all species_detected at a specific site again
dct_vst |>
  select(Plot, Date, Surveyed, Eastern.Towhee, Hooded.Warbler, Field.Sparrow) |>
  filter(Plot == check_plot) |>
  arrange(Date) |>
  print(n = Inf)

# 2.2) Set up detection matrix dimensions ----------------------------------------------

# List the plots
plts <- vst_dates |> distinct(Plot) |> pull(Plot)
plts

# List the number of sites
n_plts <- length(plts)
n_plts

# List thle number of visits by plot
vst_count <- dct_vst |> 
  filter(Surveyed == 1) |> 
  group_by(Plot) |> 
  reframe(Plot, n.Visits = length(unique(Date))) |> 
  distinct() 
vst_count |> print(n = Inf)

# List the maximum number of dates
n_dates_max <- max(vst_count$n.Visits)
n_dates_max

# List the number of species 
n_sp <- length(sp_ls)
n_sp

# Plot level information 
plt_info <- vst_dates |> 
  distinct(Plot, Plot.Type) |> 
  left_join(vst_count, by = "Plot") |> # effort
  mutate(Plot.Type = factor(
    Plot.Type, 
    levels = c("IF", "RE", "TO")),
    Plot = factor(Plot)
    ) |> 
  mutate(Plot.Type = as.numeric(Plot.Type),
         Plot = as.numeric(Plot))  
plt_info
  
# List of plot types
plt_trts <- plt_info |> 
  mutate(Effort = scale(n.Visits)[,1]) |>  
  select(Plot.Type, Plot, Effort)
plt_trts

# Scaled date
dates_scl <- dct_vst |> 
  select(Plot, Date.Index, Surveyed) |> 
  mutate(Date.Index.scl = scale(Date.Index)[,1]) 
# View
glimpse(dates_scl)

# Number of plot types
n_trts <- max(unique(plt_trts))
n_trts

# Detection array storage object
dct_mtx <- array(data = NA, dim = c(n_sp, n_plts, n_dates_max))
str(dct_mtx)

# Date storage object
day_mtx <- matrix(data = NA_integer_, nrow = n_plts, ncol = n_dates_max)
str(day_mtx)

# 2.3) Fill in detection ----------------------------------------------
glimpse(dct_vst)
# Loop over the matrix and fill in for each species
for(s in 1:n_sp){

  # Pick a single species
  sp <- sp_ls[s]

  # Loop over plots 
  for(i in 1:n_plts){

    # Pick a plot
    plt <-  plts[i]
    
    # Find out how long that plot was surveyed
    svy_cnt_plt <- plt_info |> 
      filter(Plot == i) |> 
      pull(n.Visits)
    
    # Filter detentions to a single species at a single plot
    dct_sp_plt <- dct_vst |> 
      filter(Plot.Index == i & Surveyed == 1) |> 
      select(any_of(sp_ls)) |> 
      pull(1)
    
    # Assign them to the appropriate portion of the detection matrix
    dct_mtx[s, i, 1:svy_cnt_plt] <- dct_sp_plt
    
    # Pull out the dates
    day_plt <- dates_scl |> 
      filter(Plot == plt & !is.na(Date.Index.scl) & Surveyed == 1) |> 
      pull(Date.Index.scl)
    
    # Assign these to a covatiate matrix
    day_mtx[i, 1:svy_cnt_plt] <- day_plt
    
  }
  
  # Message after each species
  message("Finsihed converting ", sp, " audio data into a detection matrix")
}

# Add row names
dimnames(dct_mtx)[[1]] <- dct_flt |> 
  distinct(Species) |>
  arrange(Species) |> 
  pull(Species)

  # View
dct_mtx[1, ,]
str(dct_mtx)
day_mtx

# Collapse detection histories for occupancy priors
occupied <- apply(dct_mtx, c(1, 2), max, na.rm = TRUE)
occupied
  
# Combine site covs as a matrix 
occ_covs <-  plt_trts |> 
  as.matrix()
occ_covs 

# Combine detection covs as a list
det_covs <- list(
  date = day_mtx
  )
det_covs

################################################################################
# 3) run spOccupancy ###########################################################
################################################################################

# 3.1) Bundle Data -------------------------------------------------------------

# Bundle the data 
dat_lst <- list(
  y = dct_mtx,
  occ.covs = occ_covs,
  det.covs = det_covs
)
dat_lst

# Define inits 
inits <- list(
  alpha.comm = 0,
  beta.comm = 0,
  alpha = 0,
  beta = 0, 
  tau.sq.beta = 1, 
  tau.sq.alpha = 1,
  z = occupied
)
inits

# Define priors
priors <- list(
  beta.comm.normal = list(mean = 0, var = 2.72),
  alpha.comm.normal = list(mean = 0, var = 2.72), 
  tau.sq.beta.ig = list(a = 0.1, b = 0.1), 
  tau.sq.alpha.ig = list(a = 0.1, b = 0.1)
  )

# MCMC parameters
n_sample <- 80000
n_rprt <- n_sample/2
n_burn <- n_sample/2
n_thin <-  50
n_chains <- 3

# How many samples
(n_sample - n_burn)*n_chains / n_thin

# Model formulas 
occ_formu <- ~ factor(Plot.Type) + Effort
det_formu <- ~ date + I(date^2)

# Directory for the model output
mcmc_dir <- "/home/will/NCSU/Model_Outputs"

# 3.2) Run the model -----------------------------------------------------------

# Run the model
aru_occ_mod1 <- msPGOcc(
  occ.formula = occ_formu,  # Occupancy Formula
  det.formula = det_formu,              # Detection formula
  data = dat_lst,                              # Data
  inits = inits,       # Initial Values
  priors = priors,     # Priors
  verbose = TRUE,      # Display messages
  n.samples = n_sample,
  n.report = n_rprt,
  n.burn = n_burn,
  n.thin = n_thin,
  n.chains = n_chains,
  n.omp.threads = 4
)

# Make sure all is good
summary(aru_occ_mod1)

# Save the Model summary
saveRDS(aru_occ_mod1, path(mcmc_dir, "aru_occ_mod1.rds"))


