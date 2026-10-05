# ##############################################################################
# Title: Occupancy modeling using spOccupancy
# BirdNet R workflow, Script 6 
# Author: Will Harrod
# Date Created: 2026-10-01
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

# Directory for the model output
mcmc_dir <- "/home/will/NCSU/Model_Outputs"

# Directory for figures
fig_dir <- "/home/will/NCSU/Figures"

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
  filter(!is.na(Threshold) & Threshold < 1.0)
glimpse(conf_thresh)

# Make a species list
sp_list <- conf_thresh |> pull(Species)  

# Join these with the detentions, remove unnecessary columns, and add the confidence thresholds
dct_flt <- dct_raw |> 
  # Remove the temporary plot
  filter(Plot != "IF10") |> 
  # Remove early recordings (Birds Only)
  filter(Date >= start_date) |> 
  # Select only the species of interest
  filter(Species %in% sp_list) |> 
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

# Load the spOccupancy output back in
aru_occ_mod1 <- readRDS(path(mcmc_dir, "aru_occ_mod1.rds"))

# View MCMC summary
summary(aru_occ_mod1)

################################################################################
# 2) Model Diagnostics #########################################################
################################################################################

# 2.1) Preliminary diagnostics -------------------------------------------------

# Traceplots 
# plot(aru_occ_mod1, 'beta', density = FALSE)
# plot(aru_occ_mod1, "alpha", density = FALSE)

# 2.2) Sub sample the posteriors to improve computation species ----------------
# WARNING!!!! This tends to crash R. I need to fix

# Define a number of sub samples
n_sub_samp <- 10
sub_idx <- sample(1:nrow(aru_occ_mod1$beta.samples), n_sub_samp)

# Create a smaller copy of the output
aru_occ_sub <- aru_occ_mod1

# Subsample all posterior matrices in the object
aru_occ_sub$beta.samples  <- aru_occ_sub$beta.samples[sub_idx, , drop = FALSE]
aru_occ_sub$alpha.samples <- aru_occ_sub$alpha.samples[sub_idx, , drop = FALSE]
aru_occ_sub$z.samples     <- aru_occ_sub$z.samples[sub_idx, drop = FALSE]
if (!is.null(aru_occ_sub$psi.samples)) aru_occ_sub$psi.samples <- aru_occ_sub$psi.samples[sub_idx, drop = FALSE]
# View
aru_occ_sub

# Posterior predictive checks
ppc_plt <-  ppcOcc(aru_occ_sub, fit.stat = "freeman-tukey", group = 1) # Grouped by site
ppc_vst <- ppcOcc(aru_occ_sub, fit.stat = "freeman-tukey", group = 2) # Grouped by visit

################################################################################
# 3) Posterior summaries #######################################################
################################################################################

# True list of plots 
plt_ls_tbl <- dct_flt |> 
  distinct(Plot.Type, Plot) |> 
  arrange(Plot.Type) |> 
  mutate(Plot.Index = as.numeric(as.factor(Plot))) |> 
  select(Plot.Index, Plot, Plot.Type) |> 
  mutate(Plot.Index = paste0("plt", Plot.Index))
plt_ls_tbl |> print(n = Inf)

# List of species 
sp_ls_tbl <- dct_flt |> 
  distinct(Species) |> 
  arrange(Species) |> 
  rownames_to_column() |> 
  rename(Species.Index = rowname) |> 
  mutate(Species.Index = paste0("sp", Species.Index))
sp_ls_tbl |> print(n = Inf)

# 3.1) Occupancy probability -------------------------------------------------

# Summarize mean and quantiles across posterior draws for occupancy probability
psi_stats <- apply(aru_occ_mod1$psi.samples, c(2, 3), function(x) {
  c(mean = mean(x), 
    CI.2.5  = quantile(x, 0.025, names = FALSE), 
    CI.97.5 = quantile(x, 0.975, names = FALSE))
})

# Add dimension names for species and sites
dimnames(psi_stats) <- list(
  Stat    = c("mean", "CI.2.5", "CI.97.5"),
  Species.Index = dimnames(aru_occ_mod1$psi.samples)[[2]] %||% paste0("sp", 1:dim(psi_stats)[2]),
  Plot.Index = dimnames(aru_occ_mod1$psi.samples)[[3]] %||% paste0("plt", 1:dim(psi_stats)[3]))

# Convert 3D summary array into a long tibble
aru_occ_psi <- as.data.frame.table(psi_stats, responseName = "value") %>%
  pivot_wider(names_from = Stat, values_from = value) %>%
  as_tibble()  |> 
  # Join the species and plot names 
  left_join(sp_ls_tbl, by = "Species.Index") |> 
  left_join(plt_ls_tbl, by = "Plot.Index") |> 
  select(Species, Plot, Plot.Type, mean, CI.2.5, CI.97.5)

# View
aru_occ_psi

# Save
write_csv(aru_occ_psi, "aru_mod1_occ_psi_summaries.csv")

# 3.2) Occupancied Sites -------------------------------------------------

# Summarize mean and quantiles across posterior draws for occupancy probability
z_stats <- apply(aru_occ_mod1$z.samples, c(2, 3), function(x) {
  c(mean = mean(x), 
    CI.2.5  = quantile(x, 0.025, names = FALSE), 
    CI.97.5 = quantile(x, 0.975, names = FALSE))
})
# View
z_stats

# Add dimension names for species and sites
dimnames(z_stats) <- list(
  Stat    = c("mean", "CI.2.5", "CI.97.5"),
  Species.Index = dimnames(aru_occ_mod1$z.samples)[[2]] %||% paste0("sp", 1:dim(z_stats)[2]),
  Plot.Index = dimnames(aru_occ_mod1$z.samples)[[3]] %||% paste0("plt", 1:dim(z_stats)[3]))

# Convert 3D summary array into a long tibble
aru_occ_z <- as.data.frame.table(z_stats, responseName = "value") %>%
  pivot_wider(names_from = Stat, values_from = value) %>%
  as_tibble()  |> 
  # Join the species and plot names 
  left_join(sp_ls_tbl, by = "Species.Index") |> 
  left_join(plt_ls_tbl, by = "Plot.Index") |> 
  select(Species, Plot, Plot.Type, mean, CI.2.5, CI.97.5)

# View
aru_occ_z |> print(n = Inf)

# Save
write_csv(aru_occ_z, "aru_mod1_occ_occupied_summaries.csv")

# 3.3) Occupancy Covariates --------------------------------------------------

# Summarize mean and quantiles across posterior draws for beta coefficients
beta_stats <- apply(aru_occ_mod1$beta.samples, 2, function(x) {
  c(mean = mean(x), 
    CI.2.5  = quantile(x, 0.025, names = FALSE), 
    CI.97.5 = quantile(x, 0.975, names = FALSE))
})

# Add dimension names for species and sites
aru_occ_beta <- as.data.frame(t(beta_stats)) |> 
  rownames_to_column(var = "Param.Species") |> 
  mutate(Parameter = case_when(
    str_detect(Param.Species, fixed("(Intercept)")) ~ "Interior Forest",
    str_detect(Param.Species, fixed("(Plot.Type)2")) ~ "Reference Edge",
    str_detect(Param.Species, fixed("(Plot.Type)3")) ~ "Turbine",
    str_detect(Param.Species, fixed("Effort")) ~ "Days Deployed",
    TRUE ~ NA_character_
  )) |> 
  mutate(Species = str_split_i(Param.Species, "-", 2)) |> 
  mutate(Species = str_replace_all(Species, fixed("."), " ")) |> 
  select(Species, Parameter, mean, CI.2.5, CI.97.5) 


# View
aru_occ_beta

# Save
write_csv(aru_occ_beta, "aru_mod1_occ_beta_summaries.csv")

# 3.4) Detection Probabilities -----------------------------------------------

# Summarize mean and quantiles across posterior draws for the effect of treatment 
alpha_stats <- apply(aru_occ_mod1$alpha.samples, 2, function(x) {
  c(mean = mean(x), 
    CI.2.5  = quantile(x, 0.025, names = FALSE), 
    CI.97.5 = quantile(x, 0.975, names = FALSE))
})
# View
alpha_stats

# Add dimension names for species and sites
aru_occ_alpha <- as.data.frame(t(alpha_stats)) |> 
  rownames_to_column(var = "Param.Species") |>  
  mutate(Parameter = case_when(
    str_detect(Param.Species, fixed("(Intercept)")) ~ "Intercept",
    str_detect(Param.Species, fixed("date-")) ~ "Date",
    str_detect(Param.Species, fixed("I(date^2)")) ~ "Date2",
    TRUE ~ NA_character_
  )) |> 
  mutate(Species = str_split_i(Param.Species, "-", 2)) |> 
  mutate(Species = str_replace_all(Species, fixed("."), " ")) |> 
  select(Species, Parameter, mean, CI.2.5, CI.97.5)
# View
aru_occ_alpha

# Save
write_csv(aru_occ_alpha, "aru_mod1_occ_alpha_summaries.csv")

################################################################################
# 4) Posterior Plots ###########################################################
################################################################################

# Palette
wind_pal <- c(
  "Interior Forest" = "forestgreen",
  "Reference Edge" = "goldenrod3",
  "Turbine" = "darkorchid4")


# 4.1) Occupancy Probability by plot type --------------------------------------

# View the occupancy data
aru_occ_beta
glimpse(aru_occ_beta)

# Intercepts by species
intercepts <- aru_occ_beta |> 
  filter(Parameter == "Interior Forest") |> 
  mutate(Intercept.Mean = mean, Intercept.CI.2.5 = CI.2.5, Intercept.CI.97.5 = CI.97.5) |> 
  select(Species, Intercept.Mean, Intercept.CI.2.5, Intercept.CI.97.5) 
  
# View
glimpse(intercepts)

# Inverse logit function
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

# Prep the data
occ_pred_dat <- aru_occ_beta |> 
  filter(Parameter != "Days Deployed") |>
  left_join(intercepts, by = "Species") |> 
  # Change covariate factors to replect occupancy probabilities
  mutate(
    mean = case_when(
      Parameter == "Interior Forest" ~ mean,
      Parameter != "Interior Forest" ~ Intercept.Mean + mean 
    ),
    CI.2.5 = case_when(
      Parameter == "Interior Forest" ~ CI.2.5,
      Parameter != "Interior Forest" ~ Intercept.CI.2.5 + CI.2.5 
    ),
    CI.97.5 = case_when(
      Parameter == "Interior Forest" ~ CI.97.5,
      Parameter != "Interior Forest" ~ Intercept.CI.97.5+ CI.97.5
    )) |> 
  mutate(Species.Param = paste(Species, Parameter, sep = "-")) |> 
  mutate(Species = factor(Species)) |> 
  mutate(Species = fct_rev(Species)) |> 
  # logit transform covariates
  mutate(
    logit.mean = inv_logit(mean),
    logit.CI.2.5 = inv_logit(CI.2.5),
    logit.CI.97.5 = inv_logit(CI.97.5)
  ) 
# View
glimpse(occ_pred_dat)

# Define a dodge offset so parameter 
pd <- position_dodge(width = 0.6)

# Make the plot
occ_pred_dat |> 
  ggplot(aes(y = Species)) +
  # Add points at the mean values for each parameter
  geom_point(
    aes(
        x = logit.mean, 
        colour = Parameter
        ), 
    shape = 15, 
    size = 1, 
    alpha = 0.8,
    position = pd
  ) +
  # Add whiskers for Credible intervals
  # geom_linerange(
  #   aes(
  #     xmin = logit.CI.2.5, 
  #     xmax = logit.CI.97.5, 
  #     # linetype = Supported, 
  #     colour = Parameter
  #   ), 
  #   linewidth = 0.4,
  #   alpha = 0.7,
  #   position = pd
  # ) +
  # Add a vertical line at zero
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(values = wind_pal) +
  # Change the Labels
  labs(x = "Parameter Estimate", y = "") + 
  # Simple theme
  theme_classic() +
  # Edit theme
  theme(
    legend.position = "top", 
    legend.text = element_text(size = 10), 
    legend.title = element_blank(),
    plot.title = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 8)
  ) +
  facet_wrap(~Parameter)
