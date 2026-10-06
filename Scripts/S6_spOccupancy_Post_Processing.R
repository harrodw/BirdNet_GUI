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
library(fs)

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

# Load the spOccupancy output back in
aru_occ_mod <- readRDS(path(mcmc_dir, "aru_occ_mod1.rds"))

# View MCMC summary
names(aru_occ_mod)
summary(aru_occ_mod)

################################################################################
# 2) Model Diagnostics #########################################################
################################################################################

# 2.1) Preliminary diagnostics -------------------------------------------------

# Traceplots 
# plot(aru_occ_mod, 'beta', density = FALSE)
# plot(aru_occ_mod, "alpha", density = FALSE)

# 2.2) Goodness of fit stats by plot -------------------------------------------
aru_occ_out_plt <- ppcOcc(aru_occ_mod, fit.stat = "freeman-tukey", group = 1) 

# View
summary(aru_occ_out_plt)
str(aru_occ_out_plt)

# Convert to a data frame
aru_occ_out_plt_tbl <- data.frame(
  fit = aru_occ_out_plt$fit.y,
  fit.rep = aru_occ_out_plt$fit.y.rep
) |> 
  tibble()

# View
glimpse(aru_occ_out_plt_tbl)

# Pivot Longer
aru_occ_out_plt_lng <- aru_occ_out_plt_tbl |> 
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "species"),
    names_pattern = "^(fit\\.rep|fit)\\.(\\d+)$"
  ) %>%
  mutate(
    species = as.numeric(species),
    rep.greater = fit.rep > fit
  )
# View
glimpse(aru_occ_out_plt_lng)

# Visualize
aru_occ_out_plt_lng |> 
   ggplot(aes(x = fit, y = fit.rep)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 0.8) +
  geom_point(aes(fill = rep.greater), shape = 21, color = "black", size = 2.5, alpha = 0.8) +
  scale_fill_manual(
    values = c("FALSE" = "lightskyblue1", "TRUE" = "lightsalmon"),
    guide = "none" 
  ) +
  labs(
    x = "True",
    y = "Fit",
    title = "Posterior Predictive Check by Plot"
  ) +
  theme_classic() 

# Which plots contribute to the poor goodness of fit? 
aru_occ_out_fit_plt <- aru_occ_out_plt$fit.y.rep.group.quants[3, , ] - aru_occ_out_plt$fit.y.group.quants[3, , ]
plot(aru_occ_out_fit_plt, pch = 19, xlab = 'Site ID', ylab = 'Replicate - True Discrepancy')

# 2.3 Repeat at the visit level ----------------------------------------------------
aru_occ_out_vst <- ppcOcc(aru_occ_mod, fit.stat = "freeman-tukey", group = 2) 

# View
summary(aru_occ_out_vst)
str(aru_occ_out_vst)

# Convert to a data frame
aru_occ_out_vst_tbl <- data.frame(
  fit = aru_occ_out_vst$fit.y,
  fit.rep = aru_occ_out_vst$fit.y.rep
) |> 
  tibble()

# View
glimpse(aru_occ_out_vst_tbl)

# Pivot Longer
aru_occ_out_vst_lng <- aru_occ_out_vst_tbl |> 
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "species"),
    names_pattern = "^(fit\\.rep|fit)\\.(\\d+)$"
  ) %>%
  mutate(
    species = as.numeric(species),
    rep.greater = fit.rep > fit
  )
# View
glimpse(aru_occ_out_vst_lng)

# Visualize
aru_occ_out_vst_lng |> 
  ggplot(aes(x = fit, y = fit.rep)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 0.8) +
  geom_point(aes(fill = rep.greater), shape = 21, color = "black", size = 2.5, alpha = 0.8) +
  scale_fill_manual(
    values = c("FALSE" = "lightskyblue1", "TRUE" = "lightsalmon"),
    guide = "none" 
  ) +
  labs(
    x = "True",
    y = "Fit",
    title = "Posterior Predictive Check by replicate"
  ) +
  theme_classic() 

################################################################################
# 3) Predictions ###############################################################
################################################################################

# View the output again
summary(aru_occ_mod)

# Define a credible interval width
ci_max <- 0.875
ci_min <- 1 - ci_max

# Model formulas 
occ_formu <- ~ factor(Plot.Type) + Effort
det_formu <- ~ date + I(date^2)

# Generate sample plots for predictions
pred_df <- data.frame(
  Plot.Type = factor(c("IF", "RE", "TO"), levels = c("IF", "RE", "TO")),
  Effort = rep(0, 3)
)

# Generate design matrix using your original occurrence formula
X.0 <- model.matrix(occ_formu, data = pred_df)
# View
X.0

# Generate average occupancy predictions at those locations
aru_occ_pred <- predict(aru_occ_mod, X.0)

# Separate the occupancy probabilities
aru_occ_prob_pred <- aru_occ_pred$psi.0.samples
colnames(aru_occ_prob_pred) <- sp_ls
# View
aru_occ_prob_pred
str(aru_occ_prob_pred)

# Summarize by plot type
aru_occ_prob_pred_if <- data.frame(aru_occ_prob_pred[, , 1]) |> 
  tibble() |> 
  pivot_longer(names_to = "Species", values_to = "Psi", cols = everything()) |> 
  mutate(Plot.Type = "Interior Forest")
aru_occ_prob_pred_re <- data.frame(aru_occ_prob_pred[, , 2]) |> 
  tibble() |>
  pivot_longer(names_to = "Species", values_to = "Psi", cols = everything()) |> 
  mutate(Plot.Type = "Reference Edge")
aru_occ_prob_pred_tb <- data.frame(aru_occ_prob_pred[, , 3]) |> 
  tibble() |>
  pivot_longer(names_to = "Species", values_to = "Psi", cols = everything()) |> 
  mutate(Plot.Type = "Turbine")

# Combine
aru_occ_prob_pred_full <- bind_rows(
  aru_occ_prob_pred_if,
  aru_occ_prob_pred_re,
  aru_occ_prob_pred_tb
)

# View
glimpse(aru_occ_prob_pred_full)
count(aru_occ_prob_pred_full, Plot.Type)

# Summarize all samples by species
aru_occ_prob_pred_sum <- aru_occ_prob_pred_full |> 
  group_by(Plot.Type, Species) |> 
  reframe(Mean = mean(Psi),
          CI.Low = quantile(Psi, probs = ci_min),
          CI.High = quantile(Psi, probs = ci_max)
          ) 
# View
glimpse(aru_occ_prob_pred_sum)

################################################################################
# 4) Posterior Plots ###########################################################
################################################################################

# View the data again
glimpse(aru_occ_prob_pred_sum)

# 3.1) Occupancy Probability by plot type --------------------------------------

# Palette
wind_pal <- c(
  "Interior Forest" = "forestgreen",
  "Reference Edge" = "goldenrod3",
  "Turbine" = "darkorchid4")

# Make the plot
aru_occ_prob_pred_fig <- aru_occ_prob_pred_sum |>
  mutate(Species = str_replace_all(Species, fixed("."), " ")) |>
  mutate(Species = factor(Species)) |> 
  mutate(Species = fct_reorder(Species, desc(Species))) |> 
  ggplot(aes(y = Species)) +
  # Add points at the mean values for each parameter
  geom_point(
    aes(
        x = Mean, 
        colour = Plot.Type
        ), 
    shape = 15, 
    size = 1.2, 
    alpha = 0.8
  ) +
  # Add whiskers for Credible intervals
  geom_linerange(
    aes(
      xmin = CI.Low,
      xmax = CI.High,
      # linetype = Supported,
      colour = Plot.Type
    ),
    linewidth = 0.4,
    alpha = 0.7
  ) +
  # Add a vertical line at zero
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(values = wind_pal) +
  # Change the Labels
  labs(x = "Parameter Estimate", y = "") + 
  # Simple theme
  theme_bw() +
  # Edit theme
  theme(
    legend.position = "top", 
    legend.text = element_text(size = 10), 
    legend.title = element_blank(),
    plot.title = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10)
  ) +
  facet_wrap(~Plot.Type)

# View the plot 
aru_occ_prob_pred_fig

# Save the plot as a png
ggsave(plot = aru_occ_prob_pred_fig,
       path(fig_dir, "aru_occupancy_prob_plot_type_whiskers.png"),
       width = 200,
       height = 175,
       units = "mm",
       dpi = 300)


# 3.2) Difference between turbines and other plots -----------------------------------

# Palette
signif_pal <- c(
  "Supported Difference" = "darkblue",
  "No Supported Difference" = "gray30"
 )

# Extract differences between turbines and the other plot types
aru_occ_diff_tb_if <- data.frame(aru_occ_prob_pred[, , 3] - aru_occ_prob_pred[, , 1]) |> 
  tibble() |> 
  pivot_longer(names_to = "Species", values_to = "Psi", cols = everything()) |> 
  mutate(Psi.Diff = "Turbine vs Interior Forest")
aru_occ_diff_tb_re <- data.frame(aru_occ_prob_pred[, , 3] - aru_occ_prob_pred[, , 2]) |> 
  tibble() |> 
  pivot_longer(names_to = "Species", values_to = "Psi", cols = everything()) |> 
  mutate(Psi.Diff = "Turbine vs Reference Edge")
# View
glimpse(aru_occ_diff_tb_if)
glimpse(aru_occ_diff_tb_re)

# Combine
aru_occ_diff_full <- bind_rows(
  aru_occ_diff_tb_if,
  aru_occ_diff_tb_re
  ) |> 
  group_by(Species, Psi.Diff) |> 
  reframe(Mean = mean(Psi),
          CI.Low = quantile(Psi, probs = ci_min),
          CI.High = quantile(Psi, probs = ci_max)
  )  |> 
  distinct() |> 
  mutate(Supported = case_when(CI.High*CI.Low > 0 ~ "Supported Difference",
                               CI.High*CI.Low <= 0 ~ "No Supported Difference"
                               )) 
# View
glimpse(aru_occ_diff_full)

# Make the plot
aru_occ_diff_plot_whisker <- aru_occ_diff_full |>
  mutate(Species = str_replace_all(Species, fixed("."), " ")) |>
  mutate(Species = factor(Species)) |> 
  mutate(Species = fct_reorder(Species, desc(Species))) |> 
  ggplot(aes(y = Species)) +
  # Add points at the mean values for each parameter
  geom_point(
    aes(x = Mean, color = Supported), 
    shape = 15, 
    size = 1.2, 
    alpha = 0.8
  ) +
  # Add whiskers for Credible intervals
  geom_linerange(
    aes(
      xmin = CI.Low,
      xmax = CI.High,
      colour = Supported
    ),
    linewidth = 0.4,
    alpha = 0.7
  ) +
  # Change colors
  scale_color_manual(values = signif_pal) +
  # Add a vertical line at zero
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  # Change the Labels
  labs(x = "Parameter Estimate", y = "") + 
  # Simple theme
  theme_bw() +
  # Edit theme
  theme(
    legend.position = "top", 
    legend.text = element_text(size = 10), 
    legend.title = element_blank(),
    plot.title = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10)
  ) +
  facet_wrap(~Psi.Diff)

# View the plot 
aru_occ_diff_plot_whisker

# Save the plot as a png
ggsave(plot = aru_occ_diff_plot_whisker,
       path(fig_dir, "aru_occupancy_diff_plot_type_whiskers.png"),
       width = 200,
       height = 175,
       units = "mm",
       dpi = 300)

