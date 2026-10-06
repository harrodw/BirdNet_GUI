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
psi_stats <- apply(aru_occ_mod$psi.samples, c(2, 3), function(x) {
  c(mean = mean(x), 
    CI.2.5  = quantile(x, 0.025, names = FALSE), 
    CI.97.5 = quantile(x, 0.975, names = FALSE))
})

# Add dimension names for species and sites
dimnames(psi_stats) <- list(
  Stat    = c("mean", "CI.2.5", "CI.97.5"),
  Species.Index = dimnames(aru_occ_mod$psi.samples)[[2]] %||% paste0("sp", 1:dim(psi_stats)[2]),
  Plot.Index = dimnames(aru_occ_mod$psi.samples)[[3]] %||% paste0("plt", 1:dim(psi_stats)[3]))

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
z_stats <- apply(aru_occ_mod$z.samples, c(2, 3), function(x) {
  c(mean = mean(x), 
    CI.2.5  = quantile(x, 0.025, names = FALSE), 
    CI.97.5 = quantile(x, 0.975, names = FALSE))
})
# View
z_stats

# Add dimension names for species and sites
dimnames(z_stats) <- list(
  Stat    = c("mean", "CI.2.5", "CI.97.5"),
  Species.Index = dimnames(aru_occ_mod$z.samples)[[2]] %||% paste0("sp", 1:dim(z_stats)[2]),
  Plot.Index = dimnames(aru_occ_mod$z.samples)[[3]] %||% paste0("plt", 1:dim(z_stats)[3]))

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
beta_stats <- apply(aru_occ_mod$beta.samples, 2, function(x) {
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
alpha_stats <- apply(aru_occ_mod$alpha.samples, 2, function(x) {
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