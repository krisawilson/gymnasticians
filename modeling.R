# (cooking in progress...)
# packages + data reading + data prep ####
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lme4))
final_data <- read_csv("processed-data/final_data.csv")

# create a dummy var, set to 1 if final round, 0 otherwise
subset_data <- final_data |> 
  filter(country == "USA") |> 
  mutate(round_dummy = if_else(grepl("final", round), 1, 0)) |> 
  # get only vars we need
  select(full_name, gender, round_dummy, apparatus, difficulty, execution, score)
rm(final_data)
# filter by gender since the apparatuses differ
mens <- subset_data |> filter(gender == "M")
womens <- subset_data |> filter(gender == "F")
rm(subset_data)
# mens modeling ####

# mens floor
mens_floor <- lmer(
  execution ~ difficulty + round_dummy + (1 | full_name),
  data = mens, na.action = na.omit, 
  subset = apparatus == "FX")

# extract random intercepts effects
mens_ranef_floor <- ranef(mens_floor)
mens_floor_df <- mens_ranef_floor$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(mens_ranef_floor$full_name)) |> 
  rename(floor_int = `(Intercept)`)
rm(mens_ranef_floor)
# mens horizontal bars
mens_hbars <- lmer(
  execution ~ difficulty + round_dummy + (1 | full_name),
  data = mens, na.action = na.omit, 
  subset = apparatus == "HB")

# extract random intercepts effects
mens_ranef_hbars <- ranef(mens_hbars)
mens_hbars_df <- mens_ranef_hbars$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(mens_ranef_hbars$full_name)) |> 
  rename(hbars_int = `(Intercept)`)
rm(mens_ranef_hbars)
# mens parallel bars
mens_pbars <- lmer(
  execution ~ difficulty + round_dummy + (1 | full_name),
  data = mens, na.action = na.omit, 
  subset = apparatus == "PB")

# extract random intercepts effects
mens_ranef_pbars <- ranef(mens_pbars)
mens_pbars_df <- mens_ranef_pbars$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(mens_ranef_pbars$full_name)) |> 
  rename(pbars_int = `(Intercept)`)
rm(mens_ranef_pbars)
# pommel horse
horse <- lmer(
  execution ~ difficulty + round_dummy + (1 | full_name),
  data = mens, na.action = na.omit, 
  subset = apparatus == "PH")

# extract random intercepts effects
horse_ranef <- ranef(horse)
horse_df <- horse_ranef$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(horse_ranef$full_name)) |> 
  rename(horse_int = `(Intercept)`)
rm(horse_ranef)
# still rings
rings <- lmer(
  execution ~ difficulty + round_dummy + (1 | full_name),
  data = mens, na.action = na.omit, 
  subset = apparatus == "SR")

# extract random intercepts effects
ranef_rings <- ranef(rings)
rings_df <- ranef_rings$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(ranef_rings$full_name)) |> 
  rename(rings_int = `(Intercept)`)
rm(ranef_rings)
# mens vault
mens_vault <- lmer(
  execution ~ difficulty + round_dummy + (1 | full_name),
  data = mens, na.action = na.omit, 
  subset = apparatus %in% c("VT", "VT1", "VT2"))

# extract random intercepts effects
mens_ranef_vault <- ranef(mens_vault)
mens_vault_df <- mens_ranef_vault$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(mens_ranef_vault$full_name)) |> 
  rename(vault_int = `(Intercept)`)
rm(mens_ranef_vault)
# join mens data ####
mens_full <- mens_floor_df |> 
  full_join(mens_hbars_df, by = "gymnast") |> 
  full_join(horse_df, by = "gymnast") |> 
  full_join(mens_pbars_df, by = "gymnast") |> 
  full_join(rings_df, by = "gymnast") |> 
  full_join(mens_vault_df, by = "gymnast") |> 
  select(gymnast, floor_int, hbars_int, pbars_int,
         horse_int, rings_int, vault_int)

# womens modeling ####
# womens floor
womens_floor <- lmer(
  execution ~ difficulty + round_dummy + (1 | full_name),
  data = womens, na.action = na.omit, 
  subset = apparatus == "FX")

# extract random intercepts effects
womens_ranef_floor <- ranef(womens_floor)
womens_floor_df <- womens_ranef_floor$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(womens_ranef_floor$full_name)) |> 
  rename(floor_int = `(Intercept)`)
rm(womens_ranef_floor)
# womens uneven bars
womens_uebars <- lmer(
  execution ~ difficulty + round_dummy + (1 | full_name),
  data = womens, na.action = na.omit, 
  subset = apparatus %in% c("UB", "UE"))

# extract random intercepts effects
womens_ranef_uebars <- ranef(womens_uebars)
womens_uebars_df <- womens_ranef_uebars$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(womens_ranef_uebars$full_name)) |> 
  rename(uebars_int = `(Intercept)`)
rm(womens_ranef_uebars)
# womens balance beam
womens_bb <- lmer(
  execution ~ difficulty + round_dummy + (1 | full_name),
  data = womens, na.action = na.omit, 
  subset = apparatus == "BB")

# extract random intercepts effects
womens_ranef_bb <- ranef(womens_bb)
womens_bb_df <- womens_ranef_bb$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(womens_ranef_bb$full_name)) |> 
  rename(bb_int = `(Intercept)`)
rm(womens_ranef_bb)
# womens vault ####
womens_vault <- lmer(
  execution ~ difficulty + round_dummy + (1 | full_name),
  data = womens, na.action = na.omit, 
  subset = apparatus %in% c("VT", "VT1", "VT2"))

# extract random intercepts effects
womens_ranef_vault <- ranef(womens_vault)
womens_vault_df <- womens_ranef_vault$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(womens_ranef_vault$full_name)) |> 
  rename(vault_int = `(Intercept)`)
rm(womens_ranef_vault)
# join womens data ####
womens_full <- womens_floor_df |> 
  full_join(womens_uebars_df, by = "gymnast") |> 
  full_join(womens_bb_df, by = "gymnast") |> 
  full_join(womens_vault_df, by = "gymnast") |> 
  select(gymnast, floor_int, bb_int, uebars_int, vault_int)

# add mean difficulty ----
womens_bb_diff <- womens |> 
  filter(apparatus == "BB") |> 
  group_by(full_name) |> 
  summarize(avg_diff_bb = mean(difficulty, na.rm=T),
            sd_diff_bb = sd(difficulty, na.rm=T))
womens_fx_diff <- womens |> 
  filter(apparatus == "FX") |> 
  group_by(full_name) |> 
  summarize(avg_diff_floor = mean(difficulty, na.rm=T),
            sd_diff_floor = sd(difficulty, na.rm=T))
womens_ue_diff <- womens |> 
  filter(apparatus %in% c("UB", "UE")) |> 
  group_by(full_name) |> 
  summarize(avg_diff_uneven_bars = mean(difficulty, na.rm=T),
            sd_diff_uneven_bars = sd(difficulty, na.rm=T))
womens_vault_diff <- womens |> 
  filter(apparatus %in% c("VT", "VT1", "VT2")) |> 
  group_by(full_name) |> 
  summarize(avg_diff_vault = mean(difficulty, na.rm=T),
            sd_diff_vault = sd(difficulty, na.rm=T))
womens_full <- womens_full |> 
  left_join(womens_bb_diff, by = join_by(gymnast == full_name)) |> 
  left_join(womens_fx_diff, by = join_by(gymnast == full_name)) |> 
  left_join(womens_ue_diff, by = join_by(gymnast == full_name)) |> 
  left_join(womens_vault_diff, by = join_by(gymnast == full_name))
rm(womens_bb_diff, womens_fx_diff, womens_ue_diff, womens_vault_diff,
   mens_vars, womens_vars)

# mens
hb_diff <- mens |> 
  filter(apparatus == "HB") |> 
  group_by(full_name) |> 
  summarize(avg_diff_hb = mean(difficulty, na.rm=T),
            sd_diff_hb = sd(difficulty, na.rm=T))
pb_diff <- mens |> 
  filter(apparatus == "PB") |> 
  group_by(full_name) |> 
  summarize(avg_diff_pb = mean(difficulty, na.rm=T),
            sd_diff_pb = sd(difficulty, na.rm=T))
mens_fx_diff <- mens |> 
  filter(apparatus == "FX") |> 
  group_by(full_name) |> 
  summarize(avg_diff_fx = mean(difficulty, na.rm=T),
            sd_diff_fx = sd(difficulty, na.rm=T))
pommel_horse_diff <- mens |> 
  filter(apparatus == "PH") |> 
  group_by(full_name) |> 
  summarize(avg_diff_ph = mean(difficulty, na.rm=T),
            sd_diff_ph = sd(difficulty, na.rm=T))
still_rings_diff <- mens |> 
  filter(apparatus == "SR") |> 
  group_by(full_name) |> 
  summarize(avg_diff_sr = mean(difficulty, na.rm=T),
            sd_diff_sr = sd(difficulty, na.rm=T))
mens_vault_diff <- mens |> 
  filter(apparatus %in% c("VT", "VT1", "VT2")) |> 
  group_by(full_name) |> 
  summarize(avg_diff_vault = mean(difficulty, na.rm=T),
            sd_diff_vault = sd(difficulty, na.rm=T))
mens_full <- mens_full |> 
  left_join(hb_diff, by = join_by(gymnast == full_name)) |> 
  left_join(mens_fx_diff, by = join_by(gymnast == full_name)) |> 
  left_join(pb_diff, by = join_by(gymnast == full_name)) |> 
  left_join(mens_vault_diff, by = join_by(gymnast == full_name)) |> 
  left_join(pommel_horse_diff, by = join_by(gymnast == full_name)) |> 
  left_join(still_rings_diff, by = join_by(gymnast == full_name))
rm(hb_diff, pb_diff, pommel_horse_diff, mens_vault_diff,
   mens_fx_diff, still_rings_diff)
# write data 
write_csv(mens_full, "processed-data/mens_intercepts.csv")
write_csv(womens_full, "processed-data/womens_intercepts.csv")

# save environment for models
rm(horse_df, mens_floor_df, mens_hbars_df, mens_pbars_df, mens_vault_df,
   rings_df, womens_bb_df, womens_floor_df, womens_uebars_df, womens_vault_df)
save(list=ls(), file = "processed-data/mlm-env.RData")
