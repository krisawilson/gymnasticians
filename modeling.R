# (cooking in progress...)
# packages + data reading + data prep ####
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lme4))
final_data <- read_csv("processed-data/final_data.csv")

# create a dummy var, set to 1 if final round, 0 otherwise
subset_data <- final_data |> 
  mutate(round_dummy = if_else(grepl("final", round), 1, 0)) |> 
  # get only vars we need
  select(full_name, country, gender, round_dummy, apparatus, score)
rm(final_data)
# filter by gender since the apparatuses differ
mens <- subset_data |> filter(gender == "M")
womens <- subset_data |> filter(gender == "F")
rm(subset_data)
# mens modeling ####

# mens floor
mens_floor <- lmer(
  score ~ round_dummy + (1 | full_name),
  data = mens, na.action = na.omit, 
  subset = apparatus == "FX")

# extract random intercepts effects
mens_ranef_floor <- ranef(mens_floor)
mens_floor_df <- mens_ranef_floor$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(mens_ranef_floor$full_name)) |> 
  rename(floor_int = `(Intercept)`)
rm(mens_ranef_floor)
floor_var <- summary(mens_floor)$varcor$full_name[1]
# mens horizontal bars
mens_hbars <- lmer(
  score ~ round_dummy + (1 | full_name),
  data = mens, na.action = na.omit, 
  subset = apparatus == "HB")

# extract random intercepts effects
mens_ranef_hbars <- ranef(mens_hbars)
mens_hbars_df <- mens_ranef_hbars$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(mens_ranef_hbars$full_name)) |> 
  rename(hbars_int = `(Intercept)`)
rm(mens_ranef_hbars)
hbars_var <- summary(mens_hbars)$varcor$full_name[1]
# mens parallel bars
mens_pbars <- lmer(
  score ~ round_dummy + (1 | full_name),
  data = mens, na.action = na.omit, 
  subset = apparatus == "PB")

# extract random intercepts effects
mens_ranef_pbars <- ranef(mens_pbars)
mens_pbars_df <- mens_ranef_pbars$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(mens_ranef_pbars$full_name)) |> 
  rename(pbars_int = `(Intercept)`)
rm(mens_ranef_pbars)
pbars_var <- summary(mens_pbars)$varcor$full_name[1]
# pommel horse
horse <- lmer(
  score ~ round_dummy + (1 | full_name),
  data = mens, na.action = na.omit, 
  subset = apparatus == "PH")

# extract random intercepts effects
horse_ranef <- ranef(horse)
horse_df <- horse_ranef$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(horse_ranef$full_name)) |> 
  rename(horse_int = `(Intercept)`)
rm(horse_ranef)
horse_var <- summary(horse)$varcor$full_name[1]
# still rings
rings <- lmer(
  score ~ round_dummy + (1 | full_name),
  data = mens, na.action = na.omit, 
  subset = apparatus == "SR")

# extract random intercepts effects
ranef_rings <- ranef(rings)
rings_df <- ranef_rings$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(ranef_rings$full_name)) |> 
  rename(rings_int = `(Intercept)`)
rm(ranef_rings)
rings_var <- summary(rings)$varcor$full_name[1]
# mens vault
mens_vault <- lmer(
  score ~ round_dummy + (1 | full_name),
  data = mens, na.action = na.omit, 
  subset = apparatus %in% c("VT", "VT1", "VT2"))

# extract random intercepts effects
mens_ranef_vault <- ranef(mens_vault)
mens_vault_df <- mens_ranef_vault$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(mens_ranef_vault$full_name)) |> 
  rename(vault_int = `(Intercept)`)
rm(mens_ranef_vault)
mens_vault_var <- summary(mens_vault)$varcor$full_name[1]
# join mens data ####
mens_full <- mens_floor_df |> 
  full_join(mens_hbars_df, by = "gymnast") |> 
  full_join(horse_df, by = "gymnast") |> 
  full_join(mens_pbars_df, by = "gymnast") |> 
  full_join(rings_df, by = "gymnast") |> 
  full_join(mens_vault_df, by = "gymnast") |> 
  select(gymnast, floor_int, hbars_int, pbars_int,
         horse_int, rings_int, vault_int)
# replace NAs with the mean of zero
mens_full <- mens_full |> 
  mutate(across(c("floor_int", "hbars_int", "pbars_int",
                  "horse_int", "rings_int", "vault_int"),
                ~ if_else(is.na(.), 0, .)))
rm(horse_df, mens_floor, mens_floor_df, mens_hbars,
   mens_hbars_df, mens_pbars, mens_pbars_df, mens_vault,
   mens_vault_df, rings, rings_df, horse)

mens_vars <- c(floor_var = floor_var, 
                    hbars_var = hbars_var,
                    horse_var = horse_var,
                    pbars_var = pbars_var,
                    rings_var = rings_var, 
                    mens_vault_var = mens_vault_var)

# womens modeling ####
# womens floor
womens_floor <- lmer(
  score ~ round_dummy + (1 | full_name),
  data = womens, na.action = na.omit, 
  subset = apparatus == "FX")

# extract random intercepts effects
womens_ranef_floor <- ranef(womens_floor)
womens_floor_df <- womens_ranef_floor$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(womens_ranef_floor$full_name)) |> 
  rename(floor_int = `(Intercept)`)
rm(womens_ranef_floor)
w_floor_var <- summary(womens_floor)$varcor$full_name[1]
# womens uneven bars
womens_uebars <- lmer(
  score ~ round_dummy + (1 | full_name),
  data = womens, na.action = na.omit, 
  subset = apparatus %in% c("UB", "UE"))

# extract random intercepts effects
womens_ranef_uebars <- ranef(womens_uebars)
womens_uebars_df <- womens_ranef_uebars$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(womens_ranef_uebars$full_name)) |> 
  rename(uebars_int = `(Intercept)`)
rm(womens_ranef_uebars)
uebars_var <- summary(womens_uebars)$varcor$full_name[1]
# womens balance beam
womens_bb <- lmer(
  score ~ round_dummy + (1 | full_name),
  data = womens, na.action = na.omit, 
  subset = apparatus == "BB")

# extract random intercepts effects
womens_ranef_bb <- ranef(womens_bb)
womens_bb_df <- womens_ranef_bb$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(womens_ranef_bb$full_name)) |> 
  rename(bb_int = `(Intercept)`)
rm(womens_ranef_bb)
bb_var <- summary(womens_bb)$varcor$full_name[1]
# womens vault ####
womens_vault <- lmer(
  score ~ round_dummy + (1 | full_name),
  data = womens, na.action = na.omit, 
  subset = apparatus %in% c("VT", "VT1", "VT2"))

# extract random intercepts effects
womens_ranef_vault <- ranef(womens_vault)
womens_vault_df <- womens_ranef_vault$full_name |> 
  as_tibble() |> 
  mutate(gymnast = rownames(womens_ranef_vault$full_name)) |> 
  rename(vault_int = `(Intercept)`)
rm(womens_ranef_vault)
womens_vault_var <- summary(womens_vault)$varcor$full_name[1]
# join womens data ####
womens_full <- womens_floor_df |> 
  full_join(womens_uebars_df, by = "gymnast") |> 
  full_join(womens_bb_df, by = "gymnast") |> 
  full_join(womens_vault_df, by = "gymnast") |> 
  select(gymnast, floor_int, bb_int, uebars_int, vault_int)
# replace NAs with the mean of zero
womens_full <- womens_full |> 
  mutate(across(c("floor_int", "bb_int", 
                  "uebars_int", "vault_int"),
                ~ if_else(is.na(.), 0, .)))
rm(womens_floor, womens_floor_df, womens_uebars,
   womens_uebars_df, womens_vault, womens_vault_df,
   womens_bb, womens_bb_df)

womens_vars <- c(w_floor_var = w_floor_var, 
                 uebars_var = uebars_var,
                 bb_var = bb_var,
                 womens_vault_var = womens_vault_var)

# add country back ####
mens_full <- left_join(mens_full, mens,
                       by = join_by(gymnast == full_name),
                       multiple = "first",
                       unmatched = "drop") |> 
  select(gymnast, country, contains("_int"))
womens_full <- left_join(womens_full, womens,
                       by = join_by(gymnast == full_name),
                       multiple = "first",
                       unmatched = "drop") |> 
  select(gymnast, country, contains("_int"))

# remove stuff
rm(mens, womens, bb_var, floor_var, hbars_var, horse_var,
   mens_vault_var, pbars_var, rings_var, uebars_var,
   w_floor_var, womens_vault_var)
# write data 
###write_csv(mens_full, "processed-data/mens_intercepts.csv")
##write_csv(womens_full, "processed-data/womens_intercepts.csv")

# save enivronment for variances
##save(list=ls(), file = "processed-data/mlm-env.RData")
