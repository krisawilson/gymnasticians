# distribution of player effects ----
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lme4))
# load env
load("processed-data/mlm-env.RData")

ranef_bootstrap <- function(me_model, nsim = 500, seed = 2024) {
  # first, define summary statistic function
  
  ## in this case, just computing random effects
  ## assuming original data has full_name and `(Intercept)`
  ranef_vector <- function(me_model){
    df <- ranef(me_model)$full_name
    return(as.numeric(df$`(Intercept)`))
  }
  # Perform parametric bootstrap
  me_model_summary <- bootMer(x = me_model, FUN = ranef_vector,
                              nsim = nsim, use.u = FALSE)
  
  # Extract original intercept estimates
  original <- as.data.frame(me_model_summary$t0)
  colnames(original) <- "original" # rename
  
  # turn estimates into data frame
  me_model_results <- as.data.frame(me_model_summary) # for validation
  
  # extract standard errors
  boot_sd <- as.data.frame(apply(me_model_results, 2, sd))
  colnames(boot_sd) <- "sd" # rename
  
  # create the final data frame
  final_df <- data.frame(original = original$original, 
                         std_error = boot_sd$sd)
  
  return(final_df)
}

# apply function! women first ----
bb_bootstrap <- ranef_bootstrap(womens_bb)
colnames(bb_bootstrap) <- c("bb_int", "bb_se_exec")
rm(womens_bb)

womens_floor_bootstrap <- ranef_bootstrap(womens_floor)
colnames(womens_floor_bootstrap) <- c("floor_int", "floor_se_exec")
rm(womens_floor)

uneven_bars_bootstrap <- ranef_bootstrap(womens_uebars)
colnames(uneven_bars_bootstrap) <- c("uneven_bars_int",
                                     "uneven_bars_se_exec")
rm(womens_uebars)

womens_vault_bootstrap <- ranef_bootstrap(womens_vault)
colnames(womens_vault_bootstrap) <- c("vault_int", "vault_se_exec")
rm(womens_vault)
# men ----
mens_floor_bootstrap <- ranef_bootstrap(mens_floor)
colnames(mens_floor_bootstrap) <- c("floor_int", "floor_se_exec")
rm(mens_floor)

pbars_bootstrap <- ranef_bootstrap(mens_pbars)
colnames(pbars_bootstrap) <- c("pbars_int", "pbars_se_exec")
rm(mens_pbars)

hbars_bootstrap <- ranef_bootstrap(mens_hbars)
colnames(hbars_bootstrap) <- c("hbars_int", "hbars_se_exec")
rm(mens_hbars)

mens_vault_bootstrap <- ranef_bootstrap(mens_vault)
colnames(mens_vault_bootstrap) <- c("vault_int", "vault_se_exec")
rm(mens_vault)

still_rings_bootstrap <- ranef_bootstrap(rings)
colnames(still_rings_bootstrap) <- c("still_rings_int",
                                     "still_rings_se_exec")
rm(rings)

pommel_horse_boostrap <- ranef_bootstrap(horse)
colnames(pommel_horse_boostrap) <- c("pommel_horse_int",
                                     "pommel_horse_se_exec")
rm(horse)

# join womens data ----

# join the data. there will be duplicate matches; that's okay
womens_full <- womens_full |> 
  left_join(bb_bootstrap, by = "bb_int") |> 
  left_join(womens_floor_bootstrap, by = "floor_int") |> 
  left_join(uneven_bars_bootstrap, 
            join_by(uebars_int == uneven_bars_int)) |> 
  left_join(womens_vault_bootstrap, by = "vault_int")

# consolidate duplicates. since they're mostly copies, this is okay
womens_avgs <- womens_full |> 
  group_by(gymnast) |> 
  summarize(floor_mean_exec = mean(floor_int,na.rm = T), 
            floor_se_exec = mean(floor_se_exec,na.rm = T),
            uneven_bars_mean_exec = mean(uebars_int,na.rm = T),
            uneven_bars_se_exec = mean(uneven_bars_se_exec,na.rm = T),
            vault_mean_exec = mean(vault_int,na.rm = T),
            vault_se_exec = mean(vault_se_exec,na.rm = T),
            bb_mean_exec = mean(bb_int,na.rm = T),
            bb_se_exec = mean(bb_se_exec,na.rm = T)) |> ungroup()

# replace the NaNs with the average of zero
womens_avgs <- womens_avgs |> 
  mutate(across(where(is.numeric), ~ if_else(is.na(.), 0, .)))
rm(bb_bootstrap, uneven_bars_bootstrap, womens_floor_bootstrap, womens_vault_bootstrap)

# join the averages w the og data
womens_total <- left_join(womens_avgs, select(womens_full, 
                                              c(gymnast, contains("diff"))),
                          by = "gymnast",multiple = "first", 
                          unmatched = "drop") |> 
  select(-contains("int"))

# join mens data ----

# join the data. there will be duplicate matches; that's okay
mens_full <- mens_full |> 
  left_join(mens_floor_bootstrap, 
            by = "floor_int") |> 
  left_join(pommel_horse_boostrap, 
            join_by(horse_int == pommel_horse_int)) |> 
  left_join(still_rings_bootstrap, 
            join_by(rings_int == still_rings_int)) |> 
  left_join(pbars_bootstrap, by = "pbars_int") |> 
  left_join(hbars_bootstrap, by = "hbars_int") |> 
  left_join(mens_vault_bootstrap, by= "vault_int")

# consolidate duplicates. since they're mostly copies, this is okay
mens_avgs <- mens_full |> 
  group_by(gymnast) |> 
  summarize(floor_mean_exec = mean(floor_int,na.rm = T), 
            floor_se_exec = mean(floor_se_exec,na.rm = T),
            hbars_mean_exec = mean(hbars_int,na.rm = T),
            hbars_se_exec = mean(hbars_se_exec,na.rm = T),
            vault_mean_exec = mean(vault_int,na.rm = T),
            vault_se_exec = mean(vault_se_exec,na.rm = T),
            pbars_mean_exec = mean(pbars_int,na.rm = T),
            pbars_se_exec = mean(pbars_se_exec,na.rm = T),
            still_rings_mean_exec = mean(rings_int, na.rm=T),
            still_rings_se_exec = mean(still_rings_se_exec, na.rm = T),
            pommel_horse_mean_exec = mean(horse_int, na.rm = T),
            pommel_horse_se_exec = mean(pommel_horse_se_exec,
                                        na.rm = T)) |> ungroup()

# replace the NaNs with the average of zero
mens_avgs <- mens_avgs |> 
  mutate(across(where(is.numeric), ~ if_else(is.na(.), 0, .)))
rm(hbars_bootstrap, mens_floor_bootstrap, mens_vault_bootstrap, 
   pbars_bootstrap, pommel_horse_boostrap, still_rings_bootstrap)

# join data
mens_total <- left_join(mens_avgs, 
                        mens_full |> select(c(gymnast, contains("diff"))), 
                        by = "gymnast",
                        multiple = "first", unmatched = "drop") |> 
  select(-contains("int"))

# write!
write_csv(mens_total, "processed-data/mens_bootstrapped_results.csv")
write_csv(womens_total, "processed-data/womens_bootstrapped_results.csv")