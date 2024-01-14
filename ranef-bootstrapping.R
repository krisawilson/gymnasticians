# distribution of player effects ----
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(VIM))
# load env
load("processed-data/mlm-env.RData")

ranef_bootstrap <- function(me_model, nsim = 100, seed = 2024) {
  # first, define summary statistic function
  
  ## in this case, just computing random effects
  ## assuming original data has full_name and `(Intercept)`
  ranef_vector <- function(me_model){
    df <- ranef(me_model)$full_name
    return(as.numeric(df$`(Intercept)`))
  }
  # Perform parametric bootstrap
  me_model_summary <- bootMer(x = me_model, FUN = ranef_vector,
                           nsim = nsim, use.u = TRUE, seed = seed)
  
  # Extract original intercept estimates
  original <- as.data.frame(me_model_summary$t0)
  colnames(original) <- "original" # rename
  
  # turn estimates into data frame
  me_model_results <- as.data.frame(me_model_summary) # for validation
  
  # extract bootstrapped estimates
  boot_mean <- as.data.frame(colMeans(me_model_results))
  colnames(boot_mean) <- "estimate" # rename
  
  # extract standard errors
  boot_sd <- as.data.frame(apply(me_model_results, 2, sd))
  colnames(boot_sd) <- "sd" # rename
  
  # create the final data frame
  final_df <- data.frame(original = original$original, 
                         bias = boot_mean$estimate - original$original, 
                         std_error = boot_sd$sd)
  
  return(final_df)
}

# apply function! women first ----
rm(mens_vars, womens_vars)
bb_bootstrap <- ranef_bootstrap(womens_bb)
colnames(bb_bootstrap) <- c("bb_int", "bb_bias", "bb_std_error")
rm(womens_bb)

womens_floor_bootstrap <- ranef_bootstrap(womens_floor)
colnames(womens_floor_bootstrap) <- c("w_floor_int", "w_floor_bias", "w_floor_std_error")
rm(womens_floor)

uneven_bars_bootstrap <- ranef_bootstrap(womens_uebars)
colnames(uneven_bars_bootstrap) <- c("uneven_bars_int", "uneven_bars_bias", "uneven_bars_std_error")
rm(womens_uebars)

womens_vault_bootstrap <- ranef_bootstrap(womens_vault)
colnames(womens_vault_bootstrap) <- c("womens_vault_int", "womens_vault_bias", "womens_vault_std_error")
rm(womens_vault)
# men ----
mens_floor_bootstrap <- ranef_bootstrap(mens_floor)
colnames(mens_floor_bootstrap) <- c("mens_floor_int", "mens_floor_bias", "mens_floor_std_error")
rm(mens_floor)

pbars_bootstrap <- ranef_bootstrap(mens_pbars)
colnames(pbars_bootstrap) <- c("pbars_int", "pbars_bias", "pbars_std_error")
rm(mens_pbars)

hbars_bootstrap <- ranef_bootstrap(mens_hbars)
colnames(hbars_bootstrap) <- c("hbars_int", "hbars_bias", "hbars_std_error")
rm(mens_hbars)

mens_vault_bootstrap <- ranef_bootstrap(mens_vault)
colnames(mens_vault_bootstrap) <- c("mens_vault_int", "mens_vault_bias", "mens_vault_std_error")
rm(mens_vault)

still_rings_bootstrap <- ranef_bootstrap(rings)
colnames(still_rings_bootstrap) <- c("still_rings_int", "still_rings_bias", "still_rings_std_error")
rm(rings)

pommel_horse_boostrap <- ranef_bootstrap(horse)
colnames(pommel_horse_boostrap) <- c("pommel_horse_int", "pommel_horse_bias", "pommel_horse_std_error")
rm(horse)

# join womens data ----
# put NAs back, then omit them from computations
womens_full <- womens_full |> 
  mutate(across(where(is.numeric), ~ if_else(. == 0, NA, .)))

# join the data. there will be duplicate matches; that's okay
womens_full <- womens_full |> 
  left_join(bb_bootstrap, by = "bb_int") |> 
  left_join(womens_floor_bootstrap, 
            join_by(floor_int == w_floor_int)) |> 
  left_join(uneven_bars_bootstrap, 
            join_by(uebars_int == uneven_bars_int)) |> 
  left_join(womens_vault_bootstrap, 
            join_by(vault_int == womens_vault_int))

# consolidate duplicates. since they're mostly copies, this is okay
womens_full <- womens_full |> 
  group_by(gymnast, country) |> 
  summarize(floor_int = mean(floor_int,na.rm = T), 
            floor_bias = mean(w_floor_bias,na.rm = T),
            floor_se = mean(w_floor_std_error,na.rm = T),
            uneven_bars_int = mean(uebars_int,na.rm = T),
            uneven_bars_bias = mean(uneven_bars_bias,na.rm = T),
            uneven_bars_se = mean(uneven_bars_std_error,na.rm = T),
            vault_int = mean(vault_int,na.rm = T),
            vault_bias = mean(womens_vault_bias,na.rm = T),
            vault_se = mean(womens_vault_std_error,na.rm = T),
            bb_int = mean(bb_int,na.rm = T),
            bb_bias = mean(bb_bias,na.rm = T),
            bb_se = mean(bb_std_error,na.rm = T)) |> ungroup()

# replace the NaNs with the average of zero
womens_full <- womens_full |> 
  mutate(across(contains("_int"), ~ if_else(is.na(.), 0, .)))
rm(bb_bootstrap, uneven_bars_bootstrap, womens_floor_bootstrap, womens_vault_bootstrap)

# join mens data ----
# put NAs back, then omit them from computations
mens_full <- mens_full |> 
  mutate(across(where(is.numeric), ~ if_else(. == 0, NA, .)))

# join the data. there will be duplicate matches; that's okay
mens_full <- mens_full |> 
  left_join(mens_floor_bootstrap, 
            join_by(floor_int == mens_floor_int)) |> 
  left_join(pommel_horse_boostrap, 
            join_by(horse_int == pommel_horse_int)) |> 
  left_join(still_rings_bootstrap, 
            join_by(rings_int == still_rings_int)) |> 
  left_join(pbars_bootstrap, by = "pbars_int") |> 
  left_join(hbars_bootstrap, by = "hbars_int") |> 
  left_join(mens_vault_bootstrap, 
            join_by(vault_int == "mens_vault_int"))

# consolidate duplicates. since they're mostly copies, this is okay
mens_full <- mens_full |> 
  group_by(gymnast, country) |> 
  summarize(floor_int = mean(floor_int,na.rm = T), 
            floor_bias = mean(mens_floor_bias,na.rm = T),
            floor_se = mean(mens_floor_std_error,na.rm = T),
            hbars_int = mean(hbars_int,na.rm = T),
            hbars_bias = mean(hbars_bias,na.rm = T),
            hbars_se = mean(hbars_std_error,na.rm = T),
            vault_int = mean(vault_int,na.rm = T),
            vault_bias = mean(mens_vault_bias,na.rm = T),
            vault_se = mean(mens_vault_std_error,na.rm = T),
            pbars_int = mean(pbars_int,na.rm = T),
            pbars_bias = mean(pbars_bias,na.rm = T),
            pbars_se = mean(pbars_std_error,na.rm = T),
            still_rings_int = mean(rings_int, na.rm=T),
            still_rings_bias = mean(still_rings_bias, na.rm = T),
            still_rings_se = mean(still_rings_std_error, na.rm = T),
            pommel_horse_int = mean(horse_int, na.rm = T),
            pommel_horse_bias = mean(pommel_horse_bias, na.rm = T),
            pommel_horse_se = mean(pommel_horse_std_error,
                                          na.rm = T)) |> ungroup()

# replace the NaNs with the average of zero
mens_full <- mens_full |> 
  mutate(across(contains("_int"), ~ if_else(is.na(.), 0, .)))
rm(hbars_bootstrap, mens_floor_bootstrap, mens_vault_bootstrap, 
   pbars_bootstrap, pommel_horse_boostrap, still_rings_bootstrap)

# for standard errors and bias, perform knn imputation
mens_full <- kNN(mens_full, # have to do em manually smh
                 variable = c("floor_bias", "floor_se",
                              "hbars_bias", "hbars_se",
                              "vault_bias", "vault_se",
                              "pbars_bias", "pbars_se",
                              "still_rings_bias", "still_rings_se",
                              "pommel_horse_bias", "pommel_horse_se"),
                 imp_var = FALSE, useImputedDist = FALSE)

womens_full <- kNN(womens_full, # have to do em manually smh
                   variable = c("floor_bias", "floor_se",
                                "uneven_bars_bias", "uneven_bars_se",
                                "vault_bias", "vault_se",
                                "bb_bias", "bb_se"),
                   imp_var = FALSE, useImputedDist = FALSE)
# write!
write_csv(mens_full, "processed-data/mens_bootstrapped_results.csv")
write_csv(womens_full, "processed-data/womens_bootstrapped_results.csv")