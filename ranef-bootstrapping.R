# distribution of player effects ----
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lme4))

# first, get list of athletes who competed in a particular event ----
#final_data <- read_csv("processed-data/final_data.csv")

# create a dummy var, set to 1 if final round, 0 otherwise
#subset_data <- final_data |> 
 # mutate(round_dummy = if_else(grepl("final", round), 1, 0)) |> 
  # get only vars we need
  #select(full_name, country, gender, round_dummy, apparatus, score)
#rm(final_data)
# filter by gender since the apparatuses differ
#bros <- subset_data |> filter(gender == "M")
#sisters <- subset_data |> filter(gender == "F")
#rm(subset_data)
#bros <- subset_data |> filter(!is.na(score))
#sisters <- sisters |> filter(!is.na(score))
# load env ----
load("processed-data/mlm-env.RData")
rm(mens_vars, womens_vars)
ranef_bootstrap <- function(me_model, nsim = 1000, seed = 1501) {
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

  # turn estimates into data frame
  me_model_results <- as.data.frame(me_model_summary) # for validation
  return(me_model_results)
}

# apply function! women first ----
bb_bootstrap <- ranef_bootstrap(womens_bb)

rm(womens_bb)

womens_floor_bootstrap <- ranef_bootstrap(womens_floor)

rm(womens_floor)

uneven_bars_bootstrap <- ranef_bootstrap(womens_uebars)

rm(womens_uebars)

womens_vault_bootstrap <- ranef_bootstrap(womens_vault)

rm(womens_vault)
# men ----
mens_floor_bootstrap <- ranef_bootstrap(mens_floor)

rm(mens_floor)

pbars_bootstrap <- ranef_bootstrap(mens_pbars)

rm(mens_pbars)

hbars_bootstrap <- ranef_bootstrap(mens_hbars)

rm(mens_hbars)

mens_vault_bootstrap <- ranef_bootstrap(mens_vault)

rm(mens_vault)

still_rings_bootstrap <- ranef_bootstrap(rings)

rm(rings)

pommel_horse_boostrap <- ranef_bootstrap(horse)

rm(horse)

# writing ----
write_csv(mens_floor_bootstrap, "processed-data/mens-bootstrapped-results/mens_floor.csv")
write_csv(pbars_bootstrap, "processed-data/mens-bootstrapped-results/pbars.csv")
write_csv(mens_vault_bootstrap, "processed-data/mens-bootstrapped-results/mens_vault.csv")
write_csv(hbars_bootstrap, "processed-data/mens-bootstrapped-results/hbars.csv")
write_csv(still_rings_bootstrap, "processed-data/mens-bootstrapped-results/still_rings.csv")
write_csv(pommel_horse_boostrap, "processed-data/mens-bootstrapped-results/pommel_horse.csv")
write_csv(womens_floor_bootstrap, "processed-data/womens-bootstrapped-results/womens_floor.csv")
write_csv(bb_bootstrap, "processed-data/womens-bootstrapped-results/beam.csv")
write_csv(womens_vault_bootstrap, "processed-data/womens-bootstrapped-results/womens_vault.csv")
write_csv(uneven_bars_bootstrap, "processed-data/womens-bootstrapped-results/uneven_bars.csv")