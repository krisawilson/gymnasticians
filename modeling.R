# (cooking in progress...)
# packages + data reading ####
suppressPackageStartupMessages(library(tidyverse))
final_data <- read_csv("processed-data/final_data.csv")

# create a dummy var, set to 1 if final round, 0 otherwise
subset_data <- final_data |> 
  mutate(round_dummy = if_else(grepl("final", round), 1, 0)) |> 
  # get only vars we need
  select(full_name, gender, round_dummy, apparatus, score)

men <- subset_data |> filter(gender == "M")
women <- subset_data |> filter(gender == "F")

# modeling ####
# let's think... full_name is a random effect. Score is what we want to predict. Round (qual vs final) is a fixed effect. We'll start with apparatus as a fixed effect, but test it as a random effect potentially. 
