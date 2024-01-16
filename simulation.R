# load libraries, data ####

# load libraries
library(tidyverse)

# load data ####
mens_data <- read_csv('processed-data/mens_intercepts.csv')
womens_data <- read_csv('processed-data/womens_intercepts.csv')

# generate all possible team combinations ####

# men's
mens_teams <- t(combn(mens_data$gymnast, 5))

# women's
womens_teams <- t(combn(womens_data$gymnast, 5))

# event function for simulation ####

event <- function(data, event_name, rep = 1000) {
  
  # initialize results data frame
  results <- data |> 
    select(gymnast)
  
  # loop through iterations
  for (i in 1:rep) {
    # set seed
    set.seed(i)
    # column names
    column_names <- paste(event_name, c('_mean_diff', '_sd_diff', '_mean_exec', '_sd_exec'), sep = '')
    # calculate difficulty
    difficulty <- mapply(function(mean, sd) min(rnorm(1, mean, sd), 10),
                         mean = data[[column_names[1]]],
                         sd = data[[column_names[2]]])
    # calculate execution
    execution <- mapply(function(mean, sd) min(rnorm(1, mean, sd), 10),
                        mean = data[[column_names[3]]],
                        sd = data[[column_names[4]]])
    # calculate final score
    score <- difficulty + execution
    
    # add scores to data frame
    results <- results |> 
      mutate(!!paste('score_', i, sep = '') := score)
  }
  # return results
  return(results)
}


# men's competition ####

# floor exercise
mens_floor <- event(mens_data, 'floor', rep = 1000)

# pommel horse
mens_horse <- event(mens_data, 'horse', rep = 1000)

# rings
mens_rings <- event(mens_data, 'rings', rep = 1000)

# vault
mens_vault <- event(mens_data, 'vault', rep = 1000)

# parallel bars
mens_pbars <- event(mens_data, 'pbars', rep = 1000)

# horizontal bar
mens_hbars <- event(mens_data, 'hbars', rep = 1000)

# women's competition ####

# vault
womens_vault <- event(womens_data, 'vault', rep = 1000)

# uneven bars
womens_uebars <- event(womens_data, 'uebars', rep = 1000)

# balance beam
womens_bb <- event(womens_data, 'bb', rep = 1000)

# floor exercise
womens_floor <- event(womens_data, 'floor', rep = 1000)