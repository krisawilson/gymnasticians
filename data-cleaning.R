# load libraries
library(lubridate)
library(tidyverse)
library(tools)

# read in data
data_2017_2021 <- read_csv('raw-data/data_2017_2021.csv')
data_2022_2023 <- read_csv('raw-data/data_2022_2023.csv')

# clean 2017 - 2021 data
clean_2017_2021 <- data_2017_2021 |> 
  reframe(
    last_name = toTitleCase(tolower(LastName)),
    first_name = toTitleCase(tolower(FirstName)),
    gender = if_else(Gender == 'w', 'F', 'M'),
    country = Country,
    date = as.Date(
      case_when(
        Date == 'SUN 25 JUL 2021' ~ '2021-07-25',
        Date == '25 July 2021' ~ '2021-07-25',
        Date == '1 Aug 2021' ~ '2021-08-01',
        Date == 'THU 29 JUL 2021' ~ '2021-07-29',
        TRUE ~ 'induce error'
      )
    ),
    competition = Competition,
    round = case_when(
      Round == 'AAqual' ~ 'aa_qual',
      Round == 'TeamQual' ~ 'team_qual',
      Round == 'TeamFinal' ~ 'team_final',
      Round == 'AAfinal' ~ 'aa_final',
      TRUE ~ Round
    ),
    location = Location,
    apparatus = Apparatus,
    rank = Rank,
    difficulty = D_Score,
    execution = E_Score,
    penalty = case_when(
      is.na(Penalty) ~ 0,
      TRUE ~ Penalty
    ),
    score = Score
  )

# clean 2022 - 2023 data
clean_2022_2023 <- data_2022_2023 |> 
  reframe(
    last_name = toTitleCase(tolower(LastName)),
    first_name = toTitleCase(tolower(FirstName)),
    gender = if_else(Gender == 'w', 'F', 'M'),
    country = Country,
    date = dmy(
      case_when(
        grepl(' - ', Date) ~ sub('.* - ', '', Date),
        grepl('-', Date) ~ sub('.*-', '', Date),
        TRUE ~ 'induce error'
      )
    ),
    competition = Competition,
    round = case_when(
      Round == 'AAqual' ~ 'aa_qual',
      Round == 'TeamQual' ~ 'team_qual',
      Round == 'TeamFinal' ~ 'team_final',
      Round == 'AAfinal' ~ 'aa_final',
      TRUE ~ Round
    ),
    location = Location,
    apparatus = Apparatus,
    rank = Rank,
    difficulty = D_Score,
    execution = E_Score,
    penalty = case_when(
      is.na(Penalty) ~ 0,
      TRUE ~ Penalty
    ),
    score = Score
  )

# combine datasets
final_data <- bind_rows(clean_2017_2021, clean_2022_2023)

# write final dataset
write_csv(final_data, 'processed-data/final_data.csv')