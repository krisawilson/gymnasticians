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

# manually clean up names: 

##remove special characters, fix errors, change nicknames
final_data <- final_data |> 
  mutate(last_name = case_when(
    last_name == "Masonstephens" ~ "Mason Stephens",
    last_name == "Modoianu-Zseder" ~ "Modoianu",
    last_name == "Escandón Marín" ~ "Escandon",
    last_name == "Fuallen" ~ "Fu Allen",
    last_name == "Grünberg" ~ "Gruenburg",
    last_name == "Guimarães" ~ "Guimaraes",
    last_name == "Hörr" ~ "Horr",
    last_name == "Villafañe" ~ "Villafane",
    TRUE ~ last_name),
    first_name = if_else(last_name == "Black", "Elsabeth",
             case_when(
               first_name == "Valentin" ~ "Valentina",
               first_name == "d Amato" ~ "D'Amato",
               first_name == "Pin-Ju" ~ "Pin Ju",
               first_name == "Yi-Chun" ~ "Yi Chun",
               first_name == "Yi-Chen" ~ "Yi Chen",
               first_name == "Liu Hsiang-Han" ~ "Hsiang Han",
               first_name == "Hua-Tien" ~ "Hua Tien",
               first_name == "Ga-Ram" ~ "Garam",
               first_name == "Matthew" ~ "Matt",
               first_name == "Fabián" ~ "Fabian",
               first_name == "Samual" ~ "Sam",
               first_name == "Yuan-Hsi" ~ "Yuan Hsi",
               first_name == "Yo-Seop" ~ "Yoseop",
               first_name %in% c("Chih-Kai", "Chih") ~ "Chih Kai",
               first_name == "Guan-Yi" ~ "Guan Yi",
               first_name == "Wei-Sheng" ~ "Wei Sheng",
               first_name == "Frankie" ~ "Man Hin",
               TRUE ~ first_name)))

# shorten names and create full name column. also fix
# apparatus input error from British Commonwealth Games
final_data <- final_data |> 
  mutate(last_name = word(last_name),
         first_name = word(first_name),
         competition = 
           if_else(apparatus == "hb", "HB", apparatus),
         full_name = paste(first_name, last_name, sep = "_"))
# write final dataset
write_csv(final_data, 'processed-data/final_data.csv')
