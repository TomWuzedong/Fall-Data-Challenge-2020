library("dplyr")
library(tidyverse)
data_2016 <- read.csv("data_2016.csv", stringsAsFactors = FALSE)
data_2018 <- read.csv("data_2018.csv", stringsAsFactors = FALSE)
data_dictonary <- read.csv("data_dictionary.csv", stringsAsFactors = FALSE)
colnames(data_2016)
head(data_2016)



# dataframe with all target columns
data_2016_target <- data_2016 %>%
  select(YEAR, STATEFIP, AGE, SEX, RACE, VOREG, VOREGHOW, VOYNOTREG, VOTED, VOTEHOW, VOWHYNOT)

### Research Questions:
# 1. Compare the voting participation percentage across different races with the entire race
#    percentage across the nation. (variable: race, ages, year; mutate percent_"race")
data_2016_race <- data_2016_target %>%
  select(YEAR, AGE, SEX, RACE)
# mutate percent for each race (length(RACE == "race") / length(data_2016_race))
# then group by race
# join the dataset with population in each race in terms of total population


# 2. Analyzing the relatively more common reasons for people
#   who are in the least-participated races to not vote (Var: VOWHYNOT, VOYNOTREG)

# 3. Analyzing the voting methods preferences across (ages, races)