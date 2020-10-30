library("dplyr")
library(tidyverse)
data_2016 <- read.csv("Data/data_2016.csv", stringsAsFactors = FALSE)
data_2018 <- read.csv("Data/data_2018.csv", stringsAsFactors = FALSE)
data_dictonary <- read.csv("Data/data_dictionary.csv", stringsAsFactors = FALSE)


# dataframe with all target columns
data_2016_target <- data_2016 %>%
  select(YEAR, STATEFIP, AGE, SEX, RACE, VOREG, VOREGHOW, VOYNOTREG, VOTED, VOTEHOW, VOWHYNOT)

data_dictonary <- data_dictonary %>%
  select(-meta)

# specify the race label
data_dictonary_state <- data_dictonary[3:77, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(data_dictonary_state, by=c("STATEFIP" = "value"))

# specify the race label
data_dictonary_race <- data_dictonary[94:122, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(data_dictonary_race, by=c("RACE" = "value"))

# specify the sex label
data_dictonary_sex <- data_dictonary[90:92, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(data_dictonary_sex, by=c("SEX" = "value"))


# specify the voreg label
data_dictonary_voreg <- data_dictonary[278:283, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(data_dictonary_voreg, by=c("VOREG" = "value"))

# specify the voreghow label
data_dictonary_voreghow <- data_dictonary[257:269, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(data_dictonary_voreghow, by=c("VOREGHOW" = "value"))

# specify the VOYNOTREG label
data_dictonary_voynotreg <- data_dictonary[229:241, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(data_dictonary_voynotreg, by=c("VOYNOTREG" = "value"))

# specify the VOTED label
data_dictonary_voted <- data_dictonary[271:276, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(data_dictonary_voted, by=c("VOTED" = "value"))

# specify the VOTEHOW label
data_dictonary_votehow <- data_dictonary[243:248, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(data_dictonary_votehow, by=c("VOTEHOW" = "value"))

# specify the VOWHYNOT label
data_dictonary_vowhynot <- data_dictonary[213:227, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(data_dictonary_vowhynot, by=c("VOWHYNOT" = "value"))



data_2016_target <- data_2016_target %>%
  select(-c(STATEFIP, SEX, RACE, VOREG, VOREGHOW, VOYNOTREG, VOTED, VOTEHOW, VOWHYNOT))

colnames(data_2016_target) <- 
  c("year", "age", "state", "race", "sex", "voreg", "voreghow", "voynotreg", "voted", "votehow", "vowhynot")
colnames(data_2016_target)
head(data_2016_target)

### Research Questions:
# 1. Compare the voting participation percentage across different races with the entire race
#    percentage across the nation. (variable: race, ages, year; mutate percent_"race")
# mutate percent for each race (length(RACE == "race") / length(data_2016_race))
# then group by race
# join the dataset with population in each race in terms of total population
data_2016_race <- data_2016_target %>%
  select(YEAR, AGE, SEX, RACE)

# 2. Analyzing the relatively more common reasons for people
#   who are in the least-participated races to not vote (Var: VOWHYNOT, VOYNOTREG)

# 3. Analyzing the voting methods preferences across (ages, races)