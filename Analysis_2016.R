library("dplyr")
library(tidyverse)
data_2016 <- read.csv("Data/data_2016.csv", stringsAsFactors = FALSE)
data_dictonary <- read.csv("Data/data_dictionary.csv", stringsAsFactors = FALSE)


# dataframe with all target columns
data_2016_target <- data_2016 %>%
  select(YEAR, STATEFIP, AGE, SEX, RACE, VOREG, VOREGHOW, VOYNOTREG, VOTED, VOTEHOW, VOWHYNOT)

data_dictonary <- data_dictonary %>%
  select(-meta)

# specify the state label
state_2016 <- data_dictonary[3:77, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(state_2016, by=c("STATEFIP" = "value"))

# specify the race label
race_2016 <- data_dictonary[94:122, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(race_2016, by=c("RACE" = "value"))

# specify the sex label
sex_2016 <- data_dictonary[90:92, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(sex_2016, by=c("SEX" = "value"))


# specify the voreg label
voreg_2016 <- data_dictonary[278:283, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(voreg_2016, by=c("VOREG" = "value"))

# specify the voreghow label
voreghow_2016 <- data_dictonary[257:269, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(voreghow_2016, by=c("VOREGHOW" = "value"))

# specify the VOYNOTREG label
voynotreg_2016 <- data_dictonary[229:241, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(voynotreg_2016, by=c("VOYNOTREG" = "value"))

# specify the VOTED label
voted_2016 <- data_dictonary[271:276, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(voted_2016, by=c("VOTED" = "value"))

# specify the VOTEHOW label
votehow_2016 <- data_dictonary[243:248, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(votehow_2016, by=c("VOTEHOW" = "value"))

# specify the VOWHYNOT label
vowhynot_2016 <- data_dictonary[213:227, ] %>%
  select(value, label)
data_2016_target <- data_2016_target %>%
  left_join(vowhynot_2016, by=c("VOWHYNOT" = "value"))



data_2016_target <- data_2016_target %>%
  select(-c(STATEFIP, SEX, RACE, VOREG, VOREGHOW, VOYNOTREG, VOTED, VOTEHOW, VOWHYNOT))

colnames(data_2016_target) <- 
  c("year", "age", "state", "race", "sex", "voreg", "voreghow", "voynotreg", "voted", "votehow", "vowhynot")
colnames(data_2016_target)
head(data_2016_target)

# count the total pop by race
race_class <- data_2016_target %>%
  group_by(race) %>%
  summarize(
    total_num = n()
  )

# count the pop reg for vote by race
# (for those who did not vote in the most recent election)
reg_race_class <- data_2016_target %>%
  group_by(race) %>%
  filter(voreg == "Registered") %>%
  summarize(
    reg_num = n()
  )

# count the pop voted by race
#  (for those who were registered)
voted_race_class <- data_2016_target %>%
  group_by(race) %>%
  filter(voted == "Voted") %>%
  summarize(
    voted_num = n()
  )

# calculate the percentage of voted pop in total pop by race
voted_percent_total <- race_class %>%
  left_join(voted_race_class, by="race") %>%
  mutate(
    voted_percent = voted_num / total_num
  )

# calculate the percentage of registerted pop grouped by race
reg_percent_total <- race_class %>%
  left_join(reg_race_class, by="race") %>%
  mutate(
    reg_percent = reg_num / total_num
  )

### Research Questions:
# 1. Compare the voting participation percentage across different races with the entire race
#    percentage across the nation. (variable: race, ages, year; mutate percent_"race")
# mutate percent for each race (length(RACE == "race") / length(data_2016_race))
# then group by race
# join the dataset with population in each race in terms of total population
data_2016_race <- data_2016_target %>%
  select(year, age, sex, race)

# 2. Analyzing the relatively more common reasons for people
#   who are in the least-participated races to not vote (Var: VOWHYNOT, VOYNOTREG)

# 3. Analyzing the voting methods preferences across (ages, races)