library("dplyr")
library(tidyverse)
library("plotly")
library(ggrepel)
library(forcats)
# library(scales)
data_2018 <- read.csv("Data/data_2018.csv", stringsAsFactors = FALSE)
data_dictonary <- read.csv("Data/data_dictionary.csv", stringsAsFactors = FALSE)


# dataframe with all target columns
data_2018_target <- data_2018 %>%
  select(YEAR, STATEFIP, AGE, SEX, RACE, VOREG, VOREGHOW, VOYNOTREG, VOTED, VOTEHOW, VOWHYNOT)

data_dictonary <- data_dictonary %>%
  select(-meta)

# specify the state label
state_2018 <- data_dictonary[3:77, ] %>%
  select(value, label)
data_2018_target <- data_2018_target %>%
  left_join(state_2018, by=c("STATEFIP" = "value"))

# specify the race label
race_2018 <- data_dictonary[94:122, ] %>%
  select(value, label)
data_2018_target <- data_2018_target %>%
  left_join(race_2018, by=c("RACE" = "value"))

# specify the sex label
sex_2018 <- data_dictonary[90:92, ] %>%
  select(value, label)
data_2018_target <- data_2018_target %>%
  left_join(sex_2018, by=c("SEX" = "value"))


# specify the voreg label
voreg_2018 <- data_dictonary[278:283, ] %>%
  select(value, label)
data_2018_target <- data_2018_target %>%
  left_join(voreg_2018, by=c("VOREG" = "value"))

# specify the voreghow label
voreghow_2018 <- data_dictonary[257:269, ] %>%
  select(value, label)
data_2018_target <- data_2018_target %>%
  left_join(voreghow_2018, by=c("VOREGHOW" = "value"))

# specify the VOYNOTREG label
voynotreg_2018 <- data_dictonary[229:241, ] %>%
  select(value, label)
data_2018_target <- data_2018_target %>%
  left_join(voynotreg_2018, by=c("VOYNOTREG" = "value"))

# specify the VOTED label
voted_2018 <- data_dictonary[271:276, ] %>%
  select(value, label)
data_2018_target <- data_2018_target %>%
  left_join(voted_2018, by=c("VOTED" = "value"))

# specify the VOTEHOW label
votehow_2018 <- data_dictonary[243:248, ] %>%
  select(value, label)
data_2018_target <- data_2018_target %>%
  left_join(votehow_2018, by=c("VOTEHOW" = "value"))

# specify the VOWHYNOT label
vowhynot_2018 <- data_dictonary[213:227, ] %>%
  select(value, label)
data_2018_target <- data_2018_target %>%
  left_join(vowhynot_2018, by=c("VOWHYNOT" = "value"))



data_2018_target <- data_2018_target %>%
  select(-c(STATEFIP, SEX, RACE, VOREG, VOREGHOW, VOYNOTREG, VOTED, VOTEHOW, VOWHYNOT))

colnames(data_2018_target) <- 
  c("year", "age", "state", "race", "sex", "voreg", "voreghow", "voynotreg", "voted", "votehow", "vowhynot")
colnames(data_2018_target)
head(data_2018_target)

# count the total pop by race
race_class <- data_2018_target %>%
  group_by(race) %>%
  summarize(
    total_num = n()
  )

# count the pop reg for vote by race
# (for those who did not vote in the most recent election)
reg_race_class <- data_2018_target %>%
  group_by(race) %>%
  filter(voreg == "Registered") %>%
  summarize(
    reg_num = n()
  )

# count the pop voted by race
#  (for those who were registered)
voted_race_class <- data_2018_target %>%
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
# mutate percent for each race (length(RACE == "race") / length(data_2018_race))
# then group by race
# join the dataset with population in each race in terms of total population
data_2018_race <- data_2018_target %>%
  select(year, age, sex, race, voted)

data_2018_race <- data_2018_race %>%
  left_join(voted_percent_total, by="race") %>%
  arrange(desc(total_num))

turnout_barplot <- 
  ggplot(data_2018_race) +
  geom_col(mapping = aes(x = fct_rev(fct_infreq(race)), 
                         y = total_num, fill = voted), 
          position = "fill") +
  # geom_text(aes(x = race,
  #               y = voted_num,
  #               label=scales::percent(voted_percent))) +
  # scale_y_continuous(labels = scales::percent) +
  coord_flip()
  # labs(
  #   title = "Percent of Surveyed Population who Voted in each Race in 2018",
  #   subtitle = "in the descending order of total surveyed population by race",
  #   y = "Total Surveyed Population", 
  #   x = "Percentage of Population that voted"
  # )
turnout_barplot_interactive <- 
  ggplotly(turnout_barplot) %>%
  layout(
    title = "Percent of Surveyed Population who Voted in each Race in 2018",                  
    xaxis = list(title = "Percentage of Population that voted"), 
    yaxis = list(title = "Total Surveyed Population")
  )

# 2. Analyzing the relatively more common reasons for people
#   who are in the least-participated races to not vote (Var: VOWHYNOT, VOYNOTREG)

# 3. Analyzing the voting methods preferences across (ages, races)