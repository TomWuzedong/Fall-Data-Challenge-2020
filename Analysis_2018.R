library("dplyr")
library(tidyverse)
library("plotly")
library(ggrepel)
library(forcats)
library(plotly)
library(maps)
library(mapproj)

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


# count the total pop by race
race_class_2018 <- data_2018_target %>%
  group_by(race) %>%
  summarize(
    total_num = n()
  )

# count the pop reg for vote by race
# (for those who did not vote in the most recent election)
reg_race_class_2018 <- data_2018_target %>%
  group_by(race) %>%
  filter(voreg == "Registered") %>%
  summarize(
    reg_num = n()
  )

# count the pop voted by race
#  (for those who were registered)
voted_race_class_2018 <- data_2018_target %>%
  group_by(race) %>%
  filter(voted == "Voted") %>%
  summarize(
    voted_num = n()
  )

# calculate the percentage of voted pop in total pop by race
voted_percent_total_2018 <- race_class_2018 %>%
  left_join(voted_race_class_2018, by="race") %>%
  mutate(
    voted_percent = voted_num / total_num
  )

# calculate the percentage of registerted pop grouped by race
reg_percent_total_2018 <- race_class_2018 %>%
  left_join(reg_race_class_2018, by="race") %>%
  mutate(
    reg_percent = reg_num / total_num
  )

# count the total pop by state
state_class_2018 <- 
  data_2018_target %>%
  group_by(state) %>%
  summarize(
    total_num = n()
  )

# count the pop reg for vote by state
# (for those who did not vote in the most recent election)
reg_state_class_2018 <- data_2018_target %>%
  group_by(state) %>%
  filter(voreg == "Registered") %>%
  summarize(
    reg_num = n()
  )

# count the pop voted by state
#  (for those who were registered)
voted_state_class_2018 <- data_2018_target %>%
  group_by(state) %>%
  filter(voted == "Voted") %>%
  summarize(
    voted_num = n()
  )

# calculate the percentage of voted pop in total pop by state
voted_percent_total_state_2018 <- state_class_2018 %>%
  left_join(voted_state_class_2018, by="state") %>%
  mutate(
    voted_percent = voted_num / total_num
  )

# calculate the percentage of registerted pop grouped by state
reg_percent_total_state_2018 <- state_class_2018 %>%
  left_join(reg_state_class_2018, by="state") %>%
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
  left_join(voted_percent_total_2018, by="race") %>%
  arrange(desc(total_num))


data_2018_race[is.na(data_2018_race)] <- 0

turnout_barplot_2018 <- 
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
turnout_barplot_interactive_2018 <- 
  ggplotly(turnout_barplot_2018) %>%
  layout(
    title = "Percent of Surveyed Population who Voted in each Race in 2018",                  
    xaxis = list(title = "Percentage of Population that voted"), 
    yaxis = list(title = "Total Surveyed Population")
  )

turnout_sex_2018 <- turnout_barplot_2018 + facet_wrap(~sex) + theme(axis.title.x=element_blank(),
                                                          axis.title.y = element_blank())
turnout_sex_interactive_2018 <- 
  ggplotly(turnout_sex_2018) %>%
  layout(
    title = "Percent of Surveyed Population who Voted by gender in 2018",                  
    xaxis = list(title = "Percentage of Population that voted"), 
    yaxis = list(title = "Total Surveyed Population")
  )


# 2. Compare the voting participation percentage across different state across the nation.

data_2018_state <- voted_percent_total_state_2018 %>%
  select(state, voted_percent) %>%
  mutate(state = tolower(state))


# Load a shapefile of U.S. states using ggplot's `map_data()` function
state_shape_2018 <- map_data("state") %>%
  rename(state = region) %>%
  left_join(data_2018_state, by="state")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

turnout_state_2018 <- 
  ggplot(state_shape_2018) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = voted_percent),
    color = "white", 
    size = .1       
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "white", high = "Red") +
  labs(title = "Percent of Surveyed Population who Voted by state in 2018",
       fill = "voted_percent") +
  blank_theme

top_10_state_2018 <- voted_percent_total_state_2018 %>%
  top_n(10, voted_percent) %>%
  arrange(desc(voted_percent))



# 3. Analyzing the relatively more common reasons for people
#   who are in the least-participated races to not vote (Var: VOWHYNOT, VOYNOTREG)
data_2018_nvote <- data_2018_target %>%
  select(year, race, voted, vowhynot) %>%
  filter(voted != "Voted" & vowhynot != "NIU")
  
colors <- 
  c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

nvote_reason_2018 <- 
  plot_ly(data_2018_nvote, labels = ~vowhynot, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               insidetextorientation='radial',
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE)

nvote_reason_2018 <- 
  nvote_reason_2018 %>% layout(title = 'Commonm Reasons for not voting in 2018',
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

least_five_race_turnout_2018 <- voted_percent_total_2018 %>%
  arrange(voted_percent) %>%
  top_n(-5, voted_percent)

# White-Black-Asian
wba_2018 <- data_2018_nvote %>%
  filter(race == "White-Black-Asian")

nvote_reason_wba_2018 <- 
  plot_ly(wba_2018, labels = ~vowhynot, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nvote_reason_wba_2018 <- 
  nvote_reason_wba_2018 %>% layout(title = 'Commonm Reasons for White-Black-Asian not voting in 2018',
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# White-American Indian-Asian
waia_2018 <- data_2018_nvote %>%
  filter(race == "White-American Indian-Asian")

nvote_reason_waia_2018 <- 
  plot_ly(waia_2018, labels = ~vowhynot, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          # insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nvote_reason_waia_2018 <- 
  nvote_reason_waia_2018 %>% layout(title = 'Commonm Reasons for White-American Indian-Asian not voting in 2018',
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# Asian-Hawaiian/Pacific Islander
ahpi_2018 <- data_2018_nvote %>%
  filter(race == "Asian-Hawaiian/Pacific Islander")

nvote_reason_ahpi_2018 <- 
  plot_ly(ahpi_2018, labels = ~vowhynot, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          # insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nvote_reason_ahpi_2018 <- 
  nvote_reason_ahpi_2018 %>% layout(title = 'Commonm Reasons for Asian-Hawaiian/Pacific Islander not voting in 2018',
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



# American Indian/Aleut/Eskimo
aiae_2018 <- data_2018_nvote %>%
  filter(race == "American Indian/Aleut/Eskimo")

nvote_reason_aiae_2018 <- 
  plot_ly(aiae_2018, labels = ~vowhynot, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nvote_reason_aiae_2018 <- 
  nvote_reason_aiae_2018 %>% layout(title = 'Commonm Reasons for American Indian/Aleut/Eskimo not voting in 2018',
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               legend = list(size=50))





data_2018_nreg <- data_2018_target %>%
  select(year, race, voreg, voynotreg) %>%
  filter(voreg != "Registered" & voynotreg != "NIU")

nreg_reason_2018 <- 
  plot_ly(data_2018_nreg, labels = ~voynotreg, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nreg_reason_2018 <- 
  nreg_reason_2018 %>% layout(title = 'Commonm Reasons for not registering to vote in 2018',
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# 4. Analyzing the voting methods preferences across (ages, races)