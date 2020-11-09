library("dplyr")
library(tidyverse)
library("plotly")
library(ggrepel)
library(forcats)
library(maps)
library(mapproj)
library(plotly)
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


# count the total pop by race
race_class_2016 <- data_2016_target %>%
  group_by(race) %>%
  summarize(
    total_num = n()
  )

# count the pop reg for vote by race
# (for those who did not vote in the most recent election)
reg_race_class_2016 <- data_2016_target %>%
  group_by(race) %>%
  filter(voreg == "Registered") %>%
  summarize(
    reg_num = n()
  )

# count the pop voted by race
#  (for those who were registered)
voted_race_class_2016 <- data_2016_target %>%
  group_by(race) %>%
  filter(voted == "Voted") %>%
  summarize(
    voted_num = n()
  )

# calculate the percentage of voted pop in total pop by race
voted_percent_total_2016 <- race_class_2016 %>%
  left_join(voted_race_class_2016, by="race") %>%
  mutate(
    voted_percent = voted_num / total_num
  )

# calculate the percentage of registerted pop grouped by race
reg_percent_total_2016 <- race_class_2016 %>%
  left_join(reg_race_class_2016, by="race") %>%
  mutate(
    reg_percent = reg_num / total_num
  )


# count the total pop by state
state_class_2016 <- 
  data_2016_target %>%
  group_by(state) %>%
  summarize(
    total_num = n()
  )

# count the pop reg for vote by state
# (for those who did not vote in the most recent election)
reg_state_class_2016 <- data_2016_target %>%
  group_by(state) %>%
  filter(voreg == "Registered") %>%
  summarize(
    reg_num = n()
  )

# count the pop voted by state
#  (for those who were registered)
voted_state_class_2016 <- data_2016_target %>%
  group_by(state) %>%
  filter(voted == "Voted") %>%
  summarize(
    voted_num = n()
  )

# calculate the percentage of voted pop in total pop by state
voted_percent_total_state_2016 <- state_class_2016 %>%
  left_join(voted_state_class_2016, by="state") %>%
  mutate(
    voted_percent = voted_num / total_num
  )

# calculate the percentage of registerted pop grouped by state
reg_percent_total_state_2016 <- state_class_2016 %>%
  left_join(reg_state_class_2016, by="state") %>%
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
  select(year, age, sex, race, voted)

data_2016_race <- data_2016_race %>%
  left_join(voted_percent_total_2016, by="race") %>%
  arrange(desc(total_num))

data_2016_race[is.na(data_2016_race)] <- 0

turnout_barplot_2016 <- 
  ggplot(data_2016_race) +
  geom_col(mapping = aes(x = fct_rev(fct_infreq(race)), y = total_num, fill = voted),
           position = "fill") +
  # geom_label_repel(data = data_2016_race %>% group_by(race), 
  # mapping = aes(x = fct_rev(fct_infreq(race)), y = total_num, label = voted_percent)) +
  coord_flip() + 
  labs(
     title = "Percent of Surveyed Population who Voted in each Race in 2016",
     subtitle = "in the descending order of total surveyed population by race",
     y = "Total Surveyed Population", 
     x = "Percentage of Population that voted"
  )
turnout_barplot_interactive_2016 <- 
  ggplotly(turnout_barplot_2016) %>%
  layout(
    title = "Percent of Surveyed Population who Voted in each Race in 2016",                  
    xaxis = list(title = "Percentage of Population that voted"), 
    yaxis = list(title = "Total Surveyed Population")
  )


turnout_sex_2016 <- turnout_barplot_2016 + facet_wrap(~sex) + theme(axis.title.x=element_blank(),
                                                          axis.title.y = element_blank())
turnout_sex_interactive_2016 <- 
  ggplotly(turnout_sex_2016) %>%
  layout(
    title = "Percent of Surveyed Population who Voted by gender in 2016",                  
    xaxis = list(title = "Percentage of Population that voted"), 
    yaxis = list(title = "Total Surveyed Population")
  )

# 2. Compare the voting participation percentage across different state across the nation.

data_2016_state <- voted_percent_total_state_2016 %>%
  select(state, voted_percent) %>%
  mutate(state = tolower(state))


# Load a shapefile of U.S. states using ggplot's `map_data()` function
state_shape_2016 <- map_data("state") %>%
  rename(state = region) %>%
  left_join(data_2016_state, by="state")

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

turnout_state_2016 <- 
  ggplot(state_shape_2016) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = voted_percent),
    color = "white", 
    size = .1       
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "white", high = "Red") +
  labs(title = "Percent of Surveyed Population who Voted by state in 2016",
       fill = "voted_percent") +
  blank_theme

turnout_state_2016_interactive <- ggplotly(turnout_state_2016)

top_10_state_2016 <- voted_percent_total_state_2016 %>%
  top_n(10, voted_percent) %>%
  arrange(desc(voted_percent))
  

# 3. Analyzing the relatively more common reasons for people
#   who are in the least-participated races to not vote (Var: VOWHYNOT, VOYNOTREG)

data_2016_nvote <- data_2016_target %>%
  select(year, race, voted, vowhynot) %>%
  filter(voted != "Voted" & vowhynot != "NIU")

colors <- 
  c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

nvote_reason_2016 <- 
  plot_ly(data_2016_nvote, labels = ~vowhynot, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nvote_reason_2016 <- 
  nvote_reason_2016 %>% layout(title = 'Commonm Reasons for not voting in 2016',
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

least_five_race_turnout_2016 <- voted_percent_total_2016 %>%
  arrange(voted_percent) %>%
  top_n(-5, voted_percent)

# White
white_2016 <- data_2016_nvote %>%
  filter(race == "White")

nvote_reason_white_2016 <- 
  plot_ly(white_2016, labels = ~vowhynot, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nvote_reason_white_2016 <- 
  nvote_reason_white_2016 %>% layout(title = 'Commonm Reasons for White not voting in 2016',
                                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Asian
asian_2016 <- data_2016_nvote %>%
  filter(race == "Asian only")

nvote_reason_asian_2016 <- 
  plot_ly(asian_2016, labels = ~vowhynot, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nvote_reason_asian_2016 <- 
  nvote_reason_asian_2016 %>% layout(title = 'Commonm Reasons for Asian not voting in 2016',
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Black/Negro
black_2016 <- data_2016_nvote %>%
  filter(race == "Black/Negro")

nvote_reason_black_2016 <- 
  plot_ly(black_2016, labels = ~vowhynot, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nvote_reason_black_2016 <- 
  nvote_reason_black_2016 %>% layout(title = 'Commonm Reasons for Black not voting in 2016',
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# American Indian-Asian
aia_2016 <- data_2016_nvote %>%
  filter(race == "American Indian-Asian")

nvote_reason_aia_2016 <- 
  plot_ly(aia_2016, labels = ~vowhynot, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nvote_reason_aia_2016 <- 
  nvote_reason_aia_2016 %>% layout(title = 'Commonm Reasons for American Indian-Asian not voting in 2016',
                                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# American Indian/Aleut/Eskimo
aiae_2016 <- data_2016_nvote %>%
  filter(race == "American Indian/Aleut/Eskimo")

nvote_reason_aiae_2016 <- 
  plot_ly(aiae_2016, labels = ~vowhynot, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          # insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nvote_reason_aiae_2016 <- 
  nvote_reason_aiae_2016 %>% layout(title = 'Commonm Reasons for American Indian/Aleut/Eskimo not voting in 2016',
                                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



# Hawaiian/Pacific Islander only
hpio_2016 <- data_2016_nvote %>%
  filter(race == "Hawaiian/Pacific Islander only")

nvote_reason_hpio_2016 <- 
  plot_ly(hpio_2016, labels = ~vowhynot, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nvote_reason_hpio_2016 <- 
  nvote_reason_hpio_2016 %>% layout(title = 'Commonm Reasons for Hawaiian/Pacific Islander only not voting in 2016',
                                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                    legend = list(size=50))


# White-Asian-Hawaiian/Pacific Islander
wahpi_2016 <- data_2016_nvote %>%
  filter(race == "White-Asian-Hawaiian/Pacific Islander")

nvote_reason_wahpi_2016 <- 
  plot_ly(wahpi_2016, labels = ~vowhynot, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nvote_reason_wahpi_2016 <- 
  nvote_reason_wahpi_2016 %>% layout(title = 'Commonm Reasons for White-Asian-Hawaiian/Pacific Islander not voting in 2016',
                                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                    legend = list(size=50))



data_2016_nreg <- data_2016_target %>%
  select(year, race, voreg, voynotreg) %>%
  filter(voreg != "Registered" & voynotreg != "NIU")

nreg_reason_2016 <- 
  plot_ly(data_2016_nreg, labels = ~voynotreg, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          insidetextorientation='radial',
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1)),
          #The 'pull' attribute can also be used to create space between the sectors
          showlegend = FALSE)

nreg_reason_2016 <- 
  nreg_reason_2016 %>% layout(title = 'Commonm Reasons for not registering to vote in 2016',
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



# 4. Analyzing the voting methods preferences across (state, races)
states_2016 <- data_2016_target %>%
  group_by(state) %>%
  summarize(
    vote_in_person = sum(votehow == "In person"),
    vote_by_mail = sum(votehow == "By mail")
  ) %>%
  mutate(
    main_voting_method = (vote_in_person > vote_by_mail)
  ) %>%
  mutate(state = tolower(state))

states_2016$main_voting_method[states_2016$main_voting_method == "TRUE"] <- "In-Person"
states_2016$main_voting_method[states_2016$main_voting_method == "FALSE"] <- "By mail"

vote_method_2016 <- map_data("state") %>%
  rename(state = region) %>%
  left_join(states_2016, by="state")

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

vote_method_state_2016 <- 
  ggplot(vote_method_2016) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = main_voting_method),
    color = "white", 
    size = .1       
  ) +
  coord_map() + # use a map-based coordinate system
  labs(title = "Vothing method across states (In-person / By mail), 2016",
       fill = "voting method") +
  blank_theme

