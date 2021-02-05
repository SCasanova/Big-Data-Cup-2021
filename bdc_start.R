library(tidyverse)
library(xgboost)

scouting <- read.csv("~/Desktop/hackathon_scouting.csv")
womens <- read.csv("~/Desktop/hackathon_womens.csv")

fourth_period <- womens %>%
  filter(Period == 4)

womens$Clock <- sapply(strsplit(womens$Clock,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)

womens <- womens %>%
  filter(Period < 4) %>%
  mutate(total_time_left = case_when(
    Period == 1 ~ Clock + 40,
    Period == 2 ~ Clock + 20,
    Period == 3 ~ Clock))

womens <- transform(womens,game_num=as.numeric(factor(womens$game_date)))

winners <- womens %>%
  filter(total_time_left < 0.2) %>%
  group_by(game_date, Home.Team, Away.Team, game_num) %>%
  summarize(home_final = mean(Home.Team.Goals, na.rm = T),
            away_final = mean(Away.Team.Goals, na.rm = T)) %>%
  mutate(winner = case_when(
    home_final > away_final ~ Home.Team,
    home_final == away_final ~ "TIE",
    home_final < away_final ~ Away.Team
  ))

womens <- womens %>%
  left_join(winners)

model_data <- womens %>%
  filter(winner != "TIE") %>%
  mutate(label = ifelse(Team == winner, 1, 0)) %>%
  select()






