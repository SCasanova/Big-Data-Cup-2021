library(ggplot2)
library(data.table)
library(tidyverse)
library(mlr)
library(xgboost)
library(parallel)
library(parallelMap)
library(png)
library(ggimage) 
library(caret)



womens <- data.table(read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv'))
#Prospect file is not up to date. URL is latest
prospects <- data.table(read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv'))



goals <- womens[Event == 'Goal']
shots <- womens[Event == 'Shot']

min(shots$X.Coordinate)

goals[, angle := shot_angle(X.Coordinate, Y.Coordinate)]

ggplot(goals, aes(X.Coordinate, Y.Coordinate)) + 
  geom_point(data = shots, color = 'blue', aes(alpha = 0.1))+
  geom_point(data = shots[Detail.2 == 'On Net'], color = 'blue', aes(alpha = 0.1))+
  geom_point(color = 'red')+
  xlim(0,200)+
  theme_minimal()

passes <- prospects[Event == 'Play']
passes[, int_pass_dist := sqrt((X.Coordinate-X.Coordinate.2)**2+(Y.Coordinate-Y.Coordinate.2)**2)]
passes[, direct:= ifelse(Detail.1 == 'Direct', 1 ,0)]

total_wss_vector <- c()
for (k in 1:15){
  set.seed(15)
  exploratory_prospects <- kmeans(passes[,c('int_pass_dist', 'direct')],centers=k,iter.max = 20) 
  total_wss_vector <- c(total_wss_vector, exploratory_prospects$tot.withinss)
}


plot(seq(1,15,1),total_wss_vector,xlab='Number of Clusters',ylab='Total Within Sum of Squares')

set.seed(1)
exploratory_prospects <- kmeans(passes[,c('direct', 'int_pass_dist')],centers=4,iter.max = 20) 
ggplot(passes, aes(direct, int_pass_dist)) +
  geom_point(col=exploratory_prospects$cluster)+
  theme_minimal()


womens[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
womens[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
womens[, skater_diff := posteam_skaters-defteam_skaters]
womens[, posteam_score := ifelse(Team == Home.Team, Home.Team.Goals, Away.Team.Goals)]
womens[, defteam_score := ifelse(Team == Home.Team, Away.Team.Goals, Home.Team.Goals)]
womens[, score_diff := posteam_score-defteam_score]

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


womens_wp_mat <- womens %>% 
  select(Period, Clock, score_diff,skater_diff, total_time_left)  %>% 
  as.matrix()

wp <- predict(wp_model, womens_wp_mat)
womens <- cbind(womens, wp)

goals_wp <- womens %>% 
  mutate(goal = ifelse(Event == 'Goal', 1, 0),
         wp = round(wp,2)) %>% 
  group_by(wp) %>% 
  summarise(goals = sum(goal))

ggplot(goals_wp, aes(wp, goals))+
  geom_point(aes(alpha = 0.3, size = goals, color = wp))+
  geom_smooth(se = F) +
  theme_minimal()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))



prospects[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
prospects[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
prospects[, skater_diff := posteam_skaters-defteam_skaters]
prospects[, posteam_score := ifelse(Team == Home.Team, Home.Team.Goals, Away.Team.Goals)]
prospects[, defteam_score := ifelse(Team == Home.Team, Away.Team.Goals, Home.Team.Goals)]
prospects[, score_diff := posteam_score-defteam_score]

prospects$Clock <- sapply(strsplit(prospects$Clock,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)
prospects <- prospects %>%
  filter(Period < 4) %>%
  mutate(total_time_left = case_when(
    Period == 1 ~ Clock + 40,
    Period == 2 ~ Clock + 20,
    Period == 3 ~ Clock))


prospects_wp_mat <- prospects %>% 
  select(Period, Clock, score_diff,skater_diff, total_time_left)  %>% 
  as.matrix()

wp <- predict(scouting_wp, prospects_wp_mat)
prospects <- cbind(prospects, wp)

goals_wp_p <- prospects %>% 
  mutate(goal = ifelse(Event == 'Goal', 1, 0),
         wp = round(wp,3)) %>% 
  group_by(wp) %>% 
  summarise(goals = sum(goal))

ggplot(goals_wp_p, aes(wp, goals))+
  geom_point(aes(alpha = 0.3, size = goals, color = wp))+
  geom_smooth(se = F) +
  theme_minimal()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))



ben_viz <- prospects %>% 
  filter(Event == 'Goal') %>% 
  select(game_num, Period, Event, wp)



