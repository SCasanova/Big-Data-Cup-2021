library(ggplot2)
library(data.table)
library(tidyverse)
library(mlr)
library(xgboost)
library(parallel)
library(parallelMap)



womens <- data.table(read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv'))
#Prospect file is not up to date. URL is latest
prospects <- data.table(read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv'))

goals <- womens[Event == 'Goal']
shots <- womens[Event == 'Shot']



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




