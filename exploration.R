library(ggplot2)
library(data.table)
library(tidyverse)



womens <- data.table(read.csv('hackathon_womens.csv'))
prospects <- data.table(read.csv('hackathon_scouting.csv'))

goals <- womens[Event == 'Goal']
shots <- womens[Event == 'Shot']

ggplot(goals, aes(X.Coordinate, Y.Coordinate)) + 
  geom_point(data = shots, color = 'blue', aes(alpha = 0.1))+
  geom_point(data = shots[Detail.2 == 'On Net'], color = 'blue', aes(alpha = 0.1))+
  geom_point(color = 'red')+
  xlim(0,200)+
  theme_minimal()

passes <- prospects[Event == 'Play']
passes[, int_pass_dist := sqrt((X.Coordinate-X.Coordinate.2)**2+(Y.Coordinate-Y.Coordinate.2)**2)]
passes$Detail.1 <- as.factor(passes$Detail.1)

total_wss_vector <- c()
for (k in 1:15){
  set.seed(15)
  exploratory_prospects <- kmeans(passes[,c('Detail.1', 'int_pass_dist')],centers=k,iter.max = 20) 
  total_wss_vector <- c(total_wss_vector, exploratory_prospects$tot.withinss)
}

r
plot(seq(1,15,1),total_wss_vector,xlab='Number of Clusters',ylab='Total Within Sum of Squares')

set.seed(1)
exploratory_prospects <- kmeans(passes[,c('Detail.1', 'int_pass_dist')],centers=5,iter.max = 20) 
plot(passes[,c()],col=exploratory_prospects$cluster)



