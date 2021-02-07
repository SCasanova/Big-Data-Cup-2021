pros_shots <- prospects[Event == 'Goal' | Event == 'Shot']
pros_shots[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
pros_shots[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
pros_shots[, skater_diff := posteam_skaters-defteam_skaters]
pros_shots[, posteam_score := ifelse(Team == Home.Team, Home.Team.Goals, Away.Team.Goals)]
pros_shots[, defteam_score := ifelse(Team == Home.Team, Away.Team.Goals, Home.Team.Goals)]
pros_shots[, score_diff := posteam_score-defteam_score]


pros_shots$Clock <- sapply(strsplit(pros_shots$Clock,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)
pros_shots <- pros_shots %>%
  filter(Period < 4) %>%
  mutate(total_time_left = case_when(
    Period == 1 ~ Clock + 40,
    Period == 2 ~ Clock + 20,
    Period == 3 ~ Clock))

pros_shots[, Goal_bin := ifelse(Event == 'Goal', 1,0)]
pros_shots[, Traffic_bin := ifelse(Detail.3 == 't', 1,0)]
pros_shots[, One_timer_bin := ifelse(Detail.4 == 't', 1,0)]



pros_shots[, dist := shot_dist_ohl(X.Coordinate, Y.Coordinate)]
pros_shots[, angle := shot_angle_ohl(X.Coordinate, Y.Coordinate)]
pros_shots[, dist_stan := stan(dist)]
pros_shots[, angle_stan := stan(angle)]
pros_shots_nodif <- pros_shots[skater_diff >= 0]

pros_shots_mat <- pros_shots_nodif %>% 
  select(Period, Clock, score_diff, skater_diff, total_time_left)  %>% 
  as.matrix()

wp_prosp <- predict(wp_model, pros_shots_mat)
pros_shots_nodif <- cbind(pros_shots_nodif, wp_prosp)

set.seed(2021)
train_rows_p <- createDataPartition(pros_shots_nodif$Goal_bin, p = 0.8, list = F)
train_shots_p <- pros_shots_nodif[train_rows_p]
test_shots_p <- pros_shots_nodif[-train_rows_p]
