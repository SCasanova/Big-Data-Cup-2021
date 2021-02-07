all_shots <- womens[Event == 'Goal' | Event == 'Shot']
all_shots[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
all_shots[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
all_shots[, skater_dif := posteam_skaters-defteam_skaters]
all_shots[, posteam_score := ifelse(Team == Home.Team, Home.Team.Goals, Away.Team.Goals)]
all_shots[, defteam_score := ifelse(Team == Home.Team, Away.Team.Goals, Home.Team.Goals)]
all_shots[, score_dif := posteam_score-defteam_score]
all_shots <- all_shots %>%
  filter(Period < 4) %>%
  mutate(total_time_left = case_when(
    Period == 1 ~ Clock + 40,
    Period == 2 ~ Clock + 20,
    Period == 3 ~ Clock))

all_shots[, Goal_bin := ifelse(Event == 'Goal', 1,0)]
all_shots[, Traffic_bin := ifelse(Detail.3 == 't', 1,0)]
all_shots[, One_timer_bin := ifelse(Detail.4 == 't', 1,0)]
all_shots[, Snapshot_bin := ifelse(Detail.1 == 'Snapshot', 1,0)]
all_shots[, Fan_bin := ifelse(Detail.1 == 'Fan', 1,0)]
all_shots[, Slapshot_bin := ifelse(Detail.1 == 'Slapshot', 1,0)]
all_shots[, Deflection_bin := ifelse(Detail.1 == 'Deflection', 1,0)]
all_shots[, WrapA_bin := ifelse(Detail.1 == 'Wrap Around', 1,0)]
all_shots[, Wristshot_bin := ifelse(Detail.1 == 'Wristshot', 1,0)]


all_shots[, dist := shot_dist(X.Coordinate, Y.Coordinate)]
all_shots[, angle := shot_angle(X.Coordinate, Y.Coordinate)]
all_shots[, dist_stan := stan(dist)]
all_shots[, angle_stan := stan(angle)]

shots_mat <- all_shots %>% 
  select(Period, Clock, score_diff,skater_diff, total_time_left)  %>% 
  as.matrix()

wp <- predict(wp_model, shots_mat)
all_shots <- cbind(all_shots, wp)


no_neg_dif <- all_shots[skater_dif >= 0]


set.seed(35)
train_rows <- createDataPartition(no_neg_dif$Goal_bin, p = 0.8, list = F)
train_shots <- no_neg_dif[train_rows]
test_shots <- no_neg_dif[-train_rows]
