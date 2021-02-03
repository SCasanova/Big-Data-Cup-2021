all_shots <- womens[Event == 'Goal' | Event == 'Shot']
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


set.seed(33)
train_rows <- sample(1:nrow(all_shots), nrow(all_shots)*0.75, replace = F)
train_shots <- all_shots[train_rows]
test_shots <- all_shots[-train_rows]
