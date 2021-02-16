pros_shots <- scouting_win_prob[Event == 'Goal' | Event == 'Shot']
pros_shots[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
pros_shots[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
pros_shots[, skater_diff := posteam_skaters-defteam_skaters]



pros_shots[, Goal_bin := ifelse(Event == 'Goal', 1,0)]
pros_shots[, Traffic_bin := ifelse(Detail.3 == 't', 1,0)]
pros_shots[, One_timer_bin := ifelse(Detail.4 == 't', 1,0)]



pros_shots[, dist := shot_dist_ohl(X.Coordinate, Y.Coordinate)]
pros_shots[, angle := mapply(shot_angle_ohl, X.Coordinate, Y.Coordinate)]
pros_shots[, dist_stan := stan(dist)]
pros_shots[, angle_stan := stan(angle)]
pros_shots_nodif <- pros_shots[skater_diff >= 0]


set.seed(2021)
train_rows_p <- createDataPartition(pros_shots_nodif$Goal_bin, p = 0.8, list = F)
train_shots_p <- pros_shots_nodif[train_rows_p]
test_shots_p <- pros_shots_nodif[-train_rows_p]
