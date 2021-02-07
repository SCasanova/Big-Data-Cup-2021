passes <- prospects %>% 
  filter((Event == 'Play' | Event == 'Incomplete Play') )

passes[, Success := ifelse(Event == 'Play', 1, 0)]
passes[, pass_dist := sqrt((X.Coordinate-X.Coordinate.2)^2+(Y.Coordinate-Y.Coordinate.2)^2)]
passes[, pass_dist_stan := stan(pass_dist)]
passes[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
passes[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
passes[, skater_dif := posteam_skaters-defteam_skaters]

set.seed(2021)
train_rows_pass <- createDataPartition(passes$Success, p = 0.8, list = F)
train_pass <- passes[train_rows_pass]
test_pass <- passes[-train_rows_pass]


passes[, One_timer_bin := 0]
passes[, Traffic_bin := ifelse(X.Coordinate >= 170, 0, 1)]
passes[, dist1 := shot_dist_ohl(X.Coordinate, Y.Coordinate)]
passes[, angle1 := shot_angle_ohl(X.Coordinate, Y.Coordinate)]
passes[, dist_stan := stan(dist1)]
passes[, angle_stan := stan(angle1)]
passes[, Goal_bin := 0]



pass_data <- passes %>% 
  select(dist_stan, angle_stan, One_timer_bin, Traffic_bin, skater_dif)  %>% 
  as.matrix()

pass_mat <-xgboost::xgb.DMatrix(data = pass_data,
                                label=passes$Goal_bin)

xG1 <- predict(xG_model_prosp, pass_mat)

passes <- cbind(passes, xG1)

passes[, dist2 := shot_dist_ohl(X.Coordinate.2, Y.Coordinate.2)]
passes[, dist_stan := stan(dist2)]
passes[, angle2 := shot_angle_ohl(X.Coordinate.2, Y.Coordinate.2)]
passes[, angle_stan := stan(angle2)]
passes[, Traffic_bin := ifelse(X.Coordinate.2 >= 170, 0, 1)]


rec_data <- passes %>% 
  select(dist_stan, angle_stan, One_timer_bin, Traffic_bin, skater_dif)  %>% 
  as.matrix()

rec_mat <-xgboost::xgb.DMatrix(data = rec_data,
                                label=passes$Goal_bin)

xG2 <- predict(xG_model_prosp, rec_mat)
passes <- cbind(passes, xG2)

options(scipen = 9999)
passes[, xGA := ifelse(Event == 'Play', xG2-xG1, -xG1)]


xga_score <- passes %>% group_by(Player) %>% 
  summarise(xGA = mean(xGA),
            passes = n()) %>% 
  filter(passes >= 30) %>% 
  arrange(desc(xGA))


