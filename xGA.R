passes <- prospects %>% 
  filter((Event == 'Play' | Event == 'Incomplete Play') & 
           ((X.Coordinate >= 125 & X.Coordinate < X.Coordinate.2) | 
              X.Coordinate >= 150) &
           Detail.1 == 'Direct')

passes[, One_timer_bin := 0]
passes[, Traffic_bin := ifelse(X.Coordinate >= 170, 0, 1)]
passes[, dist1 := shot_dist_ohl(X.Coordinate, Y.Coordinate)]
passes[, angle1 := shot_angle_ohl(X.Coordinate, Y.Coordinate)]
passes[, dist_stan := stan(dist1)]
passes[, angle_stan := stan(angle1)]
passes[, Goal_bin := 0]
passes[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
passes[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
passes[, skater_dif := posteam_skaters-defteam_skaters]


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


