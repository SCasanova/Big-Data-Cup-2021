takeaways <- prospects[Event == 'Takeaway']

takeaways[, One_timer_bin := 0]
takeaways[, Traffic_bin := ifelse(X.Coordinate >= 170, 0, 1)]
takeaways[, dist := shot_dist_ohl(X.Coordinate, Y.Coordinate)]
takeaways[, angle := shot_angle_ohl(X.Coordinate, Y.Coordinate)]
takeaways[, dist_stan := stan(dist)]
takeaways[, angle_stan := stan(angle)]
takeaways[, Goal_bin := 0]
takeaways[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
takeaways[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
takeaways[, skater_dif := posteam_skaters-defteam_skaters]

take_data <- takeaways %>% 
  select(dist_stan, angle_stan, One_timer_bin, Traffic_bin, skater_dif)  %>% 
  as.matrix()

take_mat <-xgboost::xgb.DMatrix(data = take_data,
                                label=takeaways$Goal_bin)

xG_off <- predict(xG_model_prosp, take_mat)

takeaways <- cbind(takeaways, xG_off)
takeaways[, recover_xG := xG_off]


taken_goals_score <- takeaways %>% group_by(Player) %>%
  summarise(taken_goals = sum(recover_xG),
            takeaways = n()) %>% 
  filter(takeaways >= 12) %>% 
  arrange(desc(taken_goals))
