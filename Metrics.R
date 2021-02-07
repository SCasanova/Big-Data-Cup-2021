pros_shots_preds <- pros_shots_nodif %>% 
  select(dist_stan, angle_stan, One_timer_bin, Traffic_bin, skater_diff, wp_prosp) %>% 
  as.matrix()

shot_preds <- pros_shots_nodif

xG_prosp <- predict(xG_model_prosp, pros_shots_preds)

shot_preds <- cbind(shot_preds, xG_prosp)

prob_prosp <- shot_preds %>% 
  group_by(X.Coordinate, Y.Coordinate) %>% 
  summarise(value = mean(xG_prosp),
            goals = sum(Goal_bin)) %>% 
  ungroup() %>% 
  arrange(value)



cg_score <- shot_preds %>% mutate(points = ifelse(Goal_bin == 1, 1000, xG_prosp*1000)) %>% 
  group_by(Player) %>% 
  summarise(chance_goal_score = sum(points),
            goals = sum(Goal_bin),
            shots = n(),
            avg_cg_score = chance_goal_score/shots) %>% 
  filter(shots >= 40) %>% 
  arrange(desc(avg_cg_score))
  
c_score <- shot_preds %>% mutate(points = xG_prosp*1000) %>% 
  group_by(Player) %>% 
  summarise(chance_score = sum(points),
            goals = sum(Goal_bin),
            shots = n(),
            avg_c_score = chance_score/shots) %>% 
  filter(shots >= 40) %>% 
  arrange(desc(avg_c_score)) %>% 
  select(-goals, -shots)

total_xg <- shot_preds %>% 
  group_by(Player) %>% 
  summarise(xg = sum(xG_prosp),
            goals = sum(Goal_bin),
            shots = n(),
            goals_oX = goals-xg) %>% 
  filter(shots >= 40) %>% 
  arrange(desc(goals_oX)) %>% 
  select(-shots)

pros_spass_preds <- passes %>% 
  select(pass_dist_stan,skater_dif,X.Coordinate,X.Coordinate.2,Y.Coordinate,Y.Coordinate.2)  %>% 
  as.matrix()

pass_preds <- passes

cp <- predict(cp_model, pros_spass_preds)

cp_prosp <- cbind(pass_preds, cp)


cpoe <- cp_prosp %>% 
  mutate(cpoe = Success-cp) %>% 
  group_by(Player) %>% 
  summarise(avg_c = mean(Success),
            avg_pass_prob = mean(cp),
            cpoe = mean(cpoe),
            passes = n()) %>% 
  filter(passes >= 125) %>% 
  arrange(desc(cpoe))
