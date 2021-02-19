pros_shots_preds <- pros_shots_nodif %>% 
  select(dist_stan, angle_stan, One_timer_bin, win_prob) %>% 
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
            avg_cg_score = chance_goal_score/shots,
            team = last(Team)) %>% 
  filter(shots >= 10) %>% 
  arrange(desc(avg_cg_score))
  
c_score <- shot_preds %>% mutate(points = xG_prosp*1000) %>% 
  group_by(Player) %>% 
  summarise(chance_score = sum(points),
            goals = sum(Goal_bin),
            shots = n(),
            avg_c_score = chance_score/shots) %>% 
  filter(shots >= 10) %>% 
  arrange(desc(avg_c_score)) %>% 
  select(-goals, -shots)

total_xg <- shot_preds %>% 
  mutate(goals_oX = Goal_bin-xG_prosp) %>% 
  group_by(Player) %>% 
  summarise(goals = sum(Goal_bin),
            shots = n(),
            goals_oX = mean(goals_oX)) %>% 
  filter(shots >= 10) %>% 
  arrange(desc(goals_oX)) %>% 
  select(-shots)

games <- scouting %>% group_by(Player, game_date) %>% 
  summarise(plays = n()) %>% 
  group_by(Player) %>% 
  summarise(games = n())

pros_spass_preds <- passes %>% 
  select(pass_dist_stan,skater_dif,X.Coordinate,X.Coordinate.2,Y.Coordinate,Y.Coordinate.2, direct)  %>% 
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
  filter(passes >= 40) %>% 
  arrange(desc(cpoe))

c_plus_cg <- left_join(cg_score, c_score, by = c('Player'))
scores_plus_xga <- merge(cpoe, c_plus_cg, by = c('Player'))
scores_plus_def <- merge(scores_plus_xga, taken_goals_score, by = c('Player'))
scores_plus_def <- merge(scores_plus_def, total_xg, by = c('Player'))
scores_plus_def <- left_join(scores_plus_def, games, by = c('Player'))



write.csv(scores_plus_def, 'pospect_metrics.csv')

index_prosp <- scores_plus_def %>% 
  select(Player, avg_c_score, avg_cg_score, avg_pass_prob, cpoe, avg_prob_taken, goals_oX, takeaways, shots, passes, team, games) %>% 
  mutate(actions = takeaways+passes+shots,
         c_stan = stan(avg_c_score), 
         cg_stan = stan(avg_cg_score),
         pass_prob_stan = stan(avg_pass_prob),
         cpoe_stan = stan(cpoe),
         taken_stan = stan(avg_prob_taken),
         takeaways_stan = stan(takeaways),
         goals_ox_stan = stan(goals_oX),
         takes_game = takeaways/games,
         takes_game_stan = stan(takes_game)) %>% 
  filter(team == 'Erie Otters') %>% 
  mutate(grade = (c_stan+cg_stan+goals_ox_stan)*(1/3)+(pass_prob_stan+cpoe_stan)*(1/3)+(taken_stan+takes_game_stan)*(1/3)) %>% 
  arrange(desc(grade))


index_grade <- ggplot(index_prosp, aes(grade, reorder(Player, grade)))+
  geom_point(size = 3)+
  geom_point(aes(x = c_stan ), shape = 15,color = 'red', alpha = 0.4, size = 2)+
  geom_point(aes(x = cg_stan), shape = 16,color = 'blue', alpha = 0.4, size = 2)+
  geom_point(aes(x = pass_prob_stan), shape = 17,color = '#499A24', alpha = 0.4, size = 2)+
  geom_point(aes(x = cpoe_stan), shape = 3,color = '#4E64EA', alpha = 0.4, size = 2)+
  geom_point(aes(x = taken_stan), shape = 18,color = 'grey', alpha = 0.4, size = 2)+
  geom_point(aes(x = goals_ox_stan), shape = 19,color = 'firebrick', alpha = 0.4, size = 2)+
  geom_point(aes(x = takes_game_stan), shape = 20,color = '#7B41DD', alpha = 0.4, size = 2)+
  labs(title = 'Prospct Grade Based on Metric index',
       subtitle = 'Minimum 300 actions (passes, shots, takeaways)',
       caption = 'Metrics used: C Score + CG Score+ Goals over expected (1/3) \n
       Avg Pass Prob + CPOE (1/3) \n
       Taken Goals + Takeaways per Game (1/3)',
       y = 'Player',
       x = 'Grade Index')+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 9, hjust = 0.5)
  )+
  scale_x_continuous(breaks = c(0))
ggsave('Grades.png', index_grade, width = 8, height = 8, dpi = 540)











