pros_shots_preds <- pros_shots %>% 
  mutate(dist_stan = stan(dist),
         angle_stan = stan(angle),
         x_stan = stan(X.Coordinate)) %>% 
  dplyr::select(dist_stan, angle_stan, One_timer,x_stan, Snapshot,Wristshot, win_prob) %>% 
  as.matrix()

shot_preds <- pros_shots

xG_prosp <- predict(xG_model_prosp, pros_shots_preds)

shot_preds <- cbind(shot_preds, xG_prosp)

prob_prosp <- shot_preds %>% 
  group_by(X.Coordinate, Y.Coordinate) %>% 
  summarise(value = mean(xG_prosp),
            goals = sum(Goal)) %>% 
  ungroup() %>% 
  arrange(value)

preds_vs_goals <- shot_preds %>% 
  group_by(Player) %>% 
  summarise(xgoals = sum(xG_prosp),
            goals = sum(Goal),
            shots = n()) %>% 
  #filter(shots>=10) %>% 
  arrange(desc(goals))

xg_cor <- round(cor(preds_vs_goals$goals, preds_vs_goals$xgoals),3)

ggplot(preds_vs_goals, aes(goals,xgoals)) +
  theme_minimal()+
  geom_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  geom_point(color ='#090A62', alpha=0.8, size = preds_vs_goals$shots/50)+
  labs(x = 'Goals',
       y = 'Expected Goal',
       title = 'Relationship Between xG and Goals',
       caption = paste('Correlation:', xg_cor))+
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5))


cg_score <- shot_preds %>% mutate(points = ifelse(Goal == 1, 1000, xG_prosp*1000)) %>% 
  group_by(Player) %>% 
  summarise(chance_goal_score = sum(points),
            goals = sum(Goal),
            shots = n(),
            avg_cg_score = chance_goal_score/shots,
            team = last(Team)) %>% 
  filter(shots >= 15) %>% 
  arrange(desc(avg_cg_score))
  
c_score <- shot_preds %>% mutate(points = xG_prosp*1000) %>% 
  group_by(Player) %>% 
  summarise(chance_score = sum(points),
            goals = sum(Goal),
            shots = n(),
            avg_c_score = chance_score/shots) %>% 
  filter(shots >= 15) %>% 
  arrange(desc(avg_c_score)) %>% 
  dplyr::select(-goals, -shots)

total_xg <- shot_preds %>% 
  mutate(goals_oX = Goal-xG_prosp) %>% 
  group_by(Player) %>% 
  summarise(goals = sum(Goal),
            shots = n(),
            goals_oX = mean(goals_oX)) %>% 
  filter(shots >= 15) %>% 
  arrange(desc(goals_oX)) %>% 
  dplyr::select(-shots)

games <- scouting %>% group_by(Player, game_date) %>% 
  summarise(plays = n()) %>% 
  group_by(Player) %>% 
  summarise(games = n())

pros_spass_preds <- passes %>% 
  dplyr::select(pass_dist_stan,skater_dif,`X Coordinate`,`X Coordinate 2`, direct)  %>% 
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
  filter(passes >= 120) %>% 
  arrange(desc(cpoe))

c_plus_cg <- left_join(cg_score, c_score, by = c('Player')) 
scores_plus_xga <- merge(cpoe, c_plus_cg, by = c('Player'))
scores_plus_def <- merge.data.frame(scores_plus_xga, taken_goals_score, by = c('Player'))
scores_plus_def <- merge(scores_plus_def, total_xg, by = c('Player'))
scores_plus_def <- left_join(scores_plus_def, games, by = c('Player'))

prospects <- scores_plus_def %>% mutate(actions = passes+shots+takeaways) 

write_csv(prospects, 'prospects_full.csv')

index_prosp <- scores_plus_def %>% 
  dplyr::select(Player, avg_c_score, avg_cg_score, avg_pass_prob, cpoe, avg_prob_taken, goals_oX, takeaways, shots, passes, team, games) %>% 
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
  geom_point(size = 3, color = 'black')+
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

##By Game

cg_score <- shot_preds %>% mutate(points = ifelse(Goal_bin == 1, 1000, xG_prosp*1000)) %>% 
  group_by(Player, game_date) %>% 
  summarise(chance_goal_score = sum(points),
            goals = sum(Goal_bin),
            shots = n(),
            avg_cg_score = chance_goal_score/shots) %>% 
  filter(shots >= 1) %>% 
  arrange(desc(avg_cg_score))
  
c_score <- shot_preds %>% mutate(points = xG_prosp*1000) %>% 
  group_by(Player,game_date) %>% 
  summarise(chance_score = sum(points),
            goals = sum(Goal_bin),
            shots = n(),
            avg_c_score = chance_score/shots) %>% 
  filter(shots >= 1) %>% 
  arrange(desc(avg_c_score)) %>% 
  select(-goals, -shots)

total_xg <- shot_preds %>% 
  mutate(goals_oX = Goal_bin-xG_prosp) %>% 
  group_by(Player,game_date) %>% 
  summarise(goals = sum(Goal_bin),
            shots = n(),
            goals_oX = mean(goals_oX)) %>% 
  filter(shots >= 1) %>% 
  arrange(desc(goals_oX)) %>% 
  select(-shots)

cpoe <- cp_prosp %>% 
  mutate(cpoe = Success-cp) %>% 
  group_by(Player,game_date) %>% 
  summarise(avg_c = mean(Success),
            avg_pass_prob = mean(cp),
            cpoe = mean(cpoe),
            passes = n(),
            team = last(Team)) %>% 
  filter(passes >= 2) %>% 
  arrange(desc(cpoe))

c_plus_cg <- left_join(cg_score, c_score, by = c('Player', 'game_date'))
scores_plus_xga <- left_join(cpoe, c_plus_cg, by = c('Player', 'game_date'))
scores_plus_def <- left_join(scores_plus_xga, taken_goals_score, by = c('Player', 'game_date'))
scores_plus_def <- left_join(scores_plus_def, total_xg, by = c('Player', 'game_date'))
scores_plus_def <- left_join(scores_plus_def, games, by = c('Player', 'game_date'))

bayesian_metrics <- scores_plus_def %>% 
  mutate(actions = passes+shots+takeaways) %>% 
  select(-passes,-goals.x,-shots,-goals.y)

write.csv(bayesian_metrics, 'prospect_metrics.csv')













