takeaways <- prospects[Event == 'Takeaway']

takeaways[, One_timer_bin := 0]
takeaways[, Traffic_bin := ifelse(X.Coordinate >= 170, 0, 1)]
takeaways[, dist := shot_dist_ohl(X.Coordinate, Y.Coordinate)]
takeaways[, angle := mapply(shot_angle_ohl, X.Coordinate, Y.Coordinate)]
takeaways[, dist_stan := stan(dist)]
takeaways[, angle_stan := stan(angle)]
takeaways[, Goal_bin := 0]
takeaways[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
takeaways[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
takeaways[, skater_diff := posteam_skaters-defteam_skaters]
takeaways[, posteam_score := ifelse(Team == Home.Team, Home.Team.Goals, Away.Team.Goals)]
takeaways[, defteam_score := ifelse(Team == Home.Team, Away.Team.Goals, Home.Team.Goals)]
takeaways[, score_diff := posteam_skaters-defteam_skaters]
takeaways$Clock <- sapply(strsplit(takeaways$Clock,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)
takeaways <- takeaways %>%
  filter(Period < 4) %>%
  mutate(total_time_left = case_when(
    Period == 1 ~ Clock + 40,
    Period == 2 ~ Clock + 20,
    Period == 3 ~ Clock))

pros_take_mat <- takeaways %>% 
  select(Period, Clock, score_diff, skater_diff, total_time_left)  %>% 
  as.matrix()

wp_prosp_take <- predict(wp_model, pros_take_mat)
takeaways <- cbind(takeaways, wp_prosp_take)
takeaways <- takeaways %>% 
  rename(wp_prosp = wp_prosp_take)

take_data <- takeaways %>% 
  select(dist_stan, angle_stan, One_timer_bin, Traffic_bin, wp_prosp)  %>% 
  as.matrix()

take_mat <-xgboost::xgb.DMatrix(data = take_data,
                                label=takeaways$Goal_bin)

xG_off <- predict(xG_model_prosp, take_mat)

takeaways <- cbind(takeaways, xG_off)
takeaways[, recover_xG := xG_off]


taken_goals_score <- takeaways %>% group_by(Player) %>%
  summarise(taken_goals = sum(recover_xG),
            takeaways = n(),
            avg_prob_taken = taken_goals/takeaways) %>% 
  filter(takeaways >= 10) %>% 
  arrange(desc(avg_prob_taken))
