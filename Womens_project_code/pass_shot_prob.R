womens1 <- data.table(read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv'))
womens2 <- data.table(read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_nwhl.csv'))

olympic <- womens1[str_detect(womens1$Home.Team, 'Olympic')]
ncaa <- womens1[str_detect(womens1$Home.Team, 'Lawrence') | str_detect(womens1$Home.Team, 'Golden')]

american_rules <- rbind(ncaa, womens2)

aactions <- american_rules[Event == 'Goal' | Event == 'Shot' | Event == 'Play' | Event == 'Incomplete Play']
aactions[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
aactions[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
aactions[, skater_diff := posteam_skaters-defteam_skaters]
aactions[, posteam_score := ifelse(Team == Home.Team, Home.Team.Goals, Away.Team.Goals)]
aactions[, defteam_score := ifelse(Team == Home.Team, Away.Team.Goals, Home.Team.Goals)]
aactions[, score_diff := posteam_score-defteam_score]

aactions$Clock <- sapply(strsplit(aactions$Clock,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)

aactions <- aactions %>%
  filter(Period < 4) %>%
  mutate(total_time_left = case_when(
    Period == 1 ~ Clock + 40,
    Period == 2 ~ Clock + 20,
    Period == 3 ~ Clock))

aactions[, Goal := ifelse(Event == 'Goal', 1,0)]
aactions[, dist := shot_dist_ohl(X.Coordinate, Y.Coordinate)]
aactions[, angle := mapply(shot_angle_ohl,X.Coordinate,Y.Coordinate)]
aactions[, dist_stan := stan(dist)]
aactions[, angle_stan := stan(angle)]
aactions[, dist2 := shot_dist_ohl(X.Coordinate.2, Y.Coordinate.2)]
aactions[, angle2 := mapply(shot_angle_ohl,X.Coordinate.2,Y.Coordinate.2)]
aactions[, dist_stan2 := stan(dist2)]
aactions[, angle_stan2 := stan(angle2)]

iactions <- olympic[Event == 'Goal' | Event == 'Shot']
iactions[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
iactions[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
iactions[, skater_diff := posteam_skaters-defteam_skaters]
iactions[, posteam_score := ifelse(Team == Home.Team, Home.Team.Goals, Away.Team.Goals)]
iactions[, defteam_score := ifelse(Team == Home.Team, Away.Team.Goals, Home.Team.Goals)]
iactions[, score_diff := posteam_score-defteam_score]

iactions$Clock <- sapply(strsplit(iactions$Clock,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)


iactions <- iactions %>%
  filter(Period < 4) %>%
  mutate(total_time_left = case_when(
    Period == 1 ~ Clock + 40,
    Period == 2 ~ Clock + 20,
    Period == 3 ~ Clock))

iactions[, Goal := ifelse(Event == 'Goal', 1,0)]
iactions[, dist := shot_dist(X.Coordinate, Y.Coordinate)]
iactions[, angle := mapply(shot_angle,X.Coordinate,Y.Coordinate)]
iactions[, dist_stan := stan(dist)]
iactions[, angle_stan := stan(angle)]
iactions[, dist2 := shot_dist_ohl(X.Coordinate.2, Y.Coordinate.2)]
iactions[, angle2 := mapply(shot_angle_ohl,X.Coordinate.2,Y.Coordinate.2)]
iactions[, dist_stan2 := stan(dist2)]
iactions[, angle_stan2 := stan(angle2)]

all_actions <- rbind(aactions, iactions)

all_actions <- all_actions %>%
  mutate(index = 1:n())

wp_games <- stats::predict(wp_model,
                             as.matrix(all_actions %>%
                                         dplyr::select(Period, Clock, score_diff, 
                                                skater_diff, total_time_left))) %>%
    tibble::as_tibble() %>%
    dplyr::rename(prob = "value") %>%
    dplyr::bind_cols(purrr::map_dfr(seq_along(all_actions$index), function(x) {
      tibble::tibble("wp" = 0:1,
                     "Period" = all_actions$Period[[x]],
                     "Clock" = all_actions$Clock[[x]],
                     "score_diff" = all_actions$score_diff[[x]],
                     "skater_diff" = all_actions$skater_diff[[x]],
                     "total_time_left" = all_actions$total_time_left[[x]],
                     "index" = all_actions$index[[x]])
    })) %>%
    dplyr::group_by(.data$index) %>%
    dplyr::mutate(cum_prob = cumsum(.data$prob),
                  prob = .data$prob) %>%
    dplyr::select(-.data$cum_prob) %>%
    dplyr::summarise(win_prob = sum(.data$prob * .data$wp)) %>%
    ungroup()   
  
win_prob_actions <- all_actions %>%
  inner_join(wp_games) %>%
  dplyr::select(-index) %>%
  mutate(play_num = 1:n())

xg_pred_mat <- win_prob_actions %>% 
  dplyr::select(dist_stan, angle_stan,win_prob)  %>% 
  as.matrix()

xg <- predict(xG_model, xg_pred_mat)
all_actions <- cbind(all_actions, xg)

xg_pred_mat2 <- win_prob_actions %>% 
  dplyr::select(dist_stan2, angle_stan2,win_prob)  %>% 
  rename(dist_stan = dist_stan2,
         angle_stan = angle_stan2) %>% 
  as.matrix()

xg2 <- predict(xG_model, xg_pred_mat2)
all_actions <- cbind(all_actions, xg2)

all_actions[, pass := ifelse((Event == 'Play' | Event == 'Incomplete Play'), 1, 0)]
all_actions[, x_stan := stan(X.Coordinate)]


mean(all_actions$pass)

###

set.seed(2021)
train_rows_action <- createDataPartition(all_actions$pass, p = .8, list = F)
train_waction <- all_actions[train_rows_action]
test_waction <- all_actions[-train_rows_action]

glm_passprob <- glm(pass~x_stan+xg+score_diff+skater_diff+Clock, family = binomial(link='logit'), data = train_waction)
summary(glm_passprob)

shot_rw_tr <- train_waction %>% 
  dplyr::select(x_stan, xg, score_diff, skater_diff, Clock)  %>% 
  as.matrix()

shot_rw_ts <- test_waction %>% 
  dplyr::select(x_stan, xg, score_diff, skater_diff, Clock)  %>% 
  as.matrix()

labels_rw <- train_waction$pass
ts_label_rw <- test_waction$pass

rwtrain <- xgboost::xgb.DMatrix(data = shot_rw_tr,label = labels_rw) 
rwtest <-xgboost::xgb.DMatrix(data = shot_rw_ts,label=ts_label_rw)

#MLR learner

lrn_tr_rw <- train_waction %>% 
  dplyr::select(x_stan, xg, score_diff, skater_diff, Clock, pass)  %>% 
  data.frame()
    
lrn_ts_rw <- test_waction %>% 
  dplyr::select(x_stan, xg, score_diff, skater_diff, Clock, pass)  %>% 
  data.frame()

lrn_tr_rw$pass <- as.factor(lrn_tr_rw$pass)
lrn_ts_rw$pass <- as.factor(lrn_ts_rw$pass)

traintask_rw <- makeClassifTask (data = lrn_tr_rw,target = "pass")
testtask_rw <- makeClassifTask (data = lrn_ts_rw,target = "pass")
traintask_rw <- createDummyFeatures (obj = traintask_rw)
testtask_rw <- createDummyFeatures (obj = testtask_rw)


lrn_rw <- makeLearner("classif.xgboost",predict.type = "prob")
lrn_rw$par.vals <- list( objective="binary:logistic", eval_metric="auc", 
                         nrounds=800, 
                         eta=0.01,
                         base_score =mean(train_waction$pass), 
                         booster = 'gbtree')
params_lrn_rw <- makeParamSet(makeIntegerParam("max_depth",lower = 3L,upper = 25L), 
                        makeIntegerParam("gamma",lower = 0L,upper = 6L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.4,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.4,upper = 1))


rdesc_rw <- makeResampleDesc("CV",stratify = T,iters=7L)
ctrl_rw <- makeTuneControlRandom(maxit = 20L)


parallelStartSocket(cpus = detectCores())
set.seed(33)
prosptune <- tuneParams(learner = lrn_rw, 
                     task = traintask_rw, 
                     resampling = rdesc_rw, 
                     measures = auc, 
                     par.set = params_lrn_rw, 
                     control = ctrl_rw, show.info = T)


params_rw<- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eval_metric = c('auc'),
               eta=0.01, 
               gamma=5, 
               min_child_weight=3.9, 
               max_depth = 16,
               subsample=0.944, 
               colsample_bytree=0.833,
               base_score =mean(train_waction$pass))

set.seed(33)
xgbcv_rw<- xgboost::xgb.cv( params = params_rw, 
                          data = rwtrain, 
                          nrounds = 4000, 
                          nfold = 7, 
                          showsd = T, 
                          stratified = T, 
                          print_every_n = 20, 
                          early_stopping_rounds = 20, 
                          maximize = F)

set.seed(33)
rw_model <- xgboost::xgb.train(params = params_rw, 
                               data = rwtrain, 
                               nrounds = 340, 
                               watchlist = list(val=rwtest,train=rwtrain), 
                               print_every_n = 20, 
                               early_stop_round = 10, 
                               maximize = F)

par(mfrow=c(1,1))
impor_rw <- xgboost::xgb.importance(colnames(rwtrain), model = rw_model)
xgboost::xgb.plot.importance(impor_rw)


pass_prob <- predict(rw_model, rwtest)

pass_prob_data <- cbind(test_waction, pass_prob) 
mean(pass_prob_data$pass)
mean(pass_prob_data$pass_prob)

pass_prob_data$dumb <- mean(pass_prob_data$pass)

AUC(pass_prob_data$pass_prob,pass_prob_data$pass )
AUC(pass_prob_data$dumb,pass_prob_data$pass )


LogLoss(pass_prob_data$pass_prob,pass_prob_data$pass )
LogLoss(pass_prob_data$dumb,pass_prob_data$pass )



###
rw_pred_mat <- all_actions %>% 
  dplyr::select(x_stan, xg, score_diff, skater_diff, Clock)  %>% 
  as.matrix()

pass_prob <- predict(rw_model, rw_pred_mat)
all_actions <- cbind(all_actions, pass_prob) 

all_actions[, pass_dist := ifelse((Event == 'Play' | Event == 'Incomplete Play'), 
                                  sqrt((X.Coordinate-X.Coordinate.2)^2+(Y.Coordinate-Y.Coordinate.2)^2), 
                                  NA)]
all_actions[, pass_dist_stan := stan(pass_dist)]
all_actions[, x_stan2 := stan(X.Coordinate.2)]
all_actions[, direct := ifelse((Event == 'Play' | Event == 'Incomplete Play'), 
                                  ifelse(Detail.1 == 'Direct', 1, 0), NA)]

cp_pred_mat <- all_actions %>% 
  dplyr::select(pass_dist_stan,skater_diff,x_stan,x_stan2, direct)  %>% 
  as.matrix()


cp <- predict(wcp_model, cp_pred_mat)
all_actions <- cbind(all_actions, cp)

all_actions %>% 
  mutate(round_xg = round(xg,3)) %>% 
  group_by(round_xg) %>% 
  summarise(mean(Goal),
            n()) %>% 
  View()


action_grade <- all_actions %>% 
  filter(Event == 'Play' | Event == 'Incomplete Play') %>% 
  dplyr::select(Player, Team, Event, X.Coordinate, Y.Coordinate,xg,pass_prob,cp,
                   Player.2, X.Coordinate.2, Y.Coordinate.2, xg2) %>% 
  mutate(expected_action = ifelse(pass_prob >= 0.5, 'Pass', 'Shot'),
         actual_action = ifelse(Event == 'Shot' | Event == 'Goal', 'Shot', 'Pass'),
         pass_success = ifelse(Event == 'Play', 1, 0),
         pass_risk = xg-(cp*xg2),
         pass_reward = xg2-xg,
         xga = ifelse(Event == 'Play', xg2-xg, -xg),
         suggested_action = ifelse(((pass_reward <0 & xg >= 0.05) | pass_risk >0.07) & xg > 0.077, 'Shot', 'Pass'))




action_grade  %>% filter(expected_action != actual_action)
action_grade  %>% filter(suggested_action == actual_action & suggested_action != expected_action)


action_grade %>% 
  dplyr::select(Player, pass_prob, expected_action, actual_action, suggested_action, xg, cp, pass_success, xg2, pass_reward, pass_risk) %>% 
  mutate(shot_prob = 1-pass_prob) %>% 
  rename(`Pass Probabilty` = pass_prob,
         `xG from Shot` = xg,
         `Complete Pass` = pass_success,
         `xG after Pass` = xg2)

options(scipen = 9999)
action_grade %>% 
  group_by(Player) %>% 
  summarise(correct_decs = sum(ifelse(suggested_action == actual_action, 1, 0)),
            expected_decs = sum(ifelse(expected_action == actual_action, 1, 0)),
            plays = n(),
            correct_rate = correct_decs/plays,
            expected_rate = expected_decs/plays) %>% 
  filter(plays >= 100) %>% 
  arrange(desc(correct_rate))


jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

options(scipen = 9999)

expected_shot <- all_actions %>% 
  filter(pass_prob <= 0.5)


xmap <- ggplot(expected_shot, aes(X.Coordinate, Y.Coordinate)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(15), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")+
  xlim(100,200)+
  ylim(0,85)
ggsave("xheatmap.png", xmap, device = 'png', dpi = 540)

shot <- all_actions %>% 
  filter(Event == 'Shot' | Event == 'Goal')

ac_map <- ggplot(shot, aes(X.Coordinate, Y.Coordinate)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(15), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")+
  xlim(100,200)+
  ylim(0,85)
ggsave("acheatmap.png", ac_map, device = 'png', dpi = 540)

suggested_shot <- action_grade %>% 
  filter(suggested_action == 'Shot')

sug_map <- ggplot(suggested_shot, aes(X.Coordinate, Y.Coordinate)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(15), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")+
  xlim(100,200)+
  ylim(0,85)
ggsave("sugheatmap.png", sug_map, device = 'png', dpi = 540)

