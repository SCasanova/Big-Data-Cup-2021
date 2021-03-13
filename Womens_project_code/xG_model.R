womens1 <- data.table(read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv'))
womens2 <- data.table(read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_nwhl.csv'))

olympic <- womens1[str_detect(womens1$Home.Team, 'Olympic')]
ncaa <- womens1[str_detect(womens1$Home.Team, 'Lawrence') | str_detect(womens1$Home.Team, 'Golden')]

american_rules <- rbind(ncaa, womens2)

ashots <- american_rules[Event == 'Goal' | Event == 'Shot']
ashots[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
ashots[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
ashots[, skater_diff := posteam_skaters-defteam_skaters]
ashots[, posteam_score := ifelse(Team == Home.Team, Home.Team.Goals, Away.Team.Goals)]
ashots[, defteam_score := ifelse(Team == Home.Team, Away.Team.Goals, Home.Team.Goals)]
ashots[, score_diff := posteam_score-defteam_score]

ashots$Clock <- sapply(strsplit(ashots$Clock,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)

ashots <- ashots %>%
  filter(Period < 4) %>%
  mutate(total_time_left = case_when(
    Period == 1 ~ Clock + 40,
    Period == 2 ~ Clock + 20,
    Period == 3 ~ Clock))

ashots[, Goal := ifelse(Event == 'Goal', 1,0)]
ashots[, dist := shot_dist_ohl(X.Coordinate, Y.Coordinate)]
ashots[, angle := mapply(shot_angle_ohl,X.Coordinate,Y.Coordinate)]
ashots[, dist_stan := stan(dist)]
ashots[, angle_stan := stan(angle)]

ishots <- olympic[Event == 'Goal' | Event == 'Shot']
ishots[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
ishots[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
ishots[, skater_diff := posteam_skaters-defteam_skaters]
ishots[, posteam_score := ifelse(Team == Home.Team, Home.Team.Goals, Away.Team.Goals)]
ishots[, defteam_score := ifelse(Team == Home.Team, Away.Team.Goals, Home.Team.Goals)]
ishots[, score_diff := posteam_score-defteam_score]

ishots$Clock <- sapply(strsplit(ishots$Clock,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)


ishots <- ishots %>%
  filter(Period < 4) %>%
  mutate(total_time_left = case_when(
    Period == 1 ~ Clock + 40,
    Period == 2 ~ Clock + 20,
    Period == 3 ~ Clock))

ishots[, Goal := ifelse(Event == 'Goal', 1,0)]
ishots[, dist := shot_dist(X.Coordinate, Y.Coordinate)]
ishots[, angle := mapply(shot_angle,X.Coordinate,Y.Coordinate)]
ishots[, dist_stan := stan(dist)]
ishots[, angle_stan := stan(angle)]

all_shots <- rbind(ashots, ishots)

all_shots <- all_shots %>%
  mutate(index = 1:n())

wp_games <- stats::predict(wp_model,
                             as.matrix(all_shots %>%
                                         dplyr::select(Period, Clock, score_diff, 
                                                skater_diff, total_time_left))) %>%
    tibble::as_tibble() %>%
    dplyr::rename(prob = "value") %>%
    dplyr::bind_cols(purrr::map_dfr(seq_along(all_shots$index), function(x) {
      tibble::tibble("wp" = 0:1,
                     "Period" = all_shots$Period[[x]],
                     "Clock" = all_shots$Clock[[x]],
                     "score_diff" = all_shots$score_diff[[x]],
                     "skater_diff" = all_shots$skater_diff[[x]],
                     "total_time_left" = all_shots$total_time_left[[x]],
                     "index" = all_shots$index[[x]])
    })) %>%
    dplyr::group_by(.data$index) %>%
    dplyr::mutate(cum_prob = cumsum(.data$prob),
                  prob = .data$prob) %>%
    dplyr::select(-.data$cum_prob) %>%
    dplyr::summarise(win_prob = sum(.data$prob * .data$wp)) %>%
    ungroup()   
  
win_prob <- all_shots %>%
  inner_join(wp_games) %>%
  dplyr::select(-index) %>%
  mutate(play_num = 1:n())





set.seed(35)
train_rows <- createDataPartition(win_prob$Goal, p = 0.8, list = F)
train_shots <- win_prob[train_rows]
test_shots <- win_prob[-train_rows]
glm_goal <- glm(Goal~dist_stan+ angle_stan+`Wrap Around`+Fan+One_timer, family = binomial(link='logit'), data = train_shots)
summary(glm_goal)


new_tr <- train_shots %>% 
  dplyr::select(dist_stan, angle_stan,win_prob)  %>% 
  as.matrix()

new_ts <- test_shots %>% 
  dplyr::select(dist_stan, angle_stan,win_prob)  %>% 
  as.matrix()

labels <- train_shots$Goal
ts_label <- test_shots$Goal

dtrain <- xgboost::xgb.DMatrix(data = new_tr,label = labels) 
dtest <-xgboost::xgb.DMatrix(data = new_ts,label=ts_label)

#MLR learner

lrn_tr <- train_shots %>% 
  dplyr::select(dist_stan, angle_stan,win_prob, Goal)  %>% 
  data.frame()
    
lrn_ts <- test_shots %>% 
  dplyr::select(dist_stan, angle_stan,win_prob, Goal)  %>% 
  data.frame()

lrn_tr$Goal <- as.factor(lrn_tr$Goal)
lrn_ts$Goal <- as.factor(lrn_ts$Goal)

traintask <- makeClassifTask (data = lrn_tr,target = "Goal")
testtask <- makeClassifTask (data = lrn_ts,target = "Goal")
traintask <- createDummyFeatures (obj = traintask)
testtask <- createDummyFeatures (obj = testtask)


lrn <- makeLearner("classif.xgboost",predict.type = "prob")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="auc", 
                      nrounds=1000,
                      mean_score = mean(train_shots$Goal), 
                      eta=0.005)

params_lrn <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree")), 
                        makeIntegerParam("max_depth",lower = 3L,upper = 25L), 
                        makeIntegerParam("gamma",lower = 0L,upper = 6L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.4,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.4,upper = 1))


rdesc <- makeResampleDesc("CV",stratify = T,iters=7L)
ctrl <- makeTuneControlRandom(maxit = 25L)


parallelStartSocket(cpus = detectCores())
set.seed(33)
mytune <- tuneParams(learner = lrn, 
                     task = traintask, 
                     resampling = rdesc, 
                     measures = auc, 
                     par.set = params_lrn, 
                     control = ctrl, show.info = T)


params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eval_metric = c('auc'),
               eta=0.005, 
               gamma=6, 
               min_child_weight=6.54, 
               max_depth = 4,
               subsample=0.473, 
               colsample_bytree=0.666,
               mean_score = mean(train_shots$Goal))

set.seed(33)
xgbcv <- xgboost::xgb.cv( params = params, 
                          data = dtrain, 
                          nrounds = 2000, 
                          nfold = 7, 
                          showsd = T, 
                          stratified = T, 
                          print_every_n = 20, 
                          early_stopping_rounds = 20, 
                          maximize = F)

set.seed(33)
xG_model <- xgboost::xgb.train(params = params, 
                               data = dtrain, 
                               nrounds = 700, 
                               watchlist = list(val=dtest,train=dtrain), 
                               print_every_n = 20, 
                               early_stop_round = 10, 
                               maximize = F)


impor <- xgboost::xgb.importance(colnames(dtrain), model = xG_model)
xgboost::xgb.plot.importance(impor)



xG <- predict(xG_model, dtest)
preds <- cbind(test_shots, xG)
mean(preds$Goal)
mean(preds$xG)

preds <- preds %>% mutate(pred_G = ifelse(xG >= 0.5, 1, 0))
preds$pred_G <- as.factor(preds$pred_G)
preds$Goal_fac <- as.factor(preds$Goal)
preds$dumb <- mean(preds$Goal)

AUC(preds$xG, preds$Goal_fac)
confusionMatrix(preds$pred_G, preds$Goal_fac)




###

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

prob <- preds %>% group_by(X.Coordinate, Y.Coordinate) %>% 
  summarise(mean_xG = mean(xG))


X.Coordinate <- c(seq(125,200,1))
Y.Coordinate <- c(seq(1,85,1))

cartesian <- merge(X.Coordinate,Y.Coordinate) %>% data.table() %>% 
  rename(X.Coordinate =x, Y.Coordinate = y)
cartesian[, One_timer_bin := 0]
cartesian[, Traffic_bin := ifelse(X.Coordinate >= 170, 0, 1)]
cartesian[, dist := shot_dist(X.Coordinate, Y.Coordinate)]
cartesian[, angle := mapply(shot_angle, X.Coordinate, Y.Coordinate)]
cartesian[, dist_stan := stan(dist)]
cartesian[, angle_stan := stan(angle)]
cartesian[, wp := 0.5]
cart_pred <- cartesian %>% 
  select(dist_stan, angle_stan, One_timer_bin, Traffic_bin,wp)%>% as.matrix()

xG_map <- predict(xG_model, cart_pred)
preds_map <- cbind(cartesian, xG_map)

ggplot(preds_map, aes(X.Coordinate, Y.Coordinate))+
  geom_tile(aes(fill = xG_map))


options(scipen = 9999)
map <- ggplot(preds, aes(X.Coordinate, Y.Coordinate)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(15), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
ggsave("xG_heatmap.png", map, device = 'png', dpi = 540)

ggplot(preds, aes(X.Coordinate, Y.Coordinate)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(10), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")


map_even_dif <- ggplot(preds[skater_dif == 0], aes(X.Coordinate, Y.Coordinate, fill= xG)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(10), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
ggsave("xG_heatmap_no_pp.png", map_even_dif, device = 'png', dpi = 540)

preds %>% mutate(points = ifelse(Goal_bin == 1, 1000, xG*1000)) %>% 
  group_by(Player) %>% 
  summarise(chance_score = sum(points),
            goals = sum(Goal_bin),
            shots = n(),
            avg_c_score = chance_score/shots) %>% 
  arrange(desc(avg_c_score))



  
