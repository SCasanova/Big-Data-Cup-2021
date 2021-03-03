pros_shots <- readRDS('scouting_win_prob.rds') %>% data.table()
pros_shots <- pros_shots %>% filter(Event == 'Shot' | Event == 'Goal')
pros_shots[, Goal := ifelse(Event == 'Goal', 1,0)]
pros_shots[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
pros_shots[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
pros_shots[, skater_diff := posteam_skaters-defteam_skaters]

pros_shots[, One_timer := ifelse(Detail.4 == 't', 1,0)]
pros_shots[, Traffic := ifelse(Detail.3 == 't', 1,0)]
pros_shots[, Snapshot := ifelse(Detail.1 == 'Snapshot', 1,0)]
pros_shots[, Wristshot := ifelse(Detail.1 == 'Wristshot', 1,0)]




pros_shots[, dist := shot_dist_ohl(X.Coordinate, Y.Coordinate)]
pros_shots[, angle := mapply(shot_angle_ohl, X.Coordinate, Y.Coordinate)]



set.seed(2021)
train_rows_p <- createDataPartition(pros_shots$Goal, p = 0.8, list = F)
train_shots_p <- pros_shots[train_rows_p]
test_shots_p <- pros_shots[-train_rows_p]

train_shots_p[, dist_stan := stan(dist)]
train_shots_p[, angle_stan := stan(angle)]
train_shots_p[, x_stan := stan(X.Coordinate)]
train_shots_p[, y_stan := stan(Y.Coordinate)]

test_shots_p[, dist_stan := stan(dist)]
test_shots_p[, angle_stan := stan(angle)]
test_shots_p[, x_stan := stan(X.Coordinate)]
test_shots_p[, y_stan := stan(Y.Coordinate)]

glm_goal <- glm(Goal~dist_stan+ angle_stan+One_timer+x_stan+Snapshot+Wristshot, family = binomial(link='logit'), data = train_shots_p)
summary(glm_goal)

prosp_tr <- train_shots_p %>% 
  dplyr::select(dist_stan, angle_stan, One_timer,x_stan, Snapshot, Wristshot,win_prob)  %>% 
  as.matrix()

prosp_ts <- test_shots_p %>% 
  dplyr::select(dist_stan, angle_stan, One_timer,x_stan, Snapshot, Wristshot,win_prob)  %>% 
  as.matrix()

labels_prosp <- train_shots_p$Goal
ts_label_prosp <- test_shots_p$Goal

prosptrain <- xgboost::xgb.DMatrix(data = prosp_tr,label = labels_prosp) 
prosptest <-xgboost::xgb.DMatrix(data = prosp_ts,label=ts_label_prosp)

#MLR learner

lrn_tr_prosp <- train_shots_p %>% 
  dplyr::select(dist_stan, angle_stan, One_timer,x_stan, Snapshot, Wristshot,win_prob, Goal)  %>% 
  data.frame()
    
lrn_ts_prosp <- test_shots_p %>% 
  dplyr::select(dist_stan, angle_stan, One_timer,x_stan, Snapshot, Wristshot,win_prob, Goal)  %>% 
  data.frame()

lrn_tr_prosp$Goal <- as.factor(lrn_tr_prosp$Goal)
lrn_ts_prosp$Goal <- as.factor(lrn_ts_prosp$Goal)

traintask_prosp <- makeClassifTask (data = lrn_tr_prosp,target = "Goal")
testtask_prosp <- makeClassifTask (data = lrn_ts_prosp,target = "Goal")
traintask_prosp <- createDummyFeatures (obj = traintask_prosp)
testtask_prosp <- createDummyFeatures (obj = testtask_prosp)


lrn_prosp <- makeLearner("classif.xgboost",predict.type = "prob")
lrn_prosp$par.vals <- list( objective="binary:logistic", eval_metric="auc", 
                            nrounds=2800, eta=0.005,
                            booster = 'gbtree')
params_lrn_prosp <- makeParamSet(makeIntegerParam("max_depth",lower = 3L,upper = 30L), 
                        makeIntegerParam("gamma",lower = 0L,upper = 6L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.4,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.4,upper = 1))


rdesc_prosp <- makeResampleDesc("CV",stratify = T,iters=7L)
ctrl_prosp <- makeTuneControlRandom(maxit = 25L)


parallelStartSocket(cpus = detectCores())
set.seed(33)
prosptune <- tuneParams(learner = lrn_prosp, 
                     task = traintask_prosp, 
                     resampling = rdesc_prosp, 
                     measures = auc, 
                     par.set = params_lrn_prosp, 
                     control = ctrl_prosp, show.info = T)


params_prosp <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eval_metric = c('auc'),
               eta=0.005, 
               gamma=6, 
               min_child_weight=8.1, 
               max_depth = 10,
               subsample=0.747, 
               colsample_bytree=0.909)

set.seed(33)
xgbcv_prosp <- xgboost::xgb.cv( params = params_prosp, 
                          data = prosptrain, 
                          nrounds = 2000, 
                          nfold = 7, 
                          showsd = T, 
                          stratified = T, 
                          print_every_n = 20, 
                          early_stopping_rounds = 20, 
                          maximize = F)

set.seed(33)
xG_model_prosp <- xgboost::xgb.train(params = params_prosp, 
                               data = prosptrain, 
                               nrounds = 3000, 
                               watchlist = list(val=prosptest,train=prosptrain), 
                               print_every_n = 20, 
                               early_stop_round = 10, 
                               maximize = F)

par(mfrow=c(1,1))
impor_prosp <- xgboost::xgb.importance(colnames(prosptrain), model = xG_model_prosp)
xgboost::xgb.plot.importance(impor_prosp)


xG_prosp <- predict(xG_model_prosp, prosptest)

preds_prosp <- cbind(test_shots_p, xG_prosp)

View(preds_prosp)
mean(preds_prosp$Goal)
mean(preds_prosp$xG_prosp)


preds_prosp <- preds_prosp %>% mutate(pred_G = ifelse(xG_prosp >= 0.5, 1, 0))
preds_prosp$pred_G <- as.factor(preds_prosp$pred_G)
preds_prosp$Goal_fac <- as.factor(preds_prosp$Goal)
preds_prosp$Goal <- as.numeric(preds_prosp$Goal)-1

preds_prosp$dumb <- 0.05294

AUC(preds_prosp$dumb, preds_prosp$Goal)
AUC(preds_prosp$xG_prosp, preds_prosp$Goal)


confusionMatrix(preds_prosp$pred_G, preds_prosp$Goal)

preds_vs_goals <- preds_prosp %>% 
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
  geom_point(color ='#090A62', alpha=0.8, size = preds_vs_goals$shots/9)+
  labs(x = 'Goals',
       y = 'Expected Goal',
       title = 'Relationship Between xG and Goals',
       caption = paste('Correlation:', xg_cor))+
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5))


###


jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))


write.csv(prob_prosp, 'heatmap_file.csv')



##

library(MBA)

prob_prosp=prob_prosp[ order(prob_prosp[,1], prob_prosp[,2],prob_prosp[,3]), ]
mba.int <- mba.surf(prob_prosp, 500, 500, extend=T)$xyz.est
library(fields)
fields::image.plot(mba.int)

options(scipen = 9999)
map_prosp <- ggplot(preds_prosp, aes(X.Coordinate, Y.Coordinate, fill= xG_prosp)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(10), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
#ggsave("xG_heatmap.png", map, device = 'png', dpi = 540)



p_heatmap <- ggplot(prob_prosp, aes(X.Coordinate, Y.Coordinate))+
  geom_point(aes(color = value*100), size = 5, alpha = 0.5)+
  theme_void()+
  scale_color_viridis(option = "A")
ggsave("point_heatmap.png", p_heatmap, device = 'png', dpi = 540, bg = 'transparent')


map_even_dif_prosp <- ggplot(preds_prosp[skater_dif == 0], aes(X.Coordinate, Y.Coordinate, fill= xG_prosp)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(10), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
#ggsave("xG_heatmap_no_pp.png", map_even_dif, device = 'png', dpi = 540)

