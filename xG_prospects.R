
prosp_tr <- train_shots_p %>% 
  select(dist_stan, angle_stan, One_timer_bin, Traffic_bin, skater_dif)  %>% 
  as.matrix()

prosp_ts <- test_shots_p %>% 
  select(dist_stan, angle_stan, One_timer_bin, Traffic_bin, skater_dif)  %>% 
  as.matrix()

labels_prosp <- train_shots_p$Goal_bin
ts_label_prosp <- test_shots_p$Goal_bin

prosptrain <- xgboost::xgb.DMatrix(data = prosp_tr,label = labels_prosp) 
prosptest <-xgboost::xgb.DMatrix(data = prosp_ts,label=ts_label_prosp)


params_prosp <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eval_metric = c('logloss'),
               eta=0.01, 
               gamma=0, 
               min_child_weight=4.87, 
               max_depth = 11,
               subsample=0.885, 
               colsample_bytree=0.79)

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
                               nrounds = 451, 
                               watchlist = list(val=prosptest,train=prosptrain), 
                               print_every_n = 20, 
                               early_stop_round = 10, 
                               maximize = F)

par(mfrow=c(1,1))
impor_prosp <- xgboost::xgb.importance(colnames(prosptrain), model = xG_model_prosp)
xgboost::xgb.plot.importance(impor_prosp)

#MLR learner

lrn_tr_prosp <- train_shots_p %>% 
  select(dist_stan, angle_stan, One_timer_bin, Traffic_bin, skater_dif, Goal_bin)  %>% 
  data.frame()
    
lrn_ts_prosp <- test_shots_p %>% 
  select(dist_stan, angle_stan, One_timer_bin, Traffic_bin, skater_dif, Goal_bin)  %>% 
  data.frame()

lrn_tr_prosp$Goal_bin <- as.factor(lrn_tr_prosp$Goal_bin)
lrn_ts_prosp$Goal_bin <- as.factor(lrn_ts_prosp$Goal_bin)

traintask_prosp <- makeClassifTask (data = lrn_tr_prosp,target = "Goal_bin")
testtask_prosp <- makeClassifTask (data = lrn_ts_prosp,target = "Goal_bin")
traintask_prosp <- createDummyFeatures (obj = traintask_prosp)
testtask_prosp <- createDummyFeatures (obj = testtask_prosp)


lrn_prosp <- makeLearner("classif.xgboost",predict.type = "response")
lrn_prosp$par.vals <- list( objective="binary:logistic", eval_metric="logloss", nrounds=600, eta=0.01)
params_lrn_prosp <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")), 
                        makeIntegerParam("max_depth",lower = 3L,upper = 15L), 
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
                     measures = acc, 
                     par.set = params_lrn_prosp, 
                     control = ctrl_prosp, show.info = T)

lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
#xgmodel <- train(learner = lrn_tune,task = traintask)

#predict model and vis

xG_prosp <- predict(xG_model_prosp, prosptest)

preds_prosp <- cbind(test_shots_p, xG_prosp)

View(preds_prosp)
mean(preds_prosp$Goal_bin)
mean(preds_prosp$xG_prosp)


jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

options(scipen = 9999)
map_prosp <- ggplot(preds_prosp, aes(X.Coordinate, Y.Coordinate, fill= xG_prosp)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(10), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
#ggsave("xG_heatmap.png", map, device = 'png', dpi = 540)

map_even_dif_prosp <- ggplot(preds_prosp[skater_dif == 0], aes(X.Coordinate, Y.Coordinate, fill= xG_prosp)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(10), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
#ggsave("xG_heatmap_no_pp.png", map_even_dif, device = 'png', dpi = 540)

cg_score <- preds_prosp %>% mutate(points = ifelse(Goal_bin == 1, 1000, xG_prosp*1000)) %>% 
  group_by(Player) %>% 
  summarise(chance_goal_score = sum(points),
            goals = sum(Goal_bin),
            shots = n(),
            avg_cg_score = chance_goal_score/shots) %>% 
  filter(shots >= 5) %>% 
  arrange(desc(avg_cg_score))
  
c_score <- preds_prosp %>% mutate(points = xG_prosp*1000) %>% 
  group_by(Player) %>% 
  summarise(chance_score = sum(points),
            goals = sum(Goal_bin),
            shots = n(),
            avg_c_score = chance_score/shots) %>% 
  filter(shots >= 5) %>% 
  arrange(desc(avg_c_score)) %>% 
  select(-goals, -shots)
