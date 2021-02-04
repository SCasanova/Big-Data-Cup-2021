summary(lm(Goal_bin~dist_stan+ angle_stan+One_timer_bin+skater_dif, data = train_shots))


new_tr <- train_shots %>% 
  select(dist_stan, angle_stan, One_timer_bin, skater_dif)  %>% 
  as.matrix()

new_ts <- test_shots %>% 
  select(dist_stan, angle_stan, One_timer_bin, skater_dif)  %>% 
  as.matrix()

labels <- train_shots$Goal_bin
ts_label <- test_shots$Goal_bin

dtrain <- xgboost::xgb.DMatrix(data = new_tr,label = labels) 
dtest <-xgboost::xgb.DMatrix(data = new_ts,label=ts_label)


params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eval_metric = c('logloss'),
               eta=0.01, 
               gamma=5, 
               min_child_weight=4.59, 
               max_depth = 10,
               subsample=0.977, 
               colsample_bytree=0.655)

set.seed(33)
xgbcv <- xgboost::xgb.cv( params = params, 
                          data = dtrain, 
                          nrounds = 5000, 
                          nfold = 7, 
                          showsd = T, 
                          stratified = T, 
                          print_every_n = 20, 
                          early_stopping_rounds = 20, 
                          maximize = F)

set.seed(33)
xG_model <- xgboost::xgb.train(params = params, 
                               data = dtrain, 
                               nrounds = 630, 
                               watchlist = list(val=dtest,train=dtrain), 
                               print_every_n = 20, 
                               early_stop_round = 10, 
                               maximize = F)


impor <- xgboost::xgb.importance(colnames(dtrain), model = xG_model)
xgboost::xgb.plot.importance(impor)

#MLR learner

lrn_tr <- train_shots %>% 
  select(dist_stan, angle_stan, One_timer_bin, skater_dif, Goal_bin)  %>% 
  data.frame()
    
lrn_ts <- test_shots %>% 
  select(dist_stan, angle_stan, One_timer_bin, skater_dif, Goal_bin)  %>% 
  data.frame()

lrn_tr$Goal_bin <- as.factor(lrn_tr$Goal_bin)
lrn_ts$Goal_bin <- as.factor(lrn_ts$Goal_bin)

traintask <- makeClassifTask (data = lrn_tr,target = "Goal_bin")
testtask <- makeClassifTask (data = lrn_ts,target = "Goal_bin")
traintask <- createDummyFeatures (obj = traintask)
testtask <- createDummyFeatures (obj = testtask)


lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="logloss", nrounds=1500, eta=0.01)
params_lrn <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")), 
                        makeIntegerParam("max_depth",lower = 3L,upper = 15L), 
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
                     measures = acc, 
                     par.set = params_lrn, 
                     control = ctrl, show.info = T)

lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
#xgmodel <- train(learner = lrn_tune,task = traintask)

#predict model and vis

xG <- predict(xG_model, dtest)

preds <- cbind(test_shots, xG)

View(preds)
mean(preds$xG)

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

options(scipen = 9999)
map <- ggplot(preds, aes(X.Coordinate, Y.Coordinate, fill= xG)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(10), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")

ggsave("xG_heatmap.png", map, device = 'png', dpi = 540)


