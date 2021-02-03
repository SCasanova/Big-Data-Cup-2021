new_tr <- #train split %>% 
  select(#variables for prediction)  %>% 
  as.matrix()

new_ts <- #test split %>% 
  select(#variables for prediction)  %>% 
  as.matrix()

labels <- #variable to predict
ts_label <- #variable to predict

dtrain <- xgboost::xgb.DMatrix(data = new_tr,label = labels) 
dtest <-xgboost::xgb.DMatrix(data = new_ts,label=ts_label)


params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.05, 
               gamma=2, 
               min_child_weight=5, 
               max_depth = 5,
               subsample=1, 
               colsample_bytree=1)

xgbcv <- xgboost::xgb.cv( params = params, 
                          data = dtrain, 
                          nrounds = 150, 
                          nfold = 5, 
                          showsd = T, 
                          stratified = T, 
                          print_every_n = 20, 
                          eval_metric = 'logloss',
                          early_stopping_rounds = 20, 
                          maximize = F)

set.seed(33)
xG_model <- xgboost::xgb.train (params = params, 
                               data = dtrain, 
                               nrounds = 100, 
                               watchlist = list(val=dtest,train=dtrain), 
                               print_every_n = 20, 
                               early_stop_round = 10, 
                               maximize = F , 
                               eval_metric = "logloss")


impor <- xgboost::xgb.importance(colnames(dtrain), model = xG_model)
xgboost::xgb.plot.importance(impor)

#MLR learner

lrn_tr <- #train split %>% 
  select(#variables for prediction + variable to predict)  %>% 
  data.frame()
    
lrn_ts <- #test split %>% 
  select(#variables for prediction + variable to predict)  %>% 
  data.frame()

lrn_tr$#variable to predict <- as.factor(lrn_tr$#variable to predict)
lrn_ts$#variable to predict <- as.factor(lrn_ts$#variable to predict)

traintask <- makeClassifTask (data = lrn_tr,target = "#variable to predict")
testtask <- makeClassifTask (data = lrn_ts,target = "#variable to predict")
traintask <- createDummyFeatures (obj = traintask)
testtask <- createDummyFeatures (obj = testtask)


lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="logloss", nrounds=250, eta=0.05)
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")), 
                        makeIntegerParam("max_depth",lower = 3L,upper = 15L), 
                        makeIntegerParam("gamma",lower = 0L,upper = 6L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.4,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.4,upper = 1))


rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)
ctrl <- makeTuneControlRandom(maxit = 20L)


parallelStartSocket(cpus = detectCores())
set.seed(33)
mytune <- tuneParams(learner = lrn, 
                     task = traintask, 
                     resampling = rdesc, 
                     measures = acc, 
                     par.set = params, 
                     control = ctrl, show.info = T)

lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
xgmodel <- train(learner = lrn_tune,task = traintask)

#predict model
xG <- predict(xgmodel,testtask)

