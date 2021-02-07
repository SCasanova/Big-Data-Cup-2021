
glm_pass <- glm(Success~pass_dist_stan+ skater_dif+X.Coordinate+X.Coordinate.2+Y.Coordinate+Y.Coordinate.2, family = binomial(link='logit'), data = train_pass)
summary(glm_pass)

cp_tr <- train_pass %>% 
  select(pass_dist_stan,skater_dif,X.Coordinate,X.Coordinate.2,Y.Coordinate,Y.Coordinate.2)  %>% 
  as.matrix()

cp_ts <- test_pass %>% 
  select(pass_dist_stan, skater_dif,X.Coordinate,X.Coordinate.2,Y.Coordinate,Y.Coordinate.2)  %>% 
  as.matrix()

labels_pass <- train_pass$Success
ts_label_pass <- test_pass$Success

passtrain <- xgboost::xgb.DMatrix(data = cp_tr,label = labels_pass) 
passtest <-xgboost::xgb.DMatrix(data = cp_ts,label=ts_label_pass)


params_pass <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eval_metric = c('logloss'),
               eta=0.01, 
               gamma=6, 
               min_child_weight=9.09, 
               max_depth = 17,
               subsample=0.401, 
               colsample_bytree=0.785,
               mean_score = mean(train_pass$Success))

set.seed(33)
xgbcv_pass <- xgboost::xgb.cv( params = params_pass, 
                          data = passtrain, 
                          nrounds = 2000, 
                          nfold = 7, 
                          showsd = T, 
                          stratified = T, 
                          print_every_n = 20, 
                          early_stopping_rounds = 20, 
                          maximize = F)

set.seed(33)
cp_model <- xgboost::xgb.train(params = params_pass, 
                               data = passtrain, 
                               nrounds = 896, 
                               watchlist = list(val=passtest,train=passtrain), 
                               print_every_n = 20, 
                               early_stop_round = 10, 
                               maximize = F)

par(mfrow=c(1,1))
impor_pass <- xgboost::xgb.importance(colnames(passtrain), model = CP_model)
xgboost::xgb.plot.importance(impor_pass)

#MLR learner

lrn_tr_pass <- train_pass %>% 
  select(pass_dist_stan,skater_dif,X.Coordinate,X.Coordinate.2,Y.Coordinate,Y.Coordinate.2, Success)  %>% 
  data.frame()
    
lrn_ts_pass <- test_pass %>% 
  select(pass_dist_stan,skater_dif,X.Coordinate,X.Coordinate.2,Y.Coordinate,Y.Coordinate.2, Success)  %>% 
  data.frame()

lrn_tr_pass$Success <- as.factor(lrn_tr_pass$Success)
lrn_ts_pass$Success <- as.factor(lrn_ts_pass$Success)

traintask_pass <- makeClassifTask (data = lrn_tr_pass,target = "Success")
testtask_pass <- makeClassifTask (data = lrn_ts_pass,target = "Success")
traintask_prosp <- createDummyFeatures (obj = traintask_pass)
testtask_prosp <- createDummyFeatures (obj = testtask_pass)


lrn_pass <- makeLearner("classif.xgboost",predict.type = "response")
lrn_pass$par.vals <- list( objective="binary:logistic", eval_metric="logloss", nrounds=600, eta=0.01, base_score =mean(train_pass$Success), booster = 'gbtree')
params_lrn_pass <- makeParamSet(makeIntegerParam("max_depth",lower = 3L,upper = 20L), 
                        makeIntegerParam("gamma",lower = 0L,upper = 6L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.4,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.4,upper = 1))


rdesc_pass <- makeResampleDesc("CV",stratify = T,iters=7L)
ctrl_pass <- makeTuneControlRandom(maxit = 25L)


parallelStartSocket(cpus = detectCores())
set.seed(33)
passtune <- tuneParams(learner = lrn_pass, 
                     task = traintask_pass, 
                     resampling = rdesc_pass, 
                     measures = acc, 
                     par.set = params_lrn_pass, 
                     control = ctrl_prosp, show.info = T)

lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
#xgmodel <- train(learner = lrn_tune,task = traintask)

#predict model and vis

CP <- predict(CP_model, passtest)

cp_prosp <- cbind(test_pass, CP)

#View(preds_prosp)
mean(cp_prosp$Success)
mean(cp_prosp$CP)



  

