womens1 <- data.table(read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv'))
womens2 <- data.table(read.csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_nwhl.csv'))

wpasses <- rbind(womens1, womens2) %>% filter(Event == 'Play' | Event == 'Incomplete Play')

wpasses[, Success := ifelse(Event == 'Play', 1, 0)]
wpasses[, pass_dist := sqrt((X.Coordinate-X.Coordinate.2)^2+(Y.Coordinate-Y.Coordinate.2)^2)]
wpasses[, direct := ifelse(Detail.1 == 'Direct', 1, 0)]
wpasses[, pass_dist_stan := stan(pass_dist)]
wpasses[, posteam_skaters := ifelse(Team == Home.Team, Home.Team.Skaters, Away.Team.Skaters)]
wpasses[, defteam_skaters := ifelse(Team == Home.Team, Away.Team.Skaters, Home.Team.Skaters)]
wpasses[, skater_diff := posteam_skaters-defteam_skaters]
wpasses[, x_stan := stan(X.Coordinate)]
wpasses[, x_stan2 := stan(X.Coordinate.2)]


set.seed(2021)
train_rows_wpass <- createDataPartition(wpasses$Success, p = 0.8, list = F)
train_wpass <- wpasses[train_rows_wpass]
test_wpass <- wpasses[-train_rows_wpass]

glm_wpass <- glm(Success~pass_dist_stan+ skater_diff+x_stan+x_stan2+direct, family = binomial(link='logit'), data = train_wpass)
summary(glm_wpass)

wcp_tr <- train_wpass %>% 
  dplyr::select(pass_dist_stan,skater_diff,x_stan,x_stan2, direct)  %>% 
  as.matrix()

wcp_ts <- test_wpass %>% 
  dplyr::select(pass_dist_stan,skater_diff,x_stan,x_stan2, direct)  %>% 
  as.matrix()

labels_wpass <- train_wpass$Success
ts_label_wpass <- test_wpass$Success

wpasstrain <- xgboost::xgb.DMatrix(data = wcp_tr,label = labels_wpass) 
wpasstest <-xgboost::xgb.DMatrix(data = wcp_ts,label=ts_label_wpass)

#MLR learner

lrn_tr_wpass <- train_wpass %>% 
  dplyr::select(pass_dist_stan,skater_diff,x_stan,x_stan2, direct, Success)  %>% 
  data.frame()
    
lrn_ts_wpass <- test_wpass %>% 
  dplyr::select(pass_dist_stan,skater_diff,x_stan,x_stan2, direct, Success)  %>% 
  data.frame()

lrn_tr_wpass$Success <- as.factor(lrn_tr_wpass$Success)
lrn_ts_wpass$Success <- as.factor(lrn_ts_wpass$Success)

traintask_wpass <- makeClassifTask (data = lrn_tr_wpass,target = "Success")
testtask_wpass <- makeClassifTask (data = lrn_ts_wpass,target = "Success")
traintask_wpass <- createDummyFeatures (obj = traintask_wpass)
testtask_wpass <- createDummyFeatures (obj = testtask_wpass)


lrn_wpass <- makeLearner("classif.xgboost",predict.type = "prob")
lrn_wpass$par.vals <- list( objective="binary:logistic", eval_metric="logloss", 
                           nrounds=800, 
                           eta=0.01, 
                           base_score =mean(train_wpass$Success), 
                           booster = 'gbtree')
params_lrn_wpass <- makeParamSet(makeIntegerParam("max_depth",lower = 3L,upper = 25L), 
                        makeIntegerParam("gamma",lower = 0L,upper = 6L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.4,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.4,upper = 1))


rdesc_wpass <- makeResampleDesc("CV",stratify = T,iters=7L)
ctrl_wpass <- makeTuneControlRandom(maxit = 25L)


parallelStartSocket(cpus = detectCores())
set.seed(33)
passtune <- tuneParams(learner = lrn_wpass, 
                     task = traintask_wpass, 
                     resampling = rdesc_wpass, 
                     measures = logloss, 
                     par.set = params_lrn_wpass, 
                     control = ctrl_wpass, show.info = T)


params_wpass <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eval_metric = c('auc'),
               eta=0.01, 
               gamma=6, 
               min_child_weight=4.65, 
               max_depth = 25,
               subsample=0.565, 
               colsample_bytree=0.889,
               mean_score = mean(train_wpass$Success))

set.seed(33)
xgbcv_wpass <- xgboost::xgb.cv( params = params_wpass, 
                          data = wpasstrain, 
                          nrounds = 2000, 
                          nfold = 7, 
                          showsd = T, 
                          stratified = T, 
                          print_every_n = 20, 
                          early_stopping_rounds = 20, 
                          maximize = F)

set.seed(33)
wcp_model <- xgboost::xgb.train(params = params_wpass, 
                               data = wpasstrain, 
                               nrounds = 361, 
                               watchlist = list(val=wpasstest,train=wpasstrain), 
                               print_every_n = 20, 
                               early_stop_round = 10, 
                               maximize = F)

par(mfrow=c(1,1))
impor_wpass <- xgboost::xgb.importance(colnames(wpasstrain), model = wcp_model)
xgboost::xgb.plot.importance(impor_wpass)

cp <- predict(wcp_model, wpasstest)

wcp <- cbind(test_wpass, cp)

#View(preds_prosp)
mean(wcp$Success)
mean(wcp$cp)

wcp <- wcp %>% mutate(pass_comp = ifelse(cp >= 0.5, 1, 0))
wcp$pass_comp <- as.factor(wcp$pass_comp)
wcp$Success_fac <- as.factor(wcp$Success)
wcp$dumb <- mean(wcp$Success)

AUC(wcp$cp, wcp$Success)
AUC(wcp$dumb, wcp$Success)
LogLoss(wcp$cp, wcp$Success)
LogLoss(wcp$dumb, wcp$Success)


confusionMatrix(wcp$pass_comp, wcp$Success_fac)

  

