passes <- data.table(read_csv('https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv')) %>% 
  filter((Event == 'Play' | Event == 'Incomplete Play') )

passes[, Success := ifelse(Event == 'Play', 1, 0)]
passes[, pass_dist := sqrt(( `X Coordinate`-`X Coordinate 2`)^2+(`Y Coordinate`-`Y Coordinate 2`)^2)]
passes[, pass_dist_stan := stan(pass_dist)]
passes[, posteam_skaters := ifelse(Team == `Home Team`, `Home Team Skaters`, `Away Team Skaters`)]
passes[, defteam_skaters := ifelse(Team == `Home Team`, `Away Team Skaters`, `Home Team Skaters`)]
passes[, skater_dif := posteam_skaters-defteam_skaters]
passes[, direct := ifelse(`Detail 1` == 'Direct',1,0)]


set.seed(2021)
train_rows_pass <- createDataPartition(passes$Success, p = 0.8, list = F)
train_pass <- passes[train_rows_pass]
test_pass <- passes[-train_rows_pass]

glm_pass <- glm(Success~pass_dist_stan+skater_dif+`X Coordinate`+`X Coordinate 2`+ `direct`, family = binomial(link='logit'), data = train_pass)
summary(glm_pass)

cp_tr <- train_pass %>% 
  dplyr::select(pass_dist_stan,skater_dif,`X Coordinate`,`X Coordinate 2`, direct)  %>% 
  as.matrix()

cp_ts <- test_pass %>% 
  dplyr::select(pass_dist_stan,skater_dif,`X Coordinate`,`X Coordinate 2`, direct)  %>% 
  as.matrix()

labels_pass <- train_pass$Success
ts_label_pass <- test_pass$Success

passtrain <- xgboost::xgb.DMatrix(data = cp_tr,label = labels_pass) 
passtest <-xgboost::xgb.DMatrix(data = cp_ts,label=ts_label_pass)


params_pass <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eval_metric = c('logloss'),
               eta=0.01, 
               gamma=5, 
               min_child_weight=6.96, 
               max_depth = 29,
               subsample=0.515, 
               colsample_bytree=0.809,
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
                               nrounds = 569, 
                               watchlist = list(val=passtest,train=passtrain), 
                               print_every_n = 20, 
                               early_stop_round = 10, 
                               maximize = F)

par(mfrow=c(1,1))
impor_pass <- xgboost::xgb.importance(colnames(passtrain), model = cp_model)
xgboost::xgb.plot.importance(impor_pass)

#MLR learner

lrn_tr_pass <- train_pass %>% 
  dplyr::select(pass_dist_stan,skater_dif,`X Coordinate`,`X Coordinate 2`, direct ,Success)  %>% 
  data.frame()
    
lrn_ts_pass <- test_pass %>% 
  dplyr::select(pass_dist_stan,skater_dif,`X Coordinate`,`X Coordinate 2`, direct ,Success)  %>% 
  data.frame()

lrn_tr_pass$Success <- as.factor(lrn_tr_pass$Success)
lrn_ts_pass$Success <- as.factor(lrn_ts_pass$Success)

traintask_pass <- makeClassifTask (data = lrn_tr_pass,target = "Success")
testtask_pass <- makeClassifTask (data = lrn_ts_pass,target = "Success")
traintask_pass <- createDummyFeatures (obj = traintask_pass)
testtask_pass <- createDummyFeatures (obj = testtask_pass)


lrn_pass <- makeLearner("classif.xgboost",predict.type = "prob")
lrn_pass$par.vals <- list( objective="binary:logistic", eval_metric="logloss", 
                           nrounds=600, 
                           eta=0.01, base_score =mean(train_pass$Success), 
                           booster = 'gbtree')
params_lrn_pass <- makeParamSet(makeIntegerParam("max_depth",lower = 3L,upper = 30L), 
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
                     control = ctrl_pass, show.info = T)

lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
#xgmodel <- train(learner = lrn_tune,task = traintask)

#predict model and vis

CP <- predict(cp_model, passtest)

cp_prosp <- cbind(test_pass, CP)

#View(preds_prosp)
mean(cp_prosp$Success)
mean(cp_prosp$CP)

cp_prosp <- cp_prosp %>% mutate(pass_comp = ifelse(CP >= 0.5, 1, 0))
cp_prosp$pass_comp <- as.factor(cp_prosp$pass_comp)
cp_prosp$Success_fac <- as.factor(cp_prosp$Success)


AUC(cp_prosp$CP, cp_prosp$Success)
confusionMatrix(cp_prosp$pass_comp, cp_prosp$Success_fac)


xpass_vs_pass <- cp_prosp %>% 
  group_by(Player) %>% 
  summarise(xpass = sum(cp),
            pass_comp = sum(Success),
            passes = n()) %>% 
  #filter(shots>=10) %>% 
  arrange(desc(xpass))

xg_cor <- round(cor(cp_prosp$CP, cp_prosp$Success),3)

ggplot(preds_vs_goals, aes(goals,xgoals)) +
  theme_minimal()+
  geom_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  geom_point(color ='#090A62', alpha=0.8, size = preds_vs_goals$shots/50)+
  labs(x = 'Completed passes',
       y = 'Expected Goal',
       title = 'Relationship Between xG and Goals',
       caption = paste('Correlation:', xg_cor))+
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5))


