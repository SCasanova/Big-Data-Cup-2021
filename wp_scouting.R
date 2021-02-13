library(tidyverse)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(na.tools)
library(ggimage)
library(gt)
library(mgcv)
library(scales)
library(ggforce)
library(remotes)
library(ggtext)
library(reshape2)

scouting <- read.csv("~/Desktop/hackathon_scouting.csv")

scouting$Clock <- sapply(strsplit(scouting$Clock,":"),
                       function(x) {
                         x <- as.numeric(x)
                         x[1]+x[2]/60
                       }
)

scouting <- scouting %>%
  filter(Period < 4) %>%
  mutate(total_time_left = case_when(
    Period == 1 ~ Clock + 40,
    Period == 2 ~ Clock + 20,
    Period == 3 ~ Clock))

scouting <- transform(scouting,game_num=as.numeric(factor(scouting$game_date)))

scout_winners <- scouting %>%
  filter(total_time_left < 0.2) %>%
  group_by(game_date, Home.Team, Away.Team, game_num) %>%
  summarize(home_final = mean(Home.Team.Goals, na.rm = T),
            away_final = mean(Away.Team.Goals, na.rm = T)) %>%
  mutate(winner = case_when(
    home_final > away_final ~ Home.Team,
    home_final == away_final ~ "TIE",
    home_final < away_final ~ Away.Team
  ))

scouting <- scouting %>%
  left_join(scout_winners)

scouting <- scouting %>%
  mutate(score_diff = case_when(
    Home.Team == Team ~ Home.Team.Goals - Away.Team.Goals,
    Away.Team == Team ~ Away.Team.Goals - Away.Team.Goals
  ))

scouting <- scouting %>%
  mutate(skater_diff = case_when(
    Home.Team == Team ~ Home.Team.Skaters - Away.Team.Skaters,
    Away.Team == Team ~ Away.Team.Skaters - Home.Team.Skaters
  ))

scouting_data <- scouting %>%
  filter(!is.na(winner)) %>%
  filter(winner != "TIE") %>%
  mutate(label = ifelse(Team == winner, 1, 0)) %>%
  select(label, Period, Clock, score_diff, skater_diff, total_time_left)

smp_size <- floor(0.80 * nrow(model_data))
set.seed(123)
ind <- sample(seq_len(nrow(model_data)), size = smp_size)
ind_train <- model_data[ind, ]
ind_test <- model_data[-ind, ]

nrounds <- 100
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = 2,
    eta = .025,
    gamma = 2,
    subsample=0.8,
    colsample_bytree=0.8,
    max_depth = 4,
    min_child_weight = 1
  )

full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = ind_train %>% select(-label)),
                                  label = ind_train$label)

set.seed(123) 
scouting_wp <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

importance <- xgboost::xgb.importance(feature_names = colnames(scouting_wp), model = scouting_wp)
xgb.plot.importance(importance)

model_scouting_games <- model_data %>%
  mutate(index = 1:n())

wp_scouting_games <- stats::predict(wp_model,
                           as.matrix(model_scouting_games %>%
                                       select(Period, Clock, score_diff, 
                                              skater_diff, total_time_left))) %>%
  tibble::as_tibble() %>%
  dplyr::rename(prob = "value") %>%
  dplyr::bind_cols(purrr::map_dfr(seq_along(model_scouting_games$index), function(x) {
    tibble::tibble("wp" = 0:1,
                   "Period" = model_games$Period[[x]],
                   "Clock" = model_games$Clock[[x]],
                   "score_diff" = model_games$score_diff[[x]],
                   "skater_diff" = model_games$skater_diff[[x]],
                   "total_time_left" = model_games$total_time_left[[x]],
                   "index" = model_games$index[[x]])
  })) %>%
  dplyr::group_by(.data$index) %>%
  dplyr::mutate(cum_prob = cumsum(.data$prob),
                prob = .data$prob) %>%
  dplyr::select(-.data$cum_prob) %>%
  dplyr::summarise(win_prob = sum(.data$prob * .data$wp)) %>%
  ungroup()   

scouting_win_prob <- model_scouting_games %>%
  inner_join(wp_scouting_games) %>%
  select(-label, -index) %>%
  mutate(play_num = 1:n())

scouting_win_prob2 <- scouting %>%
  filter(winner != "TIE") %>%
  mutate(label = ifelse(Team == winner, 1, 0),
         play_num = 1:n()) 

scouting_win_prob <- scouting_win_prob %>%
  left_join(scouting_win_prob2)

scouting_win_prob <- scouting_win_prob %>%
  mutate(home_wp = case_when(
    Home.Team == Team ~  win_prob,
    Away.Team == Team ~ 1 - win_prob))
scouting_win_prob <- scouting_win_prob %>%
  mutate(away_wp = case_when(
    Home.Team == Team ~  1 - win_prob,
    Away.Team == Team ~ win_prob))

scouting_game1 <- scouting_win_prob %>%
  filter(game_num == 1)




