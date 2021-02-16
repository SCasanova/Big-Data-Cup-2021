

wom_shots_preds <- no_neg_dif %>% 
  select(dist_stan, angle_stan, One_timer_bin, Traffic_bin, wp) %>% 
  as.matrix()

shot_wom <- no_neg_dif

xG <- predict(xG_model, wom_shots_preds)

full_shot_preds <- cbind(shot_wom, xG)

cg_score_w <- full_shot_preds %>% mutate(points = ifelse(Goal_bin == 1, 1000, xG*1000)) %>% 
  group_by(Player) %>% 
  summarise(chance_goal_score = sum(points),
            goals = sum(Goal_bin),
            shots = n(),
            avg_cg_score = chance_goal_score/shots) %>% 
  filter(shots >= 30) %>% 
  arrange(desc(avg_cg_score))
  
c_score_w <- full_shot_preds %>% mutate(points = xG*1000) %>% 
  group_by(Player) %>% 
  summarise(chance_score = sum(points),
            goals = sum(Goal_bin),
            shots = n(),
            avg_c_score = chance_score/shots) %>% 
  filter(shots >= 30) %>% 
  arrange(desc(avg_c_score)) %>% 
  select(-goals, -shots)


c_plus_cg_w <- merge(cg_score_w, c_score_w, by = 'Player')


c_plus_cg_w %>% arrange(desc(avg_cg_score)) %>% head(5)
c_plus_cg_w %>% arrange(desc(avg_c_score)) %>% head(5)
c_plus_cg_w %>% arrange(desc(avg_c_score)) %>% tail(20)
c_plus_cg_w %>% arrange(desc(avg_cg_score)) %>% tail()

stacey <- full_shot_preds %>% filter(Player == 'Laura Stacey')
cameranesi <- full_shot_preds %>% filter(Player == 'Dani Cameranesi')
daoust <- full_shot_preds %>% filter(Player == 'Melodie Daoust')
wakefield <- full_shot_preds %>% filter(Player == 'Jennifer Wakefield')
brandt <- full_shot_preds %>% filter(Player == 'Hannah Brandt')
turnbull <- full_shot_preds %>% filter(Player == 'Blayre Turnbull')

shooting_hex <- full_shot_preds %>% group_by(X.Coordinate, Y.Coordinate) %>% 
  summarise(xG = mean(xG),
            shots = n())

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))


playerA <- ggplot(stacey, aes(X.Coordinate, Y.Coordinate)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(15), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL,
       title = 'Player A',
       subtitle = paste('Avg. xG:', round(mean(stacey$xG), 3)*100, "%"),
       caption =paste('Shots:', nrow(stacey),"| Goals:", sum(stacey$Goal_bin)))+
  coord_fixed(ratio = 1, expand = TRUE, clip = "on")+
  xlim(140,200)+
  ylim(0,85)

playerB <-ggplot(cameranesi, aes(X.Coordinate, Y.Coordinate)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(15), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL,
       title = 'Player B',
       subtitle = paste('Avg. xG:', round(mean(cameranesi$xG), 3)*100, "%"),
       caption =paste('Shots:', nrow(cameranesi),"| Goals:", sum(cameranesi$Goal_bin)))+
  coord_fixed(ratio = 1, expand = TRUE, clip = "on")+
  xlim(140,200)+
  ylim(0,85)

playerC <-ggplot(turnbull, aes(X.Coordinate, Y.Coordinate)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(15), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL,
       title = 'Player C',
       subtitle = paste('Avg. xG:', round(mean(turnbull$xG), 3)*100, "%"),
       caption =paste('Shots:', nrow(turnbull),"| Goals:", sum(turnbull$Goal_bin)))+
  coord_fixed(ratio = 1, expand = TRUE, clip = "on")+
  xlim(140,200)+
  ylim(0,85)


playerD <-ggplot(wakefield, aes(X.Coordinate, Y.Coordinate)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(15), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL,
       title = 'Player D',
       subtitle = paste('Avg. xG:', round(mean(wakefield$xG), 3)*100, "%"),
       caption =paste('Shots:', nrow(wakefield),"| Goals:", sum(wakefield$Goal_bin)))+
  coord_fixed(ratio = 1, expand = TRUE, clip = "on")+
  xlim(140,200)+
  ylim(0,85)

playerE <-ggplot(daoust, aes(X.Coordinate, Y.Coordinate)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(15), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL,
       title = 'Player E',
       subtitle = paste('Avg. xG:', round(mean(daoust$xG), 3)*100, "%"),
       caption =paste('Shots:', nrow(daoust), "| Goals:", sum(daoust$Goal_bin) ))+
  coord_fixed(ratio = 1, expand = TRUE, clip = "on")+
  xlim(140,200)+
  ylim(0,85)

playerF <-ggplot(brandt, aes(X.Coordinate, Y.Coordinate)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(15), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL,
       title = 'Player F',
       subtitle = paste('Avg. xG:', round(mean(brandt$xG), 3)*100, "%"),
       caption =paste('Shots:', nrow(brandt), "| Goals:", sum(brandt$Goal_bin) ))+
  coord_fixed(ratio = 1, expand = TRUE, clip = "on")+
  xlim(140,200)+
  ylim(0,85)

all_4 <- ggarrange(playerA, playerB, playerC, playerD,playerE,playerF + rremove("x.text"), 
          ncol = 3, nrow = 2)
ggsave("player_comps.png", all_4, device = 'png', dpi = 540)

AllShots <-ggplot(full_shot_preds, aes(X.Coordinate, Y.Coordinate)) + 
  stat_density2d(geom="tile", show.legend = F, aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=100) + 
  scale_alpha(range = c(0.5, 1.0)) + 
  scale_fill_gradientn(colours = jet.colors(15), trans="sqrt")+
  theme_minimal()+
  labs(x = NULL, y = NULL,
       title = 'All Shots',
       subtitle = paste('Avg. xG:', round(mean(full_shot_preds$xG), 3)*100, "%"),
       caption =paste('Shots:', nrow(full_shot_preds)))+
  coord_fixed(ratio = 1, expand = TRUE, clip = "on")+
  xlim(140,200)+
  ylim(0,85)


