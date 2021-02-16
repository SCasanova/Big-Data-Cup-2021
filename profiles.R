library(fmsb)
library(viridis)
library(colormap)
library(patchwork)
library(hrbrthemes)

c_plus_cg <- left_join(cg_score, c_score, by = c('Player'))
scores_plus_xga <- merge(cpoe, c_plus_cg, by = c('Player'))
scores_plus_def <- merge(scores_plus_xga, taken_goals_score, by = c('Player'))
scores_plus_def <- merge(scores_plus_def, total_xg, by = c('Player'))


scores_plus_def <- within(scores_plus_def, pct_c <- perc.rank(avg_c_score))
scores_plus_def <- within(scores_plus_def, pct_cg <- perc.rank(avg_cg_score))
scores_plus_def <- within(scores_plus_def, pct_cpoe <- perc.rank(cpoe))
scores_plus_def <- within(scores_plus_def, pct_take_def <- perc.rank(avg_prob_taken))
scores_plus_def <- within(scores_plus_def, pct_goX <- perc.rank(goals_oX))



test_prof <- scores_plus_def %>% arrange(desc(pct_cpoe)) %>%   head(6)
mytitle <- test_prof$Player
test_prof <- test_prof %>% select(pct_cg, pct_c, pct_cpoe, pct_take_def,pct_goX)
#test_prof <- test_prof %>% select(avg_cg_score, avg_c_score, cpoe, taken_goals, xg,goals_oX)



mins <- c(rep(0,ncol(test_prof)))
maxs <- c(rep(1,ncol(test_prof)))

#mins <- c(min(scores_plus_def$avg_cg_score),
#          min(scores_plus_def$avg_c_score),
#          min(scores_plus_def$cpoe),
#          min(scores_plus_def$taken_goals),
#          min(scores_plus_def$xg),
#          min(scores_plus_def$goals_oX))

#maxs <- c(max(scores_plus_def$avg_cg_score),
#          max(scores_plus_def$avg_c_score),
#          max(scores_plus_def$cpoe),
#          max(scores_plus_def$taken_goals),
#          max(scores_plus_def$xg),
#          max(scores_plus_def$goals_oX))

test_prof <-rbind(maxs , mins , test_prof)
colnames(test_prof) <- c('CG Score', 'C Score', 'CPOE', 'xGA Saved', 'Goals over X')

# Prepare color
colors_border=colormap(colormap=colormaps$viridis, nshades=6, alpha=1)
colors_in=colormap(colormap=colormaps$viridis, nshades=6, alpha=0.3)

# Split the screen in 6 parts
par(mar=rep(0.8,4))
par(mfrow=c(2,3))

# Loop for each plot
for(i in 1:6){

  # Custom the radarChart !
  radarchart( test_prof[c(1,2,i+2),], axistype=1, 
  
    #custom polygon
    pcol=colors_border[i] , pfcol=colors_in[i] , plwd=4, plty=1 , 
  
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.8,
  
    #custom labels
    vlcex=0.8,
    
    #title
    title=mytitle[i]
    )
}



