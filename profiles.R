library(fmsb)
library(viridis)
library(colormap)
library(patchwork)
library(hrbrthemes)

c_plus_cg <- left_join(cg_score, c_score, by = c('Player'))
scores_plus_xga <- merge(xga_score, c_plus_cg, by = c('Player'))

test_prof <- scores_plus_xga  %>% arrange(desc(xGA)) %>%  head(6)
mytitle <- test_prof$Player
test_prof <- test_prof %>% select(avg_cg_score, avg_c_score, xGA)

mins <- cbind(avg_cg_score= min(scores_plus_xga$avg_cg_score), avg_c_score=min(scores_plus_xga$avg_c_score), xGA=min(scores_plus_xga$xGA))
maxs <- cbind(avg_cg_score=max(scores_plus_xga$avg_cg_score), avg_c_score=max(scores_plus_xga$avg_c_score), xGA=max(scores_plus_xga$xGA))

test_prof <-rbind(maxs , mins , test_prof)

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
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
  
    #custom labels
    vlcex=0.8,
    
    #title
    title=mytitle[i]
    )
}
