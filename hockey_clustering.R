prospect_metrics <- read.csv("~/Library/Containers/com.apple.mail/Data/Library/Mail Downloads/C2F5F5C0-3DB3-4C70-9E34-CF7C9B1B24CC/pospect_metrics.csv")

library(stats)
library(mclust)
library(mdthemes)
library(gghighlight)
library(factoextra)
library(extrafont)
# font_import(prompt=FALSE)
loadfonts() 
library(ggsci) 
library(broom)
library(igraph)
library(tidyverse)

prospect_metrics <- prospect_metrics %>%
  arrange(desc(goals))

select_metrics <- prospect_metrics %>%
  select(-Player) %>%
  scale()

set.seed(222) # set seed to ensure reproduceability b/c k-means relies on random states for initialization 
MAX_K <- 20 # max number of clusters
sse <- c() # vector to hold SSE of each model

for (k in 1:MAX_K) {
  algo_k <- kmeans(select_metrics, centers=k, nstart=22, iter.max=20) # k-means algorithm
  sse <- c(sse, algo_k$tot.withinss) # get SSE
} 

tibble(k = 1:MAX_K, SSE_difference = sse-2*lead(sse)+lead(sse, 2)) %>%
  dplyr::filter(k<MAX_K-1) %>%
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  labs(x = "K", y = "SSE Rolling 2-Unit Difference", title = "Where Does It Level Off?") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

set.seed(22)
# re-run K-Means with 12 clusters
K <- 12
kmeans12 <- kmeans(select_metrics, centers=K, nstart=22, iter.max=20)
km_centers <- as.data.frame(kmeans12$centers) # SCALED cluster centers/means

km_centers$Cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                        'Cluster 4', 'Cluster 5', 'Cluster 6',
                        'Cluster 7', 'Cluster 8', 'Cluster 9', 'Cluster 10',
                        'Cluster 11', 'Cluster 12') 

km_centers <- km_centers %>%
  rename(c('COM'='avg_c', 'APP'='avg_pass_prob', # give predictors a shorter name for plotting
           'CPOE'='cpoe', 'PA'='passes',
           'CGS'='chance_goal_score', 'G'='goals',
           'S'='shots', 'ACGS'='avg_cg_score', 'CS'='chance_score',
           'ACS'='avg_c_score', 'TG'='taken_goals', 'TA'='takeaways', 
           'APT'='avg_prob_taken', 'GOX'='goals_oX')) %>% 
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val')

km_centers$feature <- factor(km_centers$feature, levels=c('COM', 'APP', 'CPOE',
                                                          'PA', 'CGS', 'G', 'S',
                                                          'ACGS', 'CS', 'ACS',
                                                          'TG', 'TA', 'APT','GOX')) 

km_centers$Cluster <- factor(km_centers$Cluster, 
                             levels=c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4',
                                      'Cluster 5', 'Cluster 6', 'Cluster 7', 'Cluster 8',
                                      'Cluster 9', 'Cluster 10', 'Cluster 11', 'Cluster 12'))

cluster_players <- tibble(cluster=kmeans12$cluster, Player=prospect_metrics$Player)

final_clusters <- cluster_players %>%
  inner_join(prospect_metrics)

final_clusters <- final_clusters %>%
  left_join(prospect_metrics, by = "Player")

km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=Cluster)) + 
  geom_point() + # plot points
  scale_color_brewer(palette="Paired") + # color points
  gghighlight(use_direct_label = FALSE) + # highlight each cluster
  facet_wrap(~ Cluster, ncol=3) + # create seperate plots for each cluster
  labs(x = "Predictor", y = "Cluster Center", 
       title = "Visualizing K-Means Cluster Makeups") + 
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())

pca <- prcomp(select_metrics) # perform Principle Component Analysis 
pca_summary <- summary(pca) # summary of PCA model

pc2 <- as.data.frame(pca$x[,1:2]) # extract first two PCs
pc2$Cluster <- as.factor(kmeans12$cluster) # add player clusters 
cluster1_var <- round(pca_summary$importance[2,1], 4) * 100 # get variance explained by cluster 1
cluster2_var <- round(pca_summary$importance[2,2], 4) * 100 # get variance explained by cluster 2

# how different are the clusters when scaled down to two dimensions? 
pc2 %>% 
  ggplot(aes(x=PC1, y=PC2, color=Cluster, shape=Cluster)) + 
  geom_point(alpha=0.3) + 
  scale_color_futurama() + # fun color scale
  geom_rug() + # great way to visualize points on a single axis
  theme_minimal() + stat_ellipse(level=(2/3)) + # set ellipse value to one standard deviation
  scale_shape_manual(values=seq(0,15)) + 
  labs(x = paste0('PC1 (Accounts for ', cluster1_var, '% of Variance)'), # define cluster 1 % of variance
       y = paste0('PC2 (Accounts for ', cluster2_var, '% of Variance)'), # define cluster 2 % of variance
       title = 'Visualizing K-Means Cluster Differences in 2D') + 
  theme_bw()

set.seed(22)
mixture_model <-  Mclust(select_metrics)
mix_model_summary <- summary(mixture_model) # obtain model summary
mix_model_summary

mbc_centers <- mixture_model$parameters$mean %>%
  t() %>%
  as.data.frame()

mbc_centers$Cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                         'Cluster 4', 'Cluster 5') 

mbc_centers <- mbc_centers %>%
  rename(c('COM'='avg_c', 'APP'='avg_pass_prob', # give predictors a shorter name for plotting
           'CPOE'='cpoe', 'PA'='passes',
           'CGS'='chance_goal_score', 'G'='goals',
           'S'='shots', 'ACGS'='avg_cg_score', 'CS'='chance_score',
           'ACS'='avg_c_score', 'TG'='taken_goals', 'TA'='takeaways', 
           'APT'='avg_prob_taken', 'GOX'='goals_oX')) %>% 
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val')

mbc_centers$feature <- factor(mbc_centers$feature, levels=c('COM', 'APP', 'CPOE',
                                                            'PA', 'CGS', 'G', 'S',
                                                            'ACGS', 'CS', 'ACS',
                                                            'TG', 'TA', 'APT','GOX'))

mix_model_preds <- as.data.frame(predict(mixture_model, select_metrics)) # provides class probabilities
mix_model_augment <- augment(mixture_model, select_metrics) # .class gives MAP class label 
mix_model_augment$Player <- prospect_metrics$Player # add player ID

mbc_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=Cluster)) + 
  geom_point() + geom_line() + 
  scale_color_manual(values = pal_npg()(7)) + # call 8-color palette
  gghighlight(use_direct_label = FALSE) + # highlight each cluster
  facet_wrap(~ Cluster) + # plot each cluster seperately
  labs(x = "Feature", y = "Scaled Group Score", 
       title = "Visualizing Model Based Cluster Makeups") + 
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # make axis labels less cramped 
        panel.grid.minor = element_blank()) 

n_dist <- function(a, b) {
  return (sqrt(sum((a - b)^2)))
}

mma <- mix_model_augment %>%
  dplyr::select(Player, .class, .uncertainty) %>%
  cbind(dplyr::select(mix_model_preds, -classification)) %>%
  mutate(player_id = row_number())

nodes <- data_frame(nodes = mma$player_id, 
                    group = mma$.class,
                    name = mma$Player)

edges <- data_frame()
for (i in 1:nrow(nodes)) {
  for (j in 1:nrow(nodes)) {
    if (i > j) { # no need to perform distance calculations twice b/c the graph is undirected 
      edges <- rbind(edges, data_frame(from=as.character(nodes[i,'nodes']), # first player id
                                       to=as.character(nodes[j,'nodes']), # second player id
                                       weight=n_dist(mma[i, 3:8], mma[j, 3:8]))) # distance calculation
    }
  }
}

g <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE) # create graph using edges and nodes
mst_g <- mst(g) # reduce graph to an MST
set.seed(30) # ensures reproducability 30, 42
# obtain a layout for the graph (there are many other graphical layouts out there worth exploring further)
g_layout <- layout_with_lgl(mst_g) 

plot(mst_g, layout=g_layout, main="Scouting Hockey Data Layout")

player_vec <- c('Chad Yetman', 'Luke Evangelista', 'Richard Whittaker', 'Egor Afanasyev', 
                'Tyson Foerster', 'Brendan Sellan', 'Alex Gritz', 'Owen Gilhula',
                'Eric Uba', 'Cole Purboo', 'Michael Bianconi', 'Cole Schwindt',
                'Liam Foudy', 'Alec Regula', 'Dylan Robinson', 'Tyler Tucker',
                'Ole Holm', 'Adrien Beraldo', 'Noah Sedore', 'Will Cuylle', 'Jean-Luc Foudy',
                'Keean Washkurak', 'Jake Uberti', 'Vladislav Kolyachonok', 'Jack Duff',
                'Barret Kirwin', 'Connor McMichael', 'Ty Collins', 'Pavel Gogolev')

name_vec <- tibble(p_name=vertex_attr(g, "name")) %>%
  dplyr::mutate(p_name = ifelse(p_name %in% player_vec, p_name, NA), # display player name
                b_color = ifelse(p_name%in% player_vec, adjustcolor('yellow', alpha=1), adjustcolor('black', alpha=0)),
                v_deg = case_when( # identify player names moving vertically
                  p_name%in%c('Chad Yetman', 'Luke Evangelista', 'Eric Uba',
                              'Cole Purboo', 'Michael Bianconi', 'Tyson Foerster')~pi/2,
                  TRUE~0))

plot(mst_g, layout=g_layout, main="Hockey Scouting Cluster Network",
     vertex.size=5, vertex.label=name_vec$p_name, # vertex_attr(g, "name"), # include certain player names
     frame=TRUE, edge.width=10, edge.color=adjustcolor('gray', alpha=0.7),
     vertex.frame.color = name_vec$b_color, # set node frame color
     vertex.label.color="#232D4B", # text color
     vertex.label.font=2, # make player names bold
     vertex.label.cex=.5, # set name sizes 
     vertex.color=pal_npg()(7)[as.numeric(as.factor(vertex_attr(g, "group")))])
legend(x=1.2, y=1.1, c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4',
                       'Cluster 5'), pch=21,
       pt.bg=pal_npg()(7), pt.cex=1.5, bty="n", ncol=1, cex=.8)




