library(dplyr)
library(readr)
library(ggplot2)
library(rcompanion)
library(cluster)
library(factoextra)
library(cluster)
library(fpc)
library(mclust)
library(corrplot)
library(reshape2)
library(fastDummies)
library(plotly)

#load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task1.RData")
load("/home/barbara/MDS/ATDS/DetectingTweetsSexism/variables/df_after_task1.RData")
names(df)

annotator_summary <- df %>%
  group_by(annotator_id, gender, age, country, ethnicity, education) %>%
  summarise(
    yes_rate = mean(label_task1_1 == "YES"),
    total_labeled = n()
  )

annotator_summary

boxplot(annotator_summary$total_labeled) # some labeled just 11 tweets, all the rest labeled 57

yes_rate_df = data.frame(yes_rate = annotator_summary$yes_rate)

################################## K-means ################################## 

# Elbow method
fviz_nbclust(yes_rate_df, kmeans, method = "wss") + 
  theme_minimal() + 
  ggtitle("Elbow Method")
# k = 5

# Silhouette method
fviz_nbclust(yes_rate_df, kmeans, method = "silhouette") + 
  theme_minimal() + 
  ggtitle("Silhouette Method")
# K = 2,5,6

# Chosen: K = 5

kmeans_result <- kmeans(yes_rate_df, centers = 5, nstart = 25)
annotator_summary$cluster <- factor(kmeans_result$cluster)

cluster_order <- annotator_summary %>%
  group_by(cluster) %>%
  summarize(max_rate = max(yes_rate)) %>%
  arrange(max_rate) %>%
  pull(cluster)

# re order clusters to allow for better visualization
annotator_summary$cluster <- factor(annotator_summary$cluster, levels = cluster_order)

ggplot(annotator_summary, aes(x = yes_rate, fill = cluster)) +
  geom_histogram(bins = 30) +
  labs(title = "Annotators Clustered by Labeling Behavior", x = "Proportion of Sexist Labels", fill = "Cluster")

aggregate(yes_rate ~ cluster, data = annotator_summary, summary)

############ Evaluation 

# Quality

res <- cluster.stats(dist(yes_rate_df), kmeans_result$cluster)

res$within.cluster.ss # 0.56. the lower the better (unbounded)

res$avg.silwidth
# silhouette score = mean = 0.56 -> good clustering, well-separated clusters with high cohesion
# Range: -1 to 1

# Calinski-Harabasz (variance ratio) - the higher the better (unbounded)
res$ch # 1243.016 

# Dunn (separation/compactness ratio) - the higher the better (ubounded)
res$dunn # 0.016

# tested k = 4 and the only metric that was better than k = 5 was dunn

# Stability

results <- replicate(10, kmeans(yes_rate_df, centers = 5, nstart = 25)$cluster)
results

ari_matrix <- matrix(NA, ncol = 10, nrow = 10)

for (i in 1:10) {
  for (j in 1:10) {
    ari_matrix[i, j] <- adjustedRandIndex(results[, i], results[, j])
  }
}

round(ari_matrix, 2)

ari_df <- melt(ari_matrix)
names(ari_df) <- c("Run1", "Run2", "ARI")


ggplot(ari_df, aes(x = Run1, y = Run2, fill = ARI)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "ARI Heatmap of Clustering Runs",
       x = "Clustering Run",
       y = "Clustering Run",
       fill = "ARI") +
  coord_fixed()

# The ARI ranges from -1 to 1, where 1 indicates a perfect match between the clustering result and "ground truth".
# Good stability


####################### Hierarchical clustering #############################


hc <- hclust(dist(yes_rate_df), method="ward.D")
plot(hc, main = "Dendrogram of Hierarchical Clustering")

sil_widths <- sapply(2:10, function(k) {
  clusters <- cutree(hc, k = k)
  mean(silhouette(clusters, dist(yes_rate_df))[, 3])
})
plot(2:10, sil_widths, type = "b", xlab = "Number of clusters", ylab = "Avg Silhouette Width")

abline(h = 5, col = "red", lty = 2) # 4 clusters
abline(h = 10, col = "green", lty = 2) # 2 clusters
clusters_4 <- cutree(hc, k = 4)
clusters_2 <- cutree(hc, k = 2)

res2 <- cluster.stats(dist(yes_rate_df), clusters_2)
res4 <- cluster.stats(dist(yes_rate_df), clusters_4)

# wss (compactness) - the lower the better
res2$within.cluster.ss
res4$within.cluster.ss
# 4 wins

# silhouette - the higher the better
res2$avg.silwidth
res4$avg.silwidth
# 2 wins

# Calinski-Harabasz (variance ratio) - the higher the better
res2$ch
res4$ch
# 4 wins

# Dunn (separation/compactness ratio) - the higher the better
res2$dunn
res4$dunn
# 4 wins

# Choosen: 4

# Stability

clusterboot_result <- clusterboot(
  dist(yes_rate_df), 
  clustermethod = disthclustCBI,
  method = "ward.D", 
  k = 4,
  B = 100
)

clusterboot_result$bootmean # mean Jaccard stability 
# 0.7500108 (moderately stable) 0.7485142 (moderately stable)  0.6632456  (borderline/moderately stable) 0.9015975 (highly stable)
# source: https://www.rdocumentation.org/packages/fpc/versions/2.2-13/topics/clusterboot on "Details"

# Visualization

clusters_4
annotator_summary$hc_cluster <- factor(clusters_4)

cluster_order_hc <- annotator_summary %>%
  group_by(hc_cluster) %>%
  summarize(max_rate = max(yes_rate)) %>%
  arrange(max_rate) %>%
  pull(hc_cluster)

annotator_summary$hc_cluster <- factor(annotator_summary$hc_cluster, levels = cluster_order_hc)

ggplot(annotator_summary, aes(x = yes_rate, fill = hc_cluster)) +
  geom_histogram(bins = 30) +
  labs(title = "Hierarchical Clustering: Annotators by Labeling Behavior", 
       x = "Proportion of Sexist Labels", fill = "Cluster")

aggregate(yes_rate ~ hc_cluster, data = annotator_summary, summary)


##################### Comparison hierarchical with 4 x kmeans with 5 ############################

# wss (compactness) - the lower the better
res$within.cluster.ss
res4$within.cluster.ss
# kmeans wins

# silhouette - the higher the better
res$avg.silwidth
res4$avg.silwidth
# kmeans wins

# Calinski-Harabasz (variance ratio) - the higher the better
res$ch
res4$ch
# kmeans wins

# Dunn (separation/compactness ratio) - the higher the better
res$dunn
res4$dunn
# hc wins

# Stability: kmeans wins

# Chosen: k-means


##################### Relation with other variables (kmeans) #########################

ggplot(annotator_summary, aes(x = cluster, fill = gender)) +
  geom_bar(position = "fill") +
  labs(title = "Gender Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()

ggplot(annotator_summary, aes(x = cluster, fill = age)) +
  geom_bar(position = "fill") +
  labs(title = "Age Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()

ggplot(annotator_summary, aes(x = cluster, fill = education)) +
  geom_bar(position = "fill") +
  labs(title = "Education Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()

ggplot(annotator_summary, aes(x = cluster, fill = ethnicity)) +
  geom_bar(position = "fill") +
  labs(title = "Ethnicity Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()  +
  scale_fill_brewer(palette = "Set1")

# Cant do by country because visualization gets hard (fill brewer does not have enough colors)
# ggplot(annotator_summary, aes(x = cluster, fill = country)) +
#   geom_bar(position = "fill") +
#   labs(title = "Country Composition by Cluster", y = "Proportion", x = "Cluster") +
#   theme_minimal() 




############ Using other variables ########################

df_clust <- annotator_summary[,2:7]

df_clust <- df_clust[,c('age','yes_rate')]

df_clust <- dummy_cols(df_clust, remove_selected_columns = TRUE)

str(df_clust)

df_clust <- scale(df_clust)

sum(is.na(df_clust))

# Elbow method
fviz_nbclust(df_clust, kmeans, method = "wss", k.max=15) + 
  theme_minimal() + 
  ggtitle("Elbow Method")

# Silhouette method
fviz_nbclust(df_clust, kmeans, method = "silhouette", k.max=15) + 
  theme_minimal() + 
  ggtitle("Silhouette Method")

k <- 3
kmeans_result <- kmeans(df_clust, centers = k, nstart = 25)

# evaluation and visualization

fviz_cluster(kmeans_result, data = df_clust, geom = "point", ellipse.type = "convex") + 
  theme_minimal() +
  ggtitle(paste("K-means Clustering with K = ", k))

res <- cluster.stats(dist(df_clust), kmeans_result$cluster)
res

res$ch
res$avg.silwidth
res$dunn
res$within.cluster.ss

pca <- prcomp(df_clust, scale. = TRUE)
pca_data <- as.data.frame(pca$x[, 1:3]) 
pca_data$cluster <- as.factor(kmeans_result$cluster)
summary(pca)



plot_ly(
  data = pca_data, 
  x = ~PC1, 
  y = ~PC2, 
  z = ~PC3, 
  color = ~cluster, 
  colors = "Set1", 
  type = "scatter3d", 
  mode = "markers"
) %>% layout(title = "3D PCA Clustering")




# columns for demographics: gender, age, ethnicity, education, country

# 'yes_rate'                                k = 5     ch = 1243.016     silhouette = 0.56         dunn = 0.016      wss = 0.56
# 'age','yes_rate'                          k = 3     ch = 533.7539     silhouette = 0.6613809    dunn = 0.5764527  wss = 339.014  visual = clean separation on 2D and 3D 
# 'age', 'yes_rate', 'gender'               k = 6     ch = 352.8954     silhouette = 0.6459437    dunn = 0.5805406  wss = 338.026  visual = clean separation on 3D  
# 'yes_rate', 'education'                   k = 6     ch = 421          silhouette = 0.6691562    dunn = 0.554855   wss = 339.0977  visual = clean on 3D
# 'age','yes_rate', 'education'             k = 8     ch = 134.7966     silhouette = 0.5443515    dunn = 0.2699416  wss = 919.1509
# 'age','yes_rate', 'education', 'gender'   k = 15(?) ch = 138.2857     silhouette = 0.5495702    dunn = 0.5065848  wss = 611.1114   
# 'age','yes_rate', 'country'               k = 2(?)  ch = 20.87045     silhouette = 0.1227976    dunn = 0.1125445  wss = 12108.62 
# 'country', 'yes_rate'                     no 'k' cant be found until k=15 by elbow method. using k = 12 chosen by silhouette -> bad results (ch = 16.31796, sil =  0.4780373, wss = 7689.906...)
# 'yes_rate', 'gender'                      k = 5     ch = 948.6159     silhouette = 0.549411     dunn = 0.04347826 wss = 86.29997 visual = points along the same line but separated, whole variance explained by 2pcs
# 'age','yes_rate', 'education', 'gender', 'ethnicity', 'country' k = 2(?) ch = 19  silhouette = 0.13 dunn = ... all bad. also with k = 15
# 'yes_rate', 'ethnicity'                   k = 4     ch = 112.9379     silhouette = 0.68606     dunn = 0.1771895 wss = 1398.542  visual = overlapping on 2D and 3D (51% expl var on 3D)
# 'yes_rate', 'ethnicity'                   k = 10 (elbow)   ch = 968.0862     silhouette = 0.5710186     dunn = 0.02631579 wss = 103.6693  visual = overlapping on 2D and 3D (51% expl var on 3D) (same)
# 'age','yes_rate', 'ethnicity'             k = 13    ch = 320.1255    silhouette = 0.5489933     dunn = 0.02180598 wss = 306.1638


# dunn and ch: good for braking the tie of silhouettes

# Visualizing 'age', 'yes_rate'
df_clust <- df_clust[,c('age', 'yes_rate')]
df_clust <- dummy_cols(df_clust, remove_selected_columns = TRUE)
df_clust <- scale(df_clust)
k <- 6
kmeans_result <- kmeans(df_clust, centers = k, nstart = 25)
annotator_summary$cluster_multiple <- factor(kmeans_result$cluster)

ggplot(annotator_summary, aes(x = cluster_multiple, fill = gender)) +
  geom_bar(position = "fill") +
  labs(title = "Age Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()

ggplot(annotator_summary, aes(x = yes_rate, fill = cluster_multiple)) +
  geom_histogram(bins = 30) +
  labs(title = "Annotators Clustered by Labeling Behavior", x = "Proportion of Sexist Labels", fill = "Cluster")

# -> useless bc maps age 3 categories directly to the 3 clusters. same happens to 'age', 'yes_rate' and 'gender' (each age perfectly mapped for 2 clusters)
# the same might happen with education since there is 6 cat and k = 6

# better just to use yes rate clustering alone

### FINAL: clustering by yes_rate, k = 5 using K-means