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

df = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df = read_csv("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/tables/EXIST2025_train.csv")


annotator_summary <- df %>%
  group_by(annotator_id, gender, age, country, ethnicity, education) %>%
  summarise(
    yes_rate = mean(label_task1_1 == "YES"),
    total_labeled = n()
  )

annotator_summary

boxplot(annotator_summary$total_labeled) # just oen that labeled just 11 tweets, all the rest labeled 57

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


# TODO (?): cluster a partir do yes rate + demografia?