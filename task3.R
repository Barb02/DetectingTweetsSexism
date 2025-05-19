
# Task 3: Clustering Annotators Based on Labeling Patterns

# -------------------------------------------------------------------------------------------------------------------
# Libraries
# -------------------------------------------------------------------------------------------------------------------

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
library(tidyr)

# -------------------------------------------------------------------------------------------------------------------
# Initial Analysis
# -------------------------------------------------------------------------------------------------------------------

#load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task1.RData")
load("/home/barbara/MDS/ATDS/DetectingTweetsSexism/variables/df_after_task1.RData")
names(df)

annotator_summary <- df %>%
  distinct(annotator_id, .keep_all = TRUE) %>%
  select(gender, age, country, ethnicity, education)
# each row has a unique annotator

dim(annotator_summary)

sum(is.na(annotator_summary))

annotator_df <- dummy_cols(annotator_summary, remove_first_dummy = FALSE, remove_selected_columns = TRUE)

names(annotator_df)

# -------------------------------------------------------------------------------------------------------------------
# K-means
# -------------------------------------------------------------------------------------------------------------------

# Elbow method
fviz_nbclust(annotator_df, kmeans, method = "wss") + 
  theme_minimal() + 
  ggtitle("Elbow Method")
# k = 6

# Silhouette method
fviz_nbclust(annotator_df, kmeans, method = "silhouette") + 
  theme_minimal() + 
  ggtitle("Silhouette Method")
# K = 8, 10 or 4

# Chosen: K = 4, aligns better with our needs

kmeans_result <- kmeans(annotator_df, centers = 4, nstart = 25)

fviz_cluster(kmeans_result, data = annotator_df, geom = "point", ellipse.type = "convex") + 
  theme_minimal() +
  ggtitle(paste("K-means Clustering with K = 4"))
#save(kmeans_result, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/kmeans_model.RData")

pca <- prcomp(annotator_df, scale. = TRUE)
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

annotator_df$cluster <- kmeans_result$cluster

# -------------------------------------------------------------------------------------------------------------------
# Evaluation 
# -------------------------------------------------------------------------------------------------------------------

# Quality

res <- cluster.stats(dist(annotator_df), kmeans_result$cluster)

res$within.cluster.ss # 1.420638 the lower the better (unbounded)

res$avg.silwidth
# silhouette score = mean = 0.5423015 -> good clustering, well-separated clusters with high cohesion
# Range: -1 to 1

# Calinski-Harabasz (variance ratio) - the higher the better (unbounded)
res$ch # 847.3658 

# Dunn (separation/compactness ratio) - the higher the better (ubounded)
res$dunn # 0.016

# with k = 2 only silhouette is slightly better, therefore k = 3 is better overall

# Stability

results <- replicate(50, kmeans(annotator_df, centers = 4, nstart = 1)$cluster)
results

ari_matrix <- matrix(NA, ncol = 50, nrow = 50)

for (i in 1:50) {
  for (j in 1:50) {
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
# Medium/Good stability

# -------------------------------------------------------------------------------------------------------------------
# Hierarchical clustering
# -------------------------------------------------------------------------------------------------------------------

hc <- hclust(dist(annotator_df), method="ward.D2")
plot(hc, main = "Dendrogram of Hierarchical Clustering")

sil_widths <- sapply(2:10, function(k) {
  clusters <- cutree(hc, k = k)
  mean(silhouette(clusters, dist(annotator_df))[, 3])
})
plot(2:10, sil_widths, type = "b", xlab = "Number of clusters", ylab = "Avg Silhouette Width")

plot(hc, main = "Dendrogram of Hierarchical Clustering")
# 6 or 8 is too big for our problem
# Choosen: 4

clusters_2 <- cutree(hc, k = 4)

res_hc <- cluster.stats(dist(annotator_df), clusters_2)
res_hc$within.cluster.ss
res_hc$avg.silwidth
res_hc$ch  
res_hc$dunn

# Stability

clusterboot_result <- clusterboot(
  dist(annotator_df), 
  clustermethod = disthclustCBI,
  method = "ward.D2", 
  k = 4,
  B = 100
)

clusterboot_result$bootmean # mean Jaccard stability 
# 0.7408048 0.8204264 0.7944817 0.6938201
# 0.6815410 (borderline/moderately stable) 0.7446272 (moderately stable)
# source: https://www.rdocumentation.org/packages/fpc/versions/2.2-13/topics/clusterboot on "Details"

# Visualization

annotator_summary$hc_cluster <- factor(clusters_2)

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

# -------------------------------------------------------------------------------------------------------------------
# Comparison hierarchical with 2 x kmeans with 3
# -------------------------------------------------------------------------------------------------------------------

# wss (compactness) - the lower the better
res$within.cluster.ss
res_hc$within.cluster.ss
# kmeans wins

# silhouette - the higher the better
res$avg.silwidth
res_hc$avg.silwidth
# kmeans wins

# Calinski-Harabasz (variance ratio) - the higher the better
res$ch
res_hc$ch
# kmeans wins

# Dunn (separation/compactness ratio) - the higher the better
res$dunn
res_hc$dunn
# kmeans wins

# Stability: kmeans wins

# Chosen: k-means

# -------------------------------------------------------------------------------------------------------------------
# Relation with other variables (kmeans)
# -------------------------------------------------------------------------------------------------------------------

# Gender

ggplot(annotator_summary, aes(x = cluster, fill = gender)) +
  geom_bar(position = "fill") +
  labs(title = "Gender Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()

ggplot(annotator_summary, aes(x = gender, fill = cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Gender Composition by Cluster", y = "Proportion", x = "Gender") +
  theme_minimal()

# Proportions of category by cluster

subset <- annotator_summary[, c("gender", "cluster")]
cluster_prop <- subset %>%
  count(cluster, gender) %>%
  group_by(gender) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()
prop_table <- cluster_prop %>%
  select(cluster, gender, proportion) %>%
  pivot_wider(
    names_from = cluster,
    values_from = proportion,
    values_fill = 0
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  select(gender, "2", "1", "3") 
print(prop_table)

# Age

ggplot(annotator_summary, aes(x = cluster, fill = age)) +
  geom_bar(position = "fill") +
  labs(title = "Age Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()

ggplot(annotator_summary, aes(x = age, fill = cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Cluster Composition by Age", y = "Proportion", x = "Age") +
  theme_minimal()

subset <- annotator_summary[, c("age", "cluster")]
cluster_prop <- subset %>%
  count(cluster, age) %>%
  group_by(age) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()
prop_table <- cluster_prop %>%
  select(cluster, age, proportion) %>%
  pivot_wider(
    names_from = cluster,
    values_from = proportion,
    values_fill = 0
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  select(age, "2", "1", "3") 
print(prop_table)

# Education

ggplot(annotator_summary, aes(x = cluster, fill = education)) +
  geom_bar(position = "fill") +
  labs(title = "Education Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()

ggplot(annotator_summary, aes(x = education, fill = cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Cluster Composition by Education", y = "Proportion", x = "Education") +
  theme_minimal()

subset <- annotator_summary[, c("education", "cluster")]
cluster_prop <- subset %>%
  count(cluster, education) %>%
  group_by(education) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()
prop_table <- cluster_prop %>%
  select(cluster, education, proportion) %>%
  pivot_wider(
    names_from = cluster,
    values_from = proportion,
    values_fill = 0
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  select(education, "2", "1", "3") 
print(prop_table)

# Ethnicity

ggplot(annotator_summary, aes(x = cluster, fill = ethnicity)) +
  geom_bar(position = "fill") +
  labs(title = "Ethnicity Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()  +
  scale_fill_brewer(palette = "Set1")

ggplot(annotator_summary, aes(x = ethnicity, fill = cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Cluster Composition by Ethnicity", y = "Proportion", x = "Ethnicity") +
  theme_minimal() 

subset <- annotator_summary[, c("ethnicity", "cluster")]
cluster_prop <- subset %>%
  count(cluster, ethnicity) %>%
  group_by(ethnicity) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()
prop_table <- cluster_prop %>%
  select(cluster, ethnicity, proportion) %>%
  pivot_wider(
    names_from = cluster,
    values_from = proportion,
    values_fill = 0
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  select(ethnicity, "2", "1", "3") 
print(prop_table)

# Country 

#  visualization gets hard (too much countries)
ggplot(annotator_summary, aes(x = cluster, fill = country)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Country Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()

ggplot(annotator_summary, aes(x = country, fill = cluster)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Cluster Composition by Country", y = "Proportion", x = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

subset <- annotator_summary[, c("country", "cluster")]

country_cluster_prop <- subset %>%
  count(cluster, country) %>%
  group_by(country) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

prop_table <- country_cluster_prop %>%
  select(cluster, country, proportion) %>%
  pivot_wider(
    names_from = cluster,
    values_from = proportion,
    values_fill = 0
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  select(country, "2", "1", "3") 

print(prop_table, n = 33)


# -------------------------------------------------------------------------------------------------------------------
# Using other variables
# -------------------------------------------------------------------------------------------------------------------

df_clust <- annotator_summary[,2:7]

df_clust <- df_clust[,c('age','yes_rate')]

df_clust <- dummy_cols(df_clust, remove_selected_columns = TRUE)

names(df_clust)

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

# Visualizing 'age', 'yes_rate'
df_clust <- df_clust[,c('age_18-22','age_23-45','age_46+','yes_rate')]
df_clust <- scale(df_clust)
k <- 3
kmeans_result <- kmeans(df_clust, centers = k, nstart = 25)
annotator_summary$cluster_multiple <- factor(kmeans_result$cluster)

ggplot(annotator_summary, aes(x = cluster_multiple, fill = age)) +
  geom_bar(position = "fill") +
  labs(title = "Age Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()

ggplot(annotator_summary, aes(x = yes_rate, fill = cluster_multiple)) +
  geom_histogram(bins = 30) +
  labs(title = "Annotators Clustered by Labeling Behavior", x = "Proportion of Sexist Labels", fill = "Cluster")

# -> useless bc maps age 3 categories directly to the 3 clusters. same happens to 'age', 'yes_rate' and 'gender' (each age perfectly mapped for 2 clusters)
# the same might happen with education since there is 6 cat and k = 6

# better just to use yes rate clustering alone

### FINAL: clustering by yes_rate, k = 3 using K-means