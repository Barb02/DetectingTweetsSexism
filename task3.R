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

load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task1.RData")
#load("/home/barbara/MDS/ATDS/DetectingTweetsSexism/variables/df_after_task1.RData")
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
#save(annotator_summary, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/annotatorsummary.RData")

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


# -------------------------------------------------------------------------------------------------------------------
# Evaluation 
# -------------------------------------------------------------------------------------------------------------------

# Quality

res <- cluster.stats(dist(annotator_df), kmeans_result$cluster)

# wss - the lower the better (unbounded)
res$within.cluster.ss 

res$avg.silwidth
# silhouette score mean 
# Range: -1 to 1

# Calinski-Harabasz (variance ratio) - the higher the better (unbounded)
res$ch

# Dunn (separation/compactness ratio) - the higher the better (ubounded)
res$dunn 


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
# 5, 7 or 8 is too big for our problem
# Choosen: 5

clusters_2 <- cutree(hc, k = 5)

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
  k = 5,
  B = 100
)

clusterboot_result$bootmean # mean Jaccard stability 
# 0.7408048 (moderately stable)  0.8204264 (stable) 0.7944817 (stable) 0.6938201 (borderline/moderately stable)
# source: https://www.rdocumentation.org/packages/fpc/versions/2.2-13/topics/clusterboot on "Details"

# Visualization

fviz_cluster(list(data = annotator_df, cluster = clusters_2),
             geom = "point",
             ellipse.type = "convex") +
  theme_minimal() +
  ggtitle("Hierarchical Clustering with K = 5")

# -------------------------------------------------------------------------------------------------------------------
# Comparison hierarchical with 5 x kmeans with 4
# -------------------------------------------------------------------------------------------------------------------

# wss (compactness) - the lower the better
res$within.cluster.ss
res_hc$within.cluster.ss
# hc wins

# silhouette - the higher the better
res$avg.silwidth
res_hc$avg.silwidth
# hc wins

# Calinski-Harabasz (variance ratio) - the higher the better
res$ch
res_hc$ch
# kmeans wins

# Dunn (separation/compactness ratio) - the higher the better
res$dunn
res_hc$dunn
# tie

# Stability: hc wins

# Chosen: Although hc might seem better, it is easier to use kmeans for computting new centroids, while hc is not built for 
# this, so we choose k-means.

# -------------------------------------------------------------------------------------------------------------------
# Relation with other variables (kmeans)
# -------------------------------------------------------------------------------------------------------------------

# Gender

annotator_df$cluster = kmeans_result$cluster

df_plot <- annotator_df

reverse_ohe <- function(df, prefix) {
  cols <- grep(paste0("^", prefix), names(df), value = TRUE)
  values <- gsub(paste0("^", prefix), "", cols)
  
  values <- sub("^_", "", values)
  
  # Get index of the 1s in each row
  idx <- apply(df[, cols], 1, function(row) {
    match(1, row)
  })
  
  # Create the new column
  factor_col <- ifelse(is.na(idx), NA, values[idx])
  
  return(factor(factor_col, levels = values))
}

df_plot$cluster <- factor(df_plot$cluster)

df_plot$country <- reverse_ohe(df_plot, "country")
df_plot$gender <- reverse_ohe(df_plot, "gender")
df_plot$ethnicity <- reverse_ohe(df_plot, "ethnicity")
df_plot$age <- reverse_ohe(df_plot, "age")
df_plot$education <- reverse_ohe(df_plot, "education")

ggplot(df_plot, aes(x = cluster, fill = gender)) +
  geom_bar(position = "fill") +
  labs(title = "Gender Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()

ggplot(df_plot, aes(x = gender, fill = cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Gender Composition by Cluster", y = "Proportion", x = "Gender") +
  theme_minimal()


# Age

ggplot(df_plot, aes(x = cluster, fill = age)) +
  geom_bar(position = "fill") +
  labs(title = "Age Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()

ggplot(df_plot, aes(x = age, fill = cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Cluster Composition by Age", y = "Proportion", x = "Age") +
  theme_minimal()


# Education

ggplot(df_plot, aes(x = cluster, fill = education)) +
  geom_bar(position = "fill") +
  labs(title = "Education Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()

ggplot(df_plot, aes(x = education, fill = cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Cluster Composition by Education", y = "Proportion", x = "Education") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Ethnicity

ggplot(df_plot, aes(x = cluster, fill = ethnicity)) +
  geom_bar(position = "fill") +
  labs(title = "Ethnicity Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()  +
  scale_fill_brewer(palette = "Set1")

ggplot(df_plot, aes(x = ethnicity, fill = cluster)) +
  geom_bar(position = "fill") +
  labs(title = "Cluster Composition by Ethnicity", y = "Proportion", x = "Ethnicity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Country 

#  visualization gets hard (too much countries)
ggplot(df_plot, aes(x = cluster, fill = country)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Country Composition by Cluster", y = "Proportion", x = "Cluster") +
  theme_minimal()

ggplot(df_plot, aes(x = country, fill = cluster)) +
  geom_bar(position = "fill", color = "black") +
  labs(title = "Cluster Composition by Country", y = "Proportion", x = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
