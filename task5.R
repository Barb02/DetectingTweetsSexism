
# Task 5: Predicting Annotator Decisions (Recommendation System Approach)

# -------------------------------------------------------------------------------------------------------------------
# Libraries
# -------------------------------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(recommenderlab)
library(reshape2)
library(tidyr)
library(combinat)

# -------------------------------------------------------------------------------------------------------------------
# Load the training data set
# -------------------------------------------------------------------------------------------------------------------

#df_train = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST2025_train.csv")
df_train = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df_train = read_csv("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/tables/EXIST2025_train.csv")

# -------------------------------------------------------------------------------------------------------------------
# Prepare recommendation matrix
# -------------------------------------------------------------------------------------------------------------------

df_reco <- df_train %>%
  select(annotator_id, tweet, label_task1_1) %>%
  mutate(label_task1_1 = ifelse(label_task1_1 == "YES", 2,
                                ifelse(label_task1_1 == "NO", 1, NA)))

rating_matrix <- dcast(df_reco, annotator_id ~ tweet, value.var = "label_task1_1")
mat <- as.matrix(rating_matrix[,-1])
rownames(mat) <- rating_matrix$annotator_id
rrm <- as(mat, "realRatingMatrix")
image(rrm)

# -- Note --
# Real Rating Matrix because there can be missing values and that doesnt mean that the person classified the tweet as not sexist

# -------------------------------------------------------------------------------------------------------------------
# How many annotators have rated X tweets (used to choose the parameter given in the evaluationScheme)
# -------------------------------------------------------------------------------------------------------------------

tweets_por_annotator <- df_train %>%
  group_by(annotator_id) %>%
  summarise(tweets_avaliados = sum(!is.na(label_task1_1)))

tweets_por_annotator_freq <- tweets_por_annotator %>%
  count(tweets_avaliados, name = "num_annotators") %>%
  arrange(tweets_avaliados)

print(tweets_por_annotator_freq)

# -- Conclusion --
# One annotator either rated 11 or 57 tweets 

# -------------------------------------------------------------------------------------------------------------------
# How many annotators have rated each tweet
# -------------------------------------------------------------------------------------------------------------------

annotators_por_tweet <- df_train %>%
  group_by(tweet) %>%
  summarise(num_annotators = sum(!is.na(label_task1_1)))

annotators_por_tweet_freq <- annotators_por_tweet %>%
  count(num_annotators, name = "num_tweets") %>%
  arrange(num_annotators)

print(annotators_por_tweet_freq)

# -- Conclusion --
# Every single tweet was rated by 6 annotators 

# -------------------------------------------------------------------------------------------------------------------
# Cross Validation Evaluation Scheme
# -------------------------------------------------------------------------------------------------------------------

set.seed(2025)
e_split <- evaluationScheme(rrm, method = "cross-validation", given = 8, goodRating = 2)

# -- Note --
# given = 8 ensures each user has at least 8 items to rate for training, providing enough data for the model
# good Rating = 1 because ratings of "1" (sexist tweets) are more relevant

# -------------------------------------------------------------------------------------------------------------------
# Function to evaluate a method
# -------------------------------------------------------------------------------------------------------------------

evaluate_model <- function(e_scheme, method_name, params = NULL) {
  cat("\n---", method_name, "---\n")
  
  model <- Recommender(getData(e_scheme, "train"), method = method_name, parameter = params)
  
  getModel(model)
  
  res <- evaluate(e_scheme, method = method_name, type = "ratings", parameter = params)
  print(avg(res))
  
  plot(res, annotate=TRUE)
  
  return(model) 
}

# -- Note --
# type = ratings -> enables the algorithms to compute predicted scores and evaluate them via regression-like metrics
# 306 annotators entram no treino (train) 
# Os restantes 42 annotators são usados para teste, divididos entre:
# known: 346 ratings (os 8 dados para o modelo prever a partir deles)
# unknown: 2002 ratings (o que o modelo tem de prever)

# -------------------------------------------------------------------------------------------------------------------
# Evaluate methods
# -------------------------------------------------------------------------------------------------------------------

# Baseline 
model_random <- evaluate_model(e_split, "RANDOM") 

# Models 
model_popular <- evaluate_model(e_split, "POPULAR")
#model_ubcf_2 <- evaluate_model(e_split, "UBCF", params = list(method = "cosine", nn = 2))
model_ubcf_5 <- evaluate_model(e_split, "UBCF", params = list(method = "cosine", nn = 5))
model_ubcf_10 <- evaluate_model(e_split, "UBCF", params = list(method = "cosine", nn = 10))
#model_ibcf_5 <- evaluate_model(e_split, "IBCF", params = list(method = "cosine", k = 5))
#model_ibcf_10 <- evaluate_model(e_split, "IBCF", params = list(method = "cosine", k = 10))
#model_ibcf_20 <- evaluate_model(e_split, "IBCF", params = list(method = "cosine", k = 20))
#model_ibcf_25 <- evaluate_model(e_split, "IBCF", params = list(method = "cosine", k = 25))

# -------------------------------------------------------------------------------------------------------------------
# Checking things
# -------------------------------------------------------------------------------------------------------------------

for (i in 1:nrow(pred_mat)) {
  
  # Obter as 10 maiores predições para o annotator i
  top_preds <- sort(pred_mat[i, ], decreasing = TRUE)[1:10]
  
  # Verificar se algum valor não é NA e imprimir
  if (any(!is.na(top_preds))) {
    print(paste("Annotator", rownames(pred_mat)[i], ":"))
    print(top_preds[!is.na(top_preds)])
  }
}

# Independentemente do modelo que ponha nunca dá recomendações

# -------------------------------------------------------------------------------------------------------------------
# Checking things - Dividir os dados em 80% para treino e 20% para teste e ver se assim dá para fazer predict e dar algo
# -------------------------------------------------------------------------------------------------------------------

set.seed(123) 
train_index <- sample(1:nrow(rrm), size = 0.8 * nrow(rrm))

rrm_offline <- rrm[train_index, ]  
rrm_online <- rrm[-train_index, ] 

modelUBCF_R <- Recommender(rrm_offline, method = "UBCF", parameter = list(nn = 2))

recsUBCF_R <- predict(modelUBCF_R, rrm_online, type = "ratings")

recs_list <- getList(recsUBCF_R)

print(recs_list)

# NOP

# -------------------------------------------------------------------------------------------------------------------
# Max tweets two annotators have in common
# -------------------------------------------------------------------------------------------------------------------

tweets_por_annotator <- df_train %>%
  group_by(annotator_id) %>%
  summarise(tweets_classificados = list(unique(tweet)))

annotator_pairs <- combn(unique(tweets_por_annotator$annotator_id), 2, simplify = FALSE)

count_common_tweets <- function(pair) {
  annotator_1 <- pair[1]
  annotator_2 <- pair[2]
  
  tweets_1 <- tweets_por_annotator$tweets_classificados[tweets_por_annotator$annotator_id == annotator_1][[1]]
  tweets_2 <- tweets_por_annotator$tweets_classificados[tweets_por_annotator$annotator_id == annotator_2][[1]]
  
  common_tweets <- length(intersect(tweets_1, tweets_2))
  return(common_tweets)
}

common_tweets_count <- sapply(annotator_pairs, count_common_tweets)

max_common_tweets <- max(common_tweets_count)
max_common_tweets




