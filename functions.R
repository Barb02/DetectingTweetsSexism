
# Functions

# -------------------------------------------------------------------------------------------------------------------
# Libraries
# -------------------------------------------------------------------------------------------------------------------

library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(readr)
library(dplyr)
library(recommenderlab)
library(syuzhet)
library(textclean)
library(stringr)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(purrr)
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/top_collocs_yes_2.RData")
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/top_collocs_no_2.RData")

# -------------------------------------------------------------------------------------------------------------------
# Function used to create the features based on the conclusions taken from the important words
# -------------------------------------------------------------------------------------------------------------------

important_words <- function(df, tweet_col = "tweet") {
  
  words <- c("woman", "women", "men", "girl", "sex", "bitch", "fuck", 
             "love", "peopl", "gender")
  
  df_copy <- df
  tweets_lower <- tolower(df_copy[[tweet_col]])
  
  for (word in words) {
    col_name <- word
    df_copy[[col_name]] <- as.integer(str_detect(tweets_lower, fixed(word)))
  }
  
  return(df_copy)
}

# -------------------------------------------------------------------------------------------------------------------
# Function used to create the features based on the conclusions taken from the collocations
# -------------------------------------------------------------------------------------------------------------------

coloc <- function(df) {
  
  collocations_yes <- top_collocs_yes_2$collocation
  collocations_no <- top_collocs_no_2$collocation
  collocations_no <- collocations_no[collocations_no != "look like"]
  
  check_collocation <- function(tweet, collocations) {
    tweet <- tolower(tweet)
    for (colloc in collocations) {
      if (str_detect(tweet, fixed(tolower(colloc)))) {
        return(1)
      }
    }
    return(0)
  }
  
  df$colloc_yes <- sapply(df$tweet, check_collocation, collocations = collocations_yes)
  df$colloc_no  <- sapply(df$tweet, check_collocation, collocations = collocations_no)
  
  return(df)
}

# -------------------------------------------------------------------------------------------------------------------
# Function used to create the features based on the conclusions taken from sentiment analysis Part 1
# -------------------------------------------------------------------------------------------------------------------

sent_seq <- function(data) {
  data$tweet <- as.character(data$tweet)
  
  sentences_per_tweet <- lapply(data$tweet, get_sentences)
  sentiments_per_tweet <- lapply(sentences_per_tweet, get_sentiment, method = "syuzhet")
  
  data$all_pos <- sapply(sentiments_per_tweet, function(sentiments) {
    if (length(sentiments) > 0 && all(sign(sentiments) == 1)) {
      return(1)
    } else {
      return(0)
    }
  })
  
  data$all_neg <- sapply(sentiments_per_tweet, function(sentiments) {
    if (length(sentiments) > 0 && all(sign(sentiments) == -1)) {
      return(1)
    } else {
      return(0)
    }
  })
  
  return(data)
}

# -------------------------------------------------------------------------------------------------------------------
# Function used to create the features based on the conclusions taken from sentiment analysis Part 2
# -------------------------------------------------------------------------------------------------------------------

stats_emot_sent <- function(df) {
  
  sentences_per_tweet <- lapply(df$tweet, get_sentences)
  all_sentences <- unlist(sentences_per_tweet)
  tweet_lengths <- sapply(sentences_per_tweet, length)
  repeated_tweets <- rep(df$tweet, tweet_lengths)
  
  df_sentences <- data.frame(
    tweet = repeated_tweets,
    sentence = all_sentences,
    stringsAsFactors = FALSE
  )
  
  df_sentences$sentiment <- get_sentiment(df_sentences$sentence, method = "syuzhet")
  df_sentences$disgust <- get_nrc_sentiment(df_sentences$sentence)$disgust
  
  df_tweet_level <- data.frame(
    tweet = df$tweet,
    tweet_sentiment = get_sentiment(df$tweet, method = "syuzhet"),
    sadness = get_nrc_sentiment(df$tweet)$sadness,
    stringsAsFactors = FALSE
  )
  
  sentence_features <- df_sentences %>%
    group_by(tweet) %>%
    summarise(
      sent_min = min(sentiment, na.rm = TRUE),
      disgust_max = max(disgust, na.rm = TRUE)
    )
  
  df_final <- suppressWarnings(
    df %>%
      left_join(df_tweet_level, by = "tweet") %>%
      left_join(sentence_features, by = "tweet")
  )
  
  return(df_final)
}

# -------------------------------------------------------------------------------------------------------------------
# Function to assign clusters based on annotators characteristics
# -------------------------------------------------------------------------------------------------------------------

predict_hclust <- function(new_data, reference_df, cluster_labels, levels_list) {
  # Ajustar fatores para terem os mesmos níveis do dataset original
  for (varname in names(levels_list)) {
    if (varname %in% colnames(new_data)) {
      new_data[[varname]] <- factor(new_data[[varname]], levels = levels_list[[varname]])
    }
  }
  
  # One-hot encoding igual ao usado em reference_df
  vars <- colnames(reference_df)
  new_data_ohe <- model.matrix(~ . - 1, data = new_data)
  
  # Garantir que colunas estejam na mesma ordem e estrutura
  common_cols <- intersect(colnames(new_data_ohe), vars)
  new_data_ohe_aligned <- matrix(0, nrow = nrow(new_data), ncol = length(vars))
  colnames(new_data_ohe_aligned) <- vars
  rownames(new_data_ohe_aligned) <- rownames(new_data)
  
  new_data_ohe_aligned[, common_cols] <- new_data_ohe[, common_cols]
  
  # Função para calcular distância média até cada cluster
  assign_cluster <- function(point) {
    dists <- sapply(unique(cluster_labels), function(k) {
      members <- reference_df[cluster_labels == k, ]
      mean(sqrt(rowSums((t(t(members) - point))^2)))
    })
    which.min(dists)
  }
  
  # Aplicar para cada nova observação
  assigned_clusters <- apply(new_data_ohe_aligned, 1, assign_cluster)
  
  # One-hot encode dos clusters atribuídos
  cluster_ohe <- model.matrix(~ factor(assigned_clusters) - 1)
  colnames(cluster_ohe) <- paste0("cluster_", sort(unique(cluster_labels)))
  
  result <- cbind(new_data, cluster = assigned_clusters, cluster_ohe)
  return(result)
}















