
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
library(fastDummies)
library(caret)
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

predict_cluster_kmeans <- function(new_data, kmeans_model, reference_data) {
  
  expected_vars <- c("gender", "age", "country", "ethnicity", "education")
  if (!all(expected_vars %in% colnames(new_data))) {
    stop("O dataset precisa conter as colunas: gender, age, country, ethnicity, education")
  }
  
  # Cria dummies no dataset de referência e novo, com mesmo tratamento
  ref_dummies <- dummy_cols(reference_data, remove_first_dummy = FALSE, remove_selected_columns = TRUE)
  new_dummies <- dummy_cols(new_data, remove_first_dummy = FALSE, remove_selected_columns = TRUE)
  
  # Alinha os nomes das colunas
  reference_cols <- colnames(ref_dummies)
  
  # Adiciona colunas faltantes no novo dataset
  missing_in_new <- setdiff(reference_cols, colnames(new_dummies))
  for (col in missing_in_new) {
    new_dummies[[col]] <- 0
  }
  
  # Remove colunas extras do novo dataset
  new_dummies <- new_dummies[, reference_cols, drop = FALSE]
  
  # Calcular distâncias aos centroides
  distances <- as.matrix(dist(rbind(kmeans_model$centers, new_dummies)))
  distances <- distances[-seq_len(nrow(kmeans_model$centers)), 1:nrow(kmeans_model$centers)]
  
  if (is.vector(distances)) {
    distances <- matrix(distances, ncol = length(kmeans_model$size))
  }
  
  cluster_assignment <- apply(distances, 1, which.min)
  
  new_data$cluster <- cluster_assignment
  return(new_data)
}

# -------------------------------------------------------------------------------------------------------------------
# Function to assign confidence based on characteristics of the annotator
# -------------------------------------------------------------------------------------------------------------------

compute_conf_column <- function(df_train_train, df_train_new) {
  demog_cols <- c("gender", "age", "ethnicity", "education", "country")
  
  df_train_train$profile_key <- apply(df_train_train[, demog_cols], 1, paste, collapse = "|")
  df_train_train$profile_key <- as.character(df_train_train$profile_key)
  
  profile_yes <- table(df_train_train$profile_key[df_train_train$label_task1_1 == "YES"])
  profile_total <- table(df_train_train$profile_key)
  
  profile_conf <- mapply(function(key) {
    yes_count <- ifelse(!is.na(profile_yes[key]), profile_yes[key], 0)
    total_count <- profile_total[key]
    round(yes_count / total_count, 4)
  }, names(profile_total))
  names(profile_conf) <- names(profile_total)
  
  df_train_new$profile_key <- apply(df_train_new[, demog_cols], 1, paste, collapse = "|")
  df_train_new$profile_key <- as.character(df_train_new$profile_key)
  
  df_train_new$Conf <- ifelse(df_train_new$profile_key %in% names(profile_conf),
                              profile_conf[df_train_new$profile_key],
                              NA)
  
  if (any(is.na(df_train_new$Conf))) {
    dummies <- dummyVars(~ ., data = df_train_train[, demog_cols])
    train_matrix <- predict(dummies, newdata = df_train_train[, demog_cols])
    new_matrix <- predict(dummies, newdata = df_train_new[, demog_cols])  # <- CORRIGIDO
    new_matrix <- as.data.frame(new_matrix)
    train_matrix <- as.data.frame(train_matrix)
    
    missing_cols <- setdiff(colnames(train_matrix), colnames(new_matrix))
    for (col in missing_cols) {
      new_matrix[[col]] <- 0
    }
    new_matrix <- new_matrix[, colnames(train_matrix)]
    
    train_conf <- df_train_train$profile_key
    train_conf_values <- sapply(train_conf, function(k) profile_conf[[k]])
    
    known_idx <- which(!is.na(train_conf_values))
    unknown_idx <- which(is.na(df_train_new$Conf))
    
    known_matrix <- train_matrix[known_idx, ]
    unknown_matrix <- new_matrix[unknown_idx, ]
    
    dist_mat <- as.matrix(dist(rbind(unknown_matrix, known_matrix)))
    n_unknown <- nrow(unknown_matrix)
    d <- dist_mat[1:n_unknown, (n_unknown + 1):nrow(dist_mat)]
    
    nearest_idx <- apply(d, 1, which.min)
    df_train_new$Conf[unknown_idx] <- train_conf_values[known_idx][nearest_idx]
  }
  
  return(df_train_new)
}












