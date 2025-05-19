
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
# Function to assign add proportions based on annotators characteristics
# -------------------------------------------------------------------------------------------------------------------

add_proportions <- function(df, 
                            prop_table_age, 
                            prop_table_education,
                            prop_table_gender,
                            prop_table_ethnicity,
                            prop_table_country) {
  
  # Inicializar colunas "1", "2", "3" com zero
  df$`1` <- 0
  df$`2` <- 0
  df$`3` <- 0
  
  for (i in seq_len(nrow(df))) {
    
    # AGE
    age_row <- prop_table_age[prop_table_age$age == df$age[i], ]
    if (nrow(age_row) == 1) {
      df$`1`[i] <- df$`1`[i] + age_row$`1`
      df$`2`[i] <- df$`2`[i] + age_row$`2`
      df$`3`[i] <- df$`3`[i] + age_row$`3`
    }
    
    # EDUCATION
    edu_row <- prop_table_education[prop_table_education$education == df$education[i], ]
    if (nrow(edu_row) == 1) {
      df$`1`[i] <- df$`1`[i] + edu_row$`1`
      df$`2`[i] <- df$`2`[i] + edu_row$`2`
      df$`3`[i] <- df$`3`[i] + edu_row$`3`
    }
    
    # GENDER
    gender_row <- prop_table_gender[prop_table_gender$gender == df$gender[i], ]
    if (nrow(gender_row) == 1) {
      df$`1`[i] <- df$`1`[i] + gender_row$`1`
      df$`2`[i] <- df$`2`[i] + gender_row$`2`
      df$`3`[i] <- df$`3`[i] + gender_row$`3`
    }
    
    # ETHNICITY
    eth_row <- prop_table_ethnicity[prop_table_ethnicity$ethnicity == df$ethnicity[i], ]
    if (nrow(eth_row) == 1) {
      df$`1`[i] <- df$`1`[i] + eth_row$`1`
      df$`2`[i] <- df$`2`[i] + eth_row$`2`
      df$`3`[i] <- df$`3`[i] + eth_row$`3`
    }
    
    # COUNTRY
    country_row <- prop_table_country[prop_table_country$country == df$country[i], ]
    if (nrow(country_row) == 1) {
      df$`1`[i] <- df$`1`[i] + country_row$`1`
      df$`2`[i] <- df$`2`[i] + country_row$`2`
      df$`3`[i] <- df$`3`[i] + country_row$`3`
    }
  }
  
  return(df)
}















