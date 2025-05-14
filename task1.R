
# Task 1: Sexism Identification Classifier

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

# -------------------------------------------------------------------------------------------------------------------
# Initial Analysis
# -------------------------------------------------------------------------------------------------------------------

#df = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST2025_train.csv")
df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df = read_csv("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/tables/EXIST2025_train.csv")

str(df)
dim(df)
summary(df)

# -------------------------------------------------------------------------------------------------------------------
# Grouping by tweet
# -------------------------------------------------------------------------------------------------------------------

df <- df %>%
  group_by(tweet) %>%
  summarise(
    count_yes = sum(label_task1_1 == "YES", na.rm = TRUE),
    count_no = sum(label_task1_1 == "NO", na.rm = TRUE)
  ) %>%
  filter(count_yes != count_no) %>%
  mutate(final_label = ifelse(count_yes > count_no, "YES", "NO"))

df <- df %>%
  select(tweet, final_label)

df %>%
  count(final_label)

tweets = df$tweet

length(tweets)

# -------------------------------------------------------------------------------------------------------------------
# NLP
# -------------------------------------------------------------------------------------------------------------------

# Function for cleaning tokenizing, removing stop words, numbers, and applying stemming
process_tweets <- function(tweets) {
  tweets <- replace_html(tweets)                             # expose ampersands (&) to be removed later
  tweets <- gsub("@\\w+", "", tweets)                        # remove usernames
  tweets <- gsub("http\\S+", "", tweets)                     # remove links
  tweets <- gsub("\\.(\\S)", ". \\1", tweets, perl = TRUE)   # remove dots
  corpus <- corpus(tweets)
  toks <- tokens(
    corpus,
    remove_punct = TRUE,
    remove_symbols = TRUE
  )
  toks <- tokens_tolower(toks)
  toks <- tokens_remove(toks, stopwords("english"))
  toks <- tokens_remove(toks, pattern = "^\\d+$", valuetype = "regex")
  toks <- tokens_wordstem(toks)
  return(toks)
}

toks <- process_tweets(tweets)

dfm <- dfm(toks)                                           # dfm and small analysis of it 

ndoc(dfm)
nfeat(dfm)
featnames(dfm)
textplot_wordcloud(dfm, min_count = 50)                    # word cloud

dfm_tfidf <- dfm_tfidf(dfm)                                # TF-IDF
dfm_tfidf

topfeatures(dfm, 50)                                      # Top Features of all tweets
topfeatures(dfm_tfidf, 50) 

# -------------------------------------------------------------------------------------------------------------------
# Important words
# -------------------------------------------------------------------------------------------------------------------

# In this segment, we will explore the most important but also discriminating 
# words for tweets classified as YES and tweets classified as no 

dfm_yes <- dfm_subset(dfm_tfidf, df$final_label == "YES")
dfm_no <- dfm_subset(dfm_tfidf, df$final_label == "NO")

# -- Extract the top features for each --

top_yes <- topfeatures(dfm_yes, 30)
top_no <- topfeatures(dfm_no, 20)

top_yes
top_no

# Next we will plot the top words according to their frequency on tweets labbeled as YES and NO
# The plots will be next to each other for it to be easier to see the words that are not only 
#frequent but also relevant to our study

df_yes <- data.frame(word = names(top_yes), count = as.numeric(top_yes))
df_yes <- df_yes %>%
  arrange(desc(count))

n_yes <- ndoc(dfm_yes)                                  # total number of tweets labeled as YES
n_no <- ndoc(dfm_no)                                    # total number of tweets labeled as NO

# -- Calculate the relative frequency of each word in "YES" tweets --

df_yes <- data.frame(word = names(top_yes), count = as.numeric(top_yes))
df_yes <- df_yes %>%
  mutate(relative_count = count / n_yes) %>%   
  arrange(desc(relative_count))

# -- Bar plot for the top words in "YES" tweets, showing the relative frequency --

p_yes <- ggplot(df_yes, aes(x = reorder(word, -relative_count), y = relative_count, fill = "lightgreen")) +
  geom_col() + 
  labs(
    title = "Relative Frequency of Top Words in 'YES' Tweets", 
    x = "Word", 
    y = "Relative Frequency"  
  ) +
  scale_fill_manual(values = c("lightgreen")) +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(fill = "none") 

# -- Same for "NO" tweets --
df_no <- data.frame(word = names(top_no), count = as.numeric(top_no))
df_no <- df_no %>%
  mutate(relative_count = count / n_no) %>%  
  arrange(desc(relative_count))

# -- Bar plot for the top words in "NO" tweets, showing the relative frequency --
p_no <- ggplot(df_no, aes(x = reorder(word, -relative_count), y = relative_count, fill = "tomato")) +
  geom_col() +  
  labs(
    title = "Relative Frequency of Top Words in 'NO' Tweets", 
    x = "Word",  
    y = "Relative Frequency" 
  ) +
  scale_fill_manual(values = c("tomato")) + 
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")  

# Display the two plots side by side
grid.arrange(p_yes, p_no, ncol = 2)  

# From this analysis, we've decided to include the following words to the data set we will be using for modeling

# Important words associated with YES tweets -> "woman" "women" "men" "girl" "sex" "bitch" "fuck"

# Important words associated with NO tweets -> "love" "peopl" "gender"

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

df <- important_words(df)

# -------------------------------------------------------------------------------------------------------------------
# Collocations for Yes and No tweets
# -------------------------------------------------------------------------------------------------------------------

tweets_yes <- df %>% filter(final_label == "YES") %>% pull(tweet)
tweets_no <- df %>% filter(final_label == "NO") %>% pull(tweet)

toks_yes <- process_tweets(tweets_yes)
toks_no <- process_tweets(tweets_no)

collocs_yes_2 <- textstat_collocations(toks_yes, size = 2, min_count = 5)
top_collocs_yes_2 <- collocs_yes_2 %>% arrange(desc(count)) %>% head(25)

collocs_yes_3 <- textstat_collocations(toks_yes, size = 3, min_count = 5)
top_collocs_yes_3 <- collocs_yes_3 %>% arrange(desc(count)) %>% head(25)

collocs_no_2 <- textstat_collocations(toks_no, size = 2, min_count = 5)
top_collocs_no_2 <- collocs_no_2 %>% arrange(desc(count)) %>% head(25)

collocs_no_3 <- textstat_collocations(toks_no, size = 3, min_count = 5)
top_collocs_no_3 <- collocs_no_3 %>% arrange(desc(count)) %>% head(25)

top_collocs_yes_2
top_collocs_yes_3
top_collocs_no_2
top_collocs_no_3

# The collocations are clearly very different in YES and NO tweets so the idea is to create a binary feature
# for each label with 0 if a tweet doesn't have one of the collocations and 1 if it has  

# We will only be using the collocations with size 2 since with 3 there are very few occurrences 

# Note: the only common collocation in both is "look like" but it appears many more times on the YES tweets
# than it does on the NO tweets. Also, from the collocations with size 3, we can see that there are very 
# negative expressions that are most likely sexist using look like so we've decided to cut "look like"
# from the NO when creating the features. 

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

df <- coloc(df)

# -------------------------------------------------------------------------------------------------------------------
# Correlations for Yes and No tweets
# -------------------------------------------------------------------------------------------------------------------

dfm_yes_trim <- dfm_trim(dfm_yes, min_termfreq = 10)
dfm_no_trim <- dfm_trim(dfm_no, min_termfreq = 10)

correlations_yes <- textstat_simil(dfm_yes_trim, margin = "features", method = "correlation")
correlations_no <- textstat_simil(dfm_no_trim, margin = "features", method = "correlation")

corr_list_yes <- as.list(correlations_yes)
corr_list_no <- as.list(correlations_no)

corr_filtered_yes <- lapply(corr_list_yes, function(x) x[x > 0.8])
corr_filtered_no <- lapply(corr_list_no, function(x) x[x > 0.8])

cat("\n=== Correlations in YES tweets ===\n")
for (term in names(corr_filtered_yes)) {
  correlated_terms <- names(corr_filtered_yes[[term]])
  if (length(correlated_terms) > 0) {
    cat("\nTerm:", term, "\nCorrelated with:", paste(correlated_terms, collapse = ", "), "\n")
  }
}

cat("\n=== Correlations in NO tweets ===\n")
for (term in names(corr_filtered_no)) {
  correlated_terms <- names(corr_filtered_no[[term]])
  if (length(correlated_terms) > 0) {
    cat("\nTerm:", term, "\nCorrelated with:", paste(correlated_terms, collapse = ", "), "\n")
  }
}

# Some of the strongly correlated word pairs also show up in the collocations we already identified.
# Since we’re treating collocations as a single binary feature for each group (yes/no), 
# it makes sense to keep those words together. 
# So, no changes will be made to the data set based on these correlations.
# As for the other correlated words that aren’t in the collocations, they don’t show up among the top features 
# either, so there’s no real benefit in creating individual features just for them.

# -------------------------------------------------------------------------------------------------------------------
# Sentiment Analysis 
# -------------------------------------------------------------------------------------------------------------------

# Here we calculated sentiment and emotion scores for tweets' individual sentences, 
# with an overall emotion intensity score added for each sentence

sentences_per_tweet <- lapply(df$tweet, get_sentences)
all_sentences <- unlist(sentences_per_tweet)

df$tweet <- as.character(df$tweet)
repeated_tweets <- rep(df$tweet, sapply(sentences_per_tweet, length))

df_sentences <- data.frame(
  tweet = repeated_tweets,
  sentence = all_sentences,
  stringsAsFactors = FALSE
)

df_sentences$sentiment <- get_sentiment(df_sentences$sentence, method = "syuzhet")

emotion_scores <- get_nrc_sentiment(df_sentences$sentence)

df_sentences <- cbind(df_sentences, emotion_scores)

# -------------------------------------------------------------------------------------------------------------------
# Sentiment Analysis - Checking if there are relevant sequences of sentiment in the tweets
# -------------------------------------------------------------------------------------------------------------------

# In this segment, we will extract sentiment signs from the sentiments of the sentences of each tweet 
# and then create sequences. Those appearing more than 10 times are going to be used to create dummies 
# in a separate data set just to check for possible correlation to our target variable
# To base our decision on more than correlations, we will also see the distribution of sentiment sequences 
# across labels (YES/NO)

df_sentences_filtered <- df_sentences 

# -- Signs -- 
df_sentences$sentiment_sign <- sign(df_sentences$sentiment)  

# -- Group by the tweet
sequence_df <- df_sentences %>%
  group_by(tweet) %>%
  summarise(sentiment_sequence = paste(sentiment_sign, collapse = ""))

sequence_counts <- sequence_df %>%
  group_by(sentiment_sequence) %>%
  summarise(freq = n())

# -- > 10 appearances -- 
frequent_sequences <- sequence_counts %>%
  filter(freq > 10)

sequence_df_filtered <- sequence_df %>%
  filter(sentiment_sequence %in% frequent_sequences$sentiment_sequence)

# -- Dummies -- 
sequence_df_filtered$sequence_id <- sequence_df_filtered$sentiment_sequence
sequence_df_filtered <- fastDummies::dummy_cols(sequence_df_filtered, select_columns = "sequence_id", remove_selected_columns = FALSE)

sequence_with_label <- left_join(sequence_df_filtered, df[, c("tweet", "final_label")], by = "tweet")
sequence_with_label$final_label <- ifelse(sequence_with_label$final_label == "YES", 1, 0)

# -- Correlation --
sequence_cols <- grep("^sequence_id_", colnames(sequence_with_label), value = TRUE)
sequence_corr <- sapply(sequence_with_label[, sequence_cols], function(x) cor(x, sequence_with_label$final_label))
sequence_corr <- sort(sequence_corr[order(abs(sequence_corr), decreasing = TRUE)])
print(sequence_corr)

# -- Distribution -- 
sequence_label_dist <- sequence_with_label %>%
  group_by(sentiment_sequence, final_label) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = final_label, values_from = count, values_fill = 0) %>%
  rename(NO = `0`, YES = `1`) %>%
  arrange(desc(YES + NO))

print(sequence_label_dist)

# Even though there are some decent sequences we could use for our data set, we decided to explore in a more 
# general way grouping sequences in broader categories 

# -------------------------------------------------------------------------------------------------------------------
# Sentiment Analysis - Checking if there are relevant patterns of sequences of sentiment in the tweets
# -------------------------------------------------------------------------------------------------------------------

# Here are the patterns we selected to explore in order to determine their potential value:
# all positive -> tweets in which all sentences have positive sentiment
# all negative -> tweets in which all sentences have negative sentiment
# positive to neutral -> tweets in the first sentence has positive sentiment and the last neutral sentiment
# negative to neutral -> tweets in the first sentence has negative sentiment and the last neutral sentiment
# positive to negative -> tweets in the first sentence has positive sentiment and the last negative sentiment
# negative to positive -> tweets in the first sentence has negative sentiment and the last positive sentiment
# neutral -> tweets in which all sentences have neutral sentiment
# mixed -> the rest 

# We will be doing the same we did for the sequences but this time for the patterns of sequences

df_sentences_filtered <- df_sentences 

df_sentences_filtered$sentiment_sign <- sign(df_sentences_filtered$sentiment)

pattern_df <- df_sentences_filtered %>%
  group_by(tweet) %>%
  summarise(sentiment_sequence = paste(sentiment_sign, collapse = ""))

pattern_df <- pattern_df %>%
  mutate(
    pattern_type = case_when(
      grepl("^1+$", sentiment_sequence) ~ "all_pos",
      grepl("^(-1)+$", sentiment_sequence) ~ "all_neg",
      grepl("^1.*0$", sentiment_sequence) ~ "pos_to_neutral",
      grepl("^(-1).*0$", sentiment_sequence) ~ "neg_to_neutral",
      grepl("^1.*-1$", sentiment_sequence) ~ "pos_to_neg",
      grepl("^-1.*1$", sentiment_sequence) ~ "neg_to_pos",
      grepl("^0+$", sentiment_sequence) ~ "neutral",
      TRUE ~ "mixed"
    )
  )

# -- Dummies -- 
pattern_df <- fastDummies::dummy_cols(pattern_df, select_columns = "pattern_type", remove_selected_columns = TRUE)

pattern_with_label <- left_join(pattern_df, df[, c("tweet", "final_label")], by = "tweet")
pattern_with_label$final_label <- ifelse(pattern_with_label$final_label == "YES", 1, 0)

# -- Correlation --
pattern_cols <- grep("^pattern_type_", colnames(pattern_with_label), value = TRUE)
pattern_corr <- sapply(pattern_with_label[, pattern_cols], function(x) cor(x, pattern_with_label$final_label))
pattern_corr <- sort(pattern_corr[order(abs(pattern_corr), decreasing = TRUE)])
print(pattern_corr)

# -- Distribution --
pattern_with_label$pattern_type_name <- apply(
  pattern_with_label[, pattern_cols],
  1,
  function(row) {
    matched <- names(row)[which(row == 1)]
    if (length(matched) > 0) {
      gsub("pattern_type_", "", matched)
    } else {
      NA
    }
  }
)

pattern_label_dist <- pattern_with_label %>%
  group_by(pattern_type_name, final_label) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = final_label, values_from = count, values_fill = 0) %>%
  rename(NO = `0`, YES = `1`) %>%
  arrange(desc(YES + NO))
print(pattern_label_dist)

# Considering both the correlation and the distribution across the labels we've decided to had the 
# following features to our data set: all negative and all positive

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

df <- sent_seq(df)

# -------------------------------------------------------------------------------------------------------------------
# Sentiment Analysis - Checking if there are relevant sequences of emotions in the tweets
# -------------------------------------------------------------------------------------------------------------------

# In this segment we will be testing sequences of emotions to see if there is something we can use
# For each tweet, we will check the dominant emotion in each sentence and see the most frequent
# For those, dummies will be created and then we will decide if we'll turn the information into a feature
# based on the correlation to the target variable and analysis of the distribution 
# To enhance our analysis, we will filter to only have the sentences with emotions and sequences that involve 
# more than 1 different emotions

emotion_cols <- c("anger", "anticipation", "disgust", "fear", "joy", 
                  "sadness", "surprise", "trust", "positive", "negative")

# -- No neutral sentences -- 
df_sentences_nonzero <- df_sentences %>%
  filter(rowSums(select(., all_of(emotion_cols))) > 0)

# -- Dominant emotion --
df_sentences_nonzero$dominant_emotion <- apply(df_sentences_nonzero[, emotion_cols], 1, function(row) {
  return(names(row)[which.max(row)])
})

emotion_seq_df <- df_sentences_nonzero %>%
  group_by(tweet) %>%
  summarise(
    emotion_sequence = paste(dominant_emotion, collapse = "-"),
    n_unique_emotions = n_distinct(dominant_emotion),
    .groups = "drop"
  ) %>%
  filter(n_unique_emotions > 1)

sequence_counts <- emotion_seq_df %>%
  group_by(emotion_sequence) %>%
  summarise(freq = n(), .groups = "drop")

# -- > 10 apppearances -- 
frequent_sequences <- sequence_counts %>% filter(freq > 10)

emotion_seq_df <- emotion_seq_df %>%
  filter(emotion_sequence %in% frequent_sequences$emotion_sequence)

# -- Dummies --
emotion_seq_df <- fastDummies::dummy_cols(emotion_seq_df, select_columns = "emotion_sequence", remove_selected_columns = FALSE)

emotion_seq_df <- left_join(emotion_seq_df, df[, c("tweet", "final_label")], by = "tweet")
emotion_seq_df$final_label <- ifelse(emotion_seq_df$final_label == "YES", 1, 0)

# -- Correlation -- 
emotion_cols_dummy <- grep("^emotion_sequence_", colnames(emotion_seq_df), value = TRUE)
emotion_seq_corr <- sapply(emotion_seq_df[, emotion_cols_dummy], function(x) cor(x, emotion_seq_df$final_label))
emotion_seq_corr <- emotion_seq_corr[order(abs(emotion_seq_corr), decreasing = TRUE)]

multi_emotion_cols <- emotion_cols_dummy[
  sapply(emotion_cols_dummy, function(col) {
    n_emotions <- length(unlist(strsplit(gsub("emotion_sequence_", "", col), "-")))
    return(n_emotions > 1)
  })
]

print(emotion_seq_corr[multi_emotion_cols])

# -- Distribution --

emotion_seq_dist_multi <- emotion_seq_df %>%
  select(all_of(c("final_label", multi_emotion_cols))) %>%
  pivot_longer(
    cols = all_of(multi_emotion_cols),
    names_to = "emotion_sequence",
    values_to = "is_present"
  ) %>%
  filter(is_present == 1) %>%
  group_by(emotion_sequence, final_label) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = final_label,
    values_from = count,
    values_fill = 0
  ) %>%
  rename(NO = `0`, YES = `1`) %>%
  arrange(desc(YES + NO))

print(emotion_seq_dist_multi)

# Based on this, we've decided to not implement any of these in our data set since their occurrences are too low

# -------------------------------------------------------------------------------------------------------------------
# Sentiment Analysis - Checking if there are relevant statistic or general feature based on sentiment or emotions
# -------------------------------------------------------------------------------------------------------------------

# In this segment we will extract statistical and general features from sentiment and emotion 
# and see their correlation to the target column to see if there is any relevant one

df_final <- df

temp_df <- df[, c("tweet", "final_label")]

# -- Add general features --

tweet_emotions <- get_nrc_sentiment(df$tweet)

df <- cbind(df, tweet_emotions)

df$tweet_sentiment <- get_sentiment(df$tweet, method = "syuzhet")

# -- Add statistical features --

temp_df_sentences <- df_sentences %>%
  group_by(tweet) %>%
  summarise(
    sent_start = first(sentiment),
    sent_end = last(sentiment),
    sent_diff = sent_end - sent_start,
    sent_max = max(sentiment),
    sent_min = min(sentiment),
    sent_mean = mean(sentiment),
    sent_sd = sd(sentiment),
    sent_sign_changes = sum(diff(sign(sentiment)) != 0, na.rm = TRUE)
  )

# -- For one sentence tweets --
temp_df_sentences$sent_sd[is.na(temp_df_sentences$sent_sd)] <- 0


emotion_features <- df_sentences %>%
  group_by(tweet) %>%
  summarise(across(anger:trust, list(mean = mean, max = max, sum = sum)))

df <- left_join(df, temp_df_sentences, by = "tweet")
df <- left_join(df, emotion_features, by = "tweet")
df <- left_join(df, df[, c("tweet", "tweet_sentiment")], by = "tweet")

df$final_label <- ifelse(df$final_label == "YES", 1, 0)

# -- Check distribution between YES and NO --

n_yes <- sum(df$final_label == 1)
n_no  <- sum(df$final_label == 0)

yes_counts <- colSums(df[df$final_label == 1, 17:26] >= 1)
no_counts  <- colSums(df[df$final_label == 0, 17:26] >= 1)

yes_prop <- yes_counts / n_yes
no_prop  <- no_counts  / n_no

result <- data.frame(
  Column = names(df)[17:26],
  Count_1 = yes_counts,
  Proportion_1 = round(yes_prop, 3),
  Count_0 = no_counts,
  Proportion_0 = round(no_prop, 3)
)

print(result)

# -- Correlation -- 

dft <- df[, "final_label", drop = FALSE]
dft <- cbind(dft, df[, 17:60])

cor_matrix <- cor(dft, use = "complete.obs")

cor_label <- cor_matrix["final_label", ]
cor_label <- cor_label[!names(cor_label) %in% "final_label"]
cor_label <- cor_label[order(abs(cor_label), decreasing = TRUE)]

print(cor_label)

numeric_features <- na.omit(dft)

cor_matrix <- cor(numeric_features, use = "complete.obs")

corrplot(cor_matrix,
         method = "circle",
         type = "upper",
         tl.cex = 0.4,
         tl.srt = 45,
         cl.cex = 1.0,
         title = "Correlation Matrix of Features",
         mar = c(0, 0, 0, 0))

# Based on the result and in order to not get redundant with the features we're extracting, 
# We've decided to have columns for the: sent_min, tweet_sentiment, disgust_max and sadness

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
  
  df_final <- df %>%
    left_join(df_tweet_level, by = "tweet") %>%
    left_join(sentence_features, by = "tweet")
  
  return(df_final)
}

df_final <- stats_emot_sent(df_final)

# -------------------------------------------------------------------------------------------------------------------
# Sentiment Analysis - Checking if there are relevant start or finish emotion
# -------------------------------------------------------------------------------------------------------------------

# In this segment we will create a data set with binary features that for each emotion are 1 if it is in the first non
# neutral sentence and 0 if not and the same thing for the last non neutral sentence

emotion_cols <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", 
                  "surprise", "trust", "negative", "positive")


process_tweet <- function(tweet_data) {
  
  tweet_data[emotion_cols] <- tweet_data[emotion_cols] >= 1
  
  non_neutral <- tweet_data %>% filter(sentiment_sign != 0)
  
  if (nrow(non_neutral) == 0) {
    return(setNames(rep(0, length(emotion_cols) * 2),
                    c(paste0("start_", emotion_cols), paste0("end_", emotion_cols))))
  }
  
  first_row <- non_neutral[1, ]
  last_row  <- non_neutral[nrow(non_neutral), ]
  
  start_vec <- as.integer(sapply(emotion_cols, function(col) first_row[[col]]))
  end_vec   <- as.integer(sapply(emotion_cols, function(col) last_row[[col]]))
  
  names(start_vec) <- paste0("start_", emotion_cols)
  names(end_vec)   <- paste0("end_", emotion_cols)
  
  return(c(start_vec, end_vec))
}

df_emotions <- df_sentences %>%
  group_by(tweet) %>%  
  group_modify(~as.data.frame(t(process_tweet(.x)))) %>%
  ungroup()

df_emotions <- df_emotions %>%
  left_join(df %>%
              select(tweet, final_label), by = "tweet")

n_yes <- sum(df_emotions$final_label == 1)
n_no  <- sum(df_emotions$final_label == 0)

yes_counts <- colSums(df_emotions[df_emotions$final_label == 1, 2:21] >= 1)
no_counts  <- colSums(df_emotions[df_emotions$final_label == 0, 2:21] >= 1)

yes_prop <- yes_counts / n_yes
no_prop  <- no_counts  / n_no

result <- data.frame(
  Column = names(df_emotions)[2:21],
  Count_1 = yes_counts,
  Proportion_1 = round(yes_prop, 3),
  Count_0 = no_counts,
  Proportion_0 = round(no_prop, 3)
)

# -- Difference -- 
result$Difference <- abs(result$Proportion_1 - result$Proportion_0)

print(result)

# The results show that the most significant differences between proportions in YES and NO tweets are the sadness
# and disgust but we already have features regarding those emotions so we decided not to had anything 
# to avoid redundancy

# -------------------------------------------------------------------------------------------------------------------
# Exporting the data set to model in Python
# -------------------------------------------------------------------------------------------------------------------

write.csv(df_final, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/task1.csv", row.names = FALSE)












