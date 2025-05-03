library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(readr)
library(dplyr)
library(recommenderlab)
library(syuzhet)
library(textclean)
library(rpart)
library(ROCR)
library(stringr)
source("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/metrics_functions.R")

# ----------------------------------- Initial Analysis -----------------------------------

#df = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST2025_train.csv")
df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df = read_csv("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/tables/EXIST2025_train.csv")

str(df)
dim(df)
summary(df)


tweets = unique(df$tweet)
#tweets
length(tweets)

# ----------------------------------- Removing Unimportant Things -----------------------------------

tweets <- replace_html(tweets) # expose ampersands (&) to be removed later
tweets <- gsub("@\\w+", "", tweets) # remove usernames
tweets <- gsub("http\\S+", "", tweets) # remove links
tweets <- gsub("\\.(\\S)", ". \\1", tweets, perl = TRUE)

# ----------------------------------- NLP -----------------------------------

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

dfm <- dfm(toks)

ndoc(dfm)
nfeat(dfm)
featnames(dfm)
textplot_wordcloud(dfm, min_count = 50) 


any(grepl("&amp;", featnames(dfm)))  # & removed
any(grepl("peglulu2", featnames(dfm)))
any(grepl("zdvaqvaxaj", featnames(dfm)))
"20" %in% featnames(dfm)
"1.01" %in% featnames(dfm)

# TF-IDF - Helps downweight common words that occur in many documents while highlighting discriminative words
dfm_tfidf <- dfm_tfidf(dfm)
dfm_tfidf

# Top Features 
topfeatures(dfm, 200)
topfeatures(dfm_tfidf, 200)

# Collocs
collocs <- textstat_collocations(toks,
                                 size= 2,
                                 min_count = 5)
collocs_ordenado <- collocs %>% arrange(desc(count))
top_collocs <- head(collocs_ordenado,10)

# Correlations 
dfm_trimmed <- dfm_trim(dfm_tfidf, min_termfreq = 10)

correlations <- textstat_simil(
  x= dfm_trimmed, 
  margin= "features",
  method= "correlation"
)

corr_list <- as.list(correlations)

corr_filtered <- lapply(corr_list, function(x) x[x>0.8])

for (term in names(corr_filtered)) {
  correlated_terms <- names(corr_filtered[[term]])
  if (length(correlated_terms) > 0) {
    cat("\nPalavra:", term, "\nCorrelacionadas:", paste(correlated_terms, collapse = ", "), "\n")
  }
}

# ----------------------------------- Sentiment Analysis -----------------------------------

tweets2 = unique(df$tweet)

sent <- get_sentiment(tweets2, method = "syuzhet")
print(sent)

emotions <- get_nrc_sentiment(tweets2)
head(emotions)

# ----------------------------------- Preparing a new data set for modeling -----------------------------------

# Top 10 Features in columns as binary
top_feats <- names(topfeatures(dfm, 10))
dfm_top <- dfm_select(dfm, pattern = top_feats)
dfm_binary <- convert(dfm_top, to = "data.frame")
dfm_binary <- dfm_binary[, -1]
dfm_binary$tweet <- tweets2

# Sentiments and emotions as columns
features_df <- data.frame(
  tweet = tweets2,
  sentiment = sent,
  emotions,
  stringsAsFactors = FALSE
)

features_df <- cbind(features_df, dfm_binary[, -ncol(dfm_binary)])

df2 <- merge(df, features_df, by = "tweet")
df2 <- df2 %>% select(1, 10:ncol(df2))

# ----------------------------------- Statistic tests -----------------------------------

# -- Test for Sentiment --

t.test(sentiment ~ label_task1_1, data = df2) 
# Tweets labeled as sexist have significantly more negative sentiment than non-sexist ones

# -- Tests for Emotions --

e <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive")
for (emotion in e) {
  print(paste("Test for", emotion))
  print(t.test(df2[[emotion]] ~ df2$label_task1_1))
}
# Only fear and anticipation don't have a significant difference between the means 

# -- Tests for features --

for (feature in top_feats) {
  cat("\nTest for feature:", feature, "\n")
  table_feature_label <- table(df2[[feature]], df2$label_task1_1)
  chisq_result <- chisq.test(table_feature_label)
  print(chisq_result)
}
# Only the stemmed words "one" and "can" aren't significantly correlated

df2 <- df2 %>%
  select(-fear, -anticipation, -one, -can)

# -- Look_Like (The 2 words that most appear together) is worth as a binary column? --
colloc_strings <- sapply(top_collocs$collocation, paste, collapse = " ")
df2$look_like <- ifelse(str_detect(df2$tweet, "\\blook like\\b"), 1, 0)

contingency_table <- table(df2$look_like, df2$label_task1_1)
contingency_table

chi_result <- chisq.test(contingency_table)
chi_result
# It is statistically significant but it might not be worth it considering the amount of times it appears

# -- How many times do superwoman and syndrome appear in a tweet? --

# Only 54 so it's not worth investigating more 

# -- How many times do feminin and mystiqu appear in a tweet? --

# ...

df2 <- df2 %>%
  select(-look_like)

# ----------------------------------- Modeling ----------------------------------- 

set.seed(123)

# 80/20
train_index <- sample(seq_len(nrow(df2)), size = 0.8 * nrow(df2))
train_data <- df2[train_index, ]
test_data <- df2[-train_index, ]

# -- Decision Trees --

tree_model <- rpart(label_task1_1 ~ ., data = train_data, method = "class")
preds <- predict(tree_model, test_data, type = "class")

test_data$label_task1_1 <- factor(test_data$label_task1_1, levels = c("YES", "NO"))
preds <- factor(preds, levels = c("YES", "NO"))

conf_matrix <- table(Predicted = preds, Actual = test_data$label_task1_1)
print(conf_matrix)

metrics <- calculate_metrics(conf_matrix, class1 = "YES", class2 = "NO")

cat(sprintf("Metrics for \"YES\" -> Precision: %.4f, Recall: %.4f, F1-Score: %.4f\n", 
            metrics$Precision_class1, metrics$Recall_class1, metrics$F1_Score_class1))
cat(sprintf("Metrics for Class \"NO\" -> Precision: %.4f, Recall: %.4f, F1-Score: %.4f\n", 
            metrics$Precision_class2, metrics$Recall_class2, metrics$F1_Score_class2))
cat(sprintf("\nAccuracy: %.4f\n", metrics$Accuracy))



