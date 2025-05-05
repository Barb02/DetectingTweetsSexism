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
tweets <- gsub("\\.(\\S)", ". \\1", tweets, perl = TRUE) # remove dots

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
topfeatures(dfm_tfidf, 200) # weights

# Collocs

# size = 3
collocs <- textstat_collocations(toks,
                                 size= 3,
                                 min_count = 5)
collocs_ordenado <- collocs %>% arrange(desc(count))
top_collocs <- head(collocs_ordenado,10)
top_collocs
# "look like" appears a lot

# size = 2
collocs <- textstat_collocations(toks,
                                 size= 2,
                                 min_count = 5)
collocs_ordenado <- collocs %>% arrange(desc(count))
top_collocs <- head(collocs_ordenado,10)
top_collocs

# Correlations 
dfm_trimmed <- dfm_trim(dfm, min_termfreq = 10)  # dfm_tfidf?

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

top_feats <- names(topfeatures(dfm, 10))   # df tfidf ?
dfm_top <- dfm_select(dfm, pattern = top_feats)

#top_feats <- names(topfeatures(dfm_tfidf, 10))
#dfm_top <- dfm_select(dfm_tfidf, pattern = top_feats)


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
str(df2)
summary(df2)

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
  print(chisq_result$expected)
}  # this test can only be done for dfm, not for tf idf bc tf idf represent weights and not counts
# Only the stemmed words "one" and "can" are independent from the label

df2 <- df2 %>%
  select(-fear, -anticipation, -one, -can)

# -- Look_Like (The 2 words that most appear together) is worth as a binary column? --

colloc_strings <- sapply(top_collocs$collocation, paste, collapse = " ")
df2$look_like <- ifelse(str_detect(df2$tweet, "\\blook like\\b"), 1, 0)

contingency_table <- table(df2$look_like, df2$label_task1_1)
contingency_table

chi_result <- chisq.test(contingency_table)
chi_result

# It is statistically significant but it might not be worth it considering the amount of times it appears (516)

# -- How many times do superwoman and syndrome appear in a tweet together? (no order) --

dfm_temp <- convert(dfm, to = "data.frame")
sum(dfm_temp$superwoman > 0 & dfm_temp$syndrom > 0) # 19

# -- How many times do feminin and mystiqu appear in a tweet? --

sum(dfm_temp$feminin > 0 & dfm_temp$mystiqu > 0) # 20

# -- How many times do gold and digger appear in a tweet? --

sum(dfm_temp$gold > 0 & dfm_temp$digger > 0) # 19


df2 <- df2 %>%
  select(-look_like)

# Exporting the data set to model in Python

#write.csv(df2, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/df2.csv", row.names = FALSE)
#write.csv(df2, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/df2_tfidf.csv", row.names = FALSE)


