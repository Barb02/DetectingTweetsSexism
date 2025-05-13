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

# ----------------------------------- Initial Analysis -----------------------------------

#df = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST2025_train.csv")
df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df = read_csv("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/tables/EXIST2025_train.csv")

str(df)
dim(df)
summary(df)

# ----------------------------------- Changing to one per tweet -----------------------------------

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

# ---------- Tokens for YES and for NO separately -------------------------------------------------

tweets_yes <- df$tweet[df$final_label == "YES"]
tweets_no <- df$tweet[df$final_label == "NO"]

clean_tweets <- function(tweets) {
  tweets <- replace_html(tweets)
  tweets <- gsub("@\\w+", "", tweets)
  tweets <- gsub("http\\S+", "", tweets)
  tweets <- gsub("\\.(\\S)", ". \\1", tweets, perl = TRUE)
  return(tweets)
}

tweets_yes <- clean_tweets(tweets_yes)
tweets_no <- clean_tweets(tweets_no)

toks_yes <- tokens(tweets_yes, remove_punct = TRUE, remove_symbols = TRUE)
toks_yes <- tokens_tolower(toks_yes)
toks_yes <- tokens_remove(toks_yes, stopwords("english"))
toks_yes <- tokens_remove(toks_yes, pattern = "^\\d+$", valuetype = "regex")
toks_yes <- tokens_wordstem(toks_yes)

toks_no <- tokens(tweets_no, remove_punct = TRUE, remove_symbols = TRUE)
toks_no <- tokens_tolower(toks_no)
toks_no <- tokens_remove(toks_no, stopwords("english"))
toks_no <- tokens_remove(toks_no, pattern = "^\\d+$", valuetype = "regex")
toks_no <- tokens_wordstem(toks_no)

# ----------------------------------- Collocations YES -----------------------------------

collocs_yes_2 <- textstat_collocations(toks_yes, size = 2, min_count = 5)
top_collocs_yes_2 <- collocs_yes_2 %>% arrange(desc(count)) %>% head(25)

collocs_yes_3 <- textstat_collocations(toks_yes, size = 3, min_count = 5)
top_collocs_yes_3 <- collocs_yes_3 %>% arrange(desc(count)) %>% head(25)

# ----------------------------------- Collocations NO -----------------------------------

collocs_no_2 <- textstat_collocations(toks_no, size = 2, min_count = 5)
top_collocs_no_2 <- collocs_no_2 %>% arrange(desc(count)) %>% head(25)

collocs_no_3 <- textstat_collocations(toks_no, size = 3, min_count = 5)
top_collocs_no_3 <- collocs_no_3 %>% arrange(desc(count)) %>% head(25)

top_collocs_yes_2
top_collocs_yes_3
top_collocs_no_2
top_collocs_no_3

# ----------------------------------- Correlações Separadas YES e NO -----------------------------------

# Criar dfm para cada classe
dfm_yes <- dfm_subset(dfm, df$final_label == "YES")
dfm_no <- dfm_subset(dfm, df$final_label == "NO")

# Opcional: reduzir para palavras com frequência mínima
dfm_yes_trim <- dfm_trim(dfm_yes, min_termfreq = 10)
dfm_no_trim <- dfm_trim(dfm_no, min_termfreq = 10)

# Calcular correlações
correlations_yes <- textstat_simil(dfm_yes_trim, margin = "features", method = "correlation")
correlations_no <- textstat_simil(dfm_no_trim, margin = "features", method = "correlation")

# Filtrar para correlações > 0.8
corr_list_yes <- as.list(correlations_yes)
corr_list_no <- as.list(correlations_no)

corr_filtered_yes <- lapply(corr_list_yes, function(x) x[x > 0.8])
corr_filtered_no <- lapply(corr_list_no, function(x) x[x > 0.8])

# Mostrar resultados para YES
cat("\n=== Correlações em tweets YES ===\n")
for (term in names(corr_filtered_yes)) {
  correlated_terms <- names(corr_filtered_yes[[term]])
  if (length(correlated_terms) > 0) {
    cat("\nPalavra:", term, "\nCorrelacionadas:", paste(correlated_terms, collapse = ", "), "\n")
  }
}

# Mostrar resultados para NO
cat("\n=== Correlações em tweets NO ===\n")
for (term in names(corr_filtered_no)) {
  correlated_terms <- names(corr_filtered_no[[term]])
  if (length(correlated_terms) > 0) {
    cat("\nPalavra:", term, "\nCorrelacionadas:", paste(correlated_terms, collapse = ", "), "\n")
  }
}

# ----------------------------------- Sentiment Analysis -----------------------------------

tweets2 = df$tweet

sent <- get_sentiment(tweets2, method = "syuzhet")
print(sent)

emotions <- get_nrc_sentiment(tweets2)
head(emotions)

# ----------------------------------- Preparing a new data set for modeling -----------------------------------

dfm_yes <- dfm_subset(dfm, df$final_label == "YES")
dfm_no <- dfm_subset(dfm, df$final_label == "NO")

# Top features
top_yes <- topfeatures(dfm_yes, 15)
top_no <- topfeatures(dfm_no, 15)
top_yes
top_no

df_yes <- data.frame(word = names(top_yes), count = as.numeric(top_yes))
df_yes <- df_yes %>%
  arrange(desc(count))

n_yes <- ndoc(dfm_yes)
n_no <- ndoc(dfm_no)

df_yes <- data.frame(word = names(top_yes), count = as.numeric(top_yes))
df_yes <- df_yes %>%
  mutate(relative_count = count / n_yes) %>%   # Dividir pela quantidade total de YES
  arrange(desc(relative_count))

# Gráfico para os tweets YES
p_yes <- ggplot(df_yes, aes(x = reorder(word, -relative_count), y = relative_count, fill = "lightgreen")) +
  geom_col() +
  labs(
    title = "Frequência relativa das top palavras em tweets YES",
    x = "Palavra",
    y = "Frequência Relativa"
  ) +
  scale_fill_manual(values = c("lightgreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# DataFrame para NO
df_no <- data.frame(word = names(top_no), count = as.numeric(top_no))
df_no <- df_no %>%
  mutate(relative_count = count / n_no) %>%   # Dividir pela quantidade total de NO
  arrange(desc(relative_count))

# Gráfico para os tweets NO
p_no <- ggplot(df_no, aes(x = reorder(word, -relative_count), y = relative_count, fill = "tomato")) +
  geom_col() +
  labs(
    title = "Frequência relativa das top palavras em tweets NO",
    x = "Palavra",
    y = "Frequência Relativa"
  ) +
  scale_fill_manual(values = c("tomato")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exibir os gráficos lado a lado
grid.arrange(p_yes, p_no, ncol = 2)

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





















