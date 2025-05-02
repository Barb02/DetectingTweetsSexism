library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(readr)
library(dplyr)
library(recommenderlab)
library(syuzhet)
library(textclean)

# ----------------------------------- Initial Analysis -----------------------------------

#df = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST2025_train.csv")
df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_train.csv")

str(df)
dim(df)
summary(df)


tweets = unique(df$tweet)
tweets
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

# ----------------------------------- Sentiment Analysis -----------------------------------

sent <- get_sentiment(df$tweet, method = "syuzhet")
print(sent)
emotions <- get_nrc_sentiment(df$tweet)
head(emotions,10)


