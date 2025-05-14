
# Main - Creating a data set for modeling 

# -------------------------------------------------------------------------------------------------------------------
# Importing
# -------------------------------------------------------------------------------------------------------------------

source("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/functions.R")

#df = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST2025_train.csv")
df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df = read_csv("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/tables/EXIST2025_train.csv")

# -------------------------------------------------------------------------------------------------------------------
# Task 1
# -------------------------------------------------------------------------------------------------------------------

#df <- df %>%
#  group_by(tweet) %>%
#  summarise(
#    count_yes = sum(label_task1_1 == "YES", na.rm = TRUE),
#    count_no = sum(label_task1_1 == "NO", na.rm = TRUE)
#  ) %>%
#  filter(count_yes != count_no) %>%
#  mutate(final_label = ifelse(count_yes > count_no, "YES", "NO"))

#df <- df %>%  select(tweet, final_label)

#df <- important_words(df, tweet_col = "tweet")
#df <- coloc(df)
#df <- sent_seq(df)
#df <- stats_emot_sent(df)

#df <- df2 %>%
#  left_join(df, by = "tweet")

# -- Cut the lines that have NAs which are the line that contain 
#df <- na.omit(df)
#df <- subset(df, select = -final_label)
#names(df)[names(df) == "gender.x"] <- "gender"
#names(df)[11:20] <- paste0("word_", names(df)[11:20])
#names(df)[names(df) == "word_gender.y"] <- "word_gender"

#save(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task1.RData")

load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task1.RData")

# -------------------------------------------------------------------------------------------------------------------
# Task 2
# -------------------------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------------------------
# Task 3
# -------------------------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------------------------
# Task 4
# -------------------------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------------------------
# Task 5
# -------------------------------------------------------------------------------------------------------------------





