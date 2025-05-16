
# Main - Creating a data set for modeling 

# -------------------------------------------------------------------------------------------------------------------
# Importing
# -------------------------------------------------------------------------------------------------------------------

library(fastDummies)
source("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/functions.R")
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/prop_table_age.RData")
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/prop_table_country.RData")
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/prop_table_education.RData")
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/prop_table_ethnicity.RData")
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/prop_table_gender.RData")


#df = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_train.csv")
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

#load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task1.RData")

# -------------------------------------------------------------------------------------------------------------------
# Task 2
# -------------------------------------------------------------------------------------------------------------------

#df_dummies <- dummy_cols(df[, 5:9], remove_selected_columns = TRUE)
#df_dummies <- df_dummies[, c("gender_F", "age_18-22", "ethnicity_Middle Eastern", "ethnicity_other", 
#                             "ethnicity_Multiracial", "ethnicity_Black or African American", "education_Bachelor’s degree",
#                             "education_Doctorate","country_Algeria", "country_Canada", "country_Cyprus", 
#                             "country_Ireland", "country_Israel")]
#df <- cbind(df, df_dummies)

# -------------------------------------------------------------------------------------------------------------------
# Task 3
# -------------------------------------------------------------------------------------------------------------------

#df <- add_proportions(df,
#                      prop_table_age,
#                      prop_table_education,
#                      prop_table_gender,
#                      prop_table_ethnicity,
#                      prop_table_country)


#save(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task_2_3.RData")

load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task_2_3.RData")

# -------------------------------------------------------------------------------------------------------------------
# Task 4
# -------------------------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------------------------
# Task 5
# -------------------------------------------------------------------------------------------------------------------





