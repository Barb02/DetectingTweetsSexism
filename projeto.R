
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

# For Train
#df = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df = read_csv("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/tables/EXIST2025_train.csv")

# For DVLABELED
df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_dev_labeled.csv")
names(df)[names(df) == "age_group"] <- "age"
names(df)[names(df) == "annotator"] <- "annotator_id"
names(df)[names(df) == "study_level"] <- "education"

# -------------------------------------------------------------------------------------------------------------------
# Task 1
# -------------------------------------------------------------------------------------------------------------------

# For train
#df <- df %>%
#  group_by(tweet) %>%
#  summarise(
#    count_yes = sum(label_task1_1 == "YES", na.rm = TRUE),
#    count_no = sum(label_task1_1 == "NO", na.rm = TRUE)
#  ) %>%
#  filter(count_yes != count_no) %>%
#  mutate(final_label = ifelse(count_yes > count_no, "YES", "NO"))
#df <- df %>%  select(tweet, final_label)

# For labeled
#names(df)[names(df) == "gender"] <- "gender.x"

#df <- important_words(df, tweet_col = "tweet")
#df <- coloc(df)
#df <- sent_seq(df)
#df <- stats_emot_sent(df)

# For train
#df <- df %>%
#  left_join(df, by = "tweet")

# For labeled
#names(df)[11:20] <- paste0("word_", names(df)[11:20])
#names(df)[names(df) == "gender.x"] <- "gender"

#For train
# -- Cut the lines that have NAs which are the lines that contain Yes = No 
#df <- na.omit(df)
#df <- subset(df, select = -final_label)
#names(df)[names(df) == "gender.x"] <- "gender"
#names(df)[11:20] <- paste0("word_", names(df)[11:20])
#names(df)[names(df) == "word_gender.y"] <- "word_gender"


#save(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task1.RData")

#load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task1.RData")

#save(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/dfval_after_task1.RData")

load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/dfval_after_task1.RData")

# -------------------------------------------------------------------------------------------------------------------
# Task 2
# -------------------------------------------------------------------------------------------------------------------

required_cols <- c("gender_F", "age_18-22", "ethnicity_Middle Eastern", "ethnicity_other", 
                   "ethnicity_Multiracial", "ethnicity_Black or African American", 
                   "education_Bachelor’s degree", "education_Doctorate",
                   "country_Algeria", "country_Canada", "country_Cyprus", 
                   "country_Ireland", "country_Israel")


df_dummies <- dummy_cols(df[, 5:9], remove_selected_columns = TRUE)
for (col in required_cols) {
  if (!(col %in% names(df_dummies))) {
    df_dummies[[col]] <- 0
  }
}
df_dummies <- df_dummies[, required_cols]

df <- cbind(df, df_dummies)

# -------------------------------------------------------------------------------------------------------------------
# Task 3
# -------------------------------------------------------------------------------------------------------------------

df <- add_proportions(df,
                      prop_table_age,
                      prop_table_education,
                      prop_table_gender,
                      prop_table_ethnicity,
                      prop_table_country)


#save(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task_2_3.RData")

#load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task_2_3.RData")

#save(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/dfval_after_task_2_3.RData")

#load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/dfval_after_task_2_3.RData")

#write.csv(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/trainfinal.csv", row.names = FALSE)
write.csv(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/valfinal.csv", row.names = FALSE)

# -------------------------------------------------------------------------------------------------------------------
# Task 4
# -------------------------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------------------------
# Task 5
# -------------------------------------------------------------------------------------------------------------------





