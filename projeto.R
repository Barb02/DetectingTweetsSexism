
# Main - Creating a data set for modeling 

# -------------------------------------------------------------------------------------------------------------------
# Importing
# -------------------------------------------------------------------------------------------------------------------

library(fastDummies)
source("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/functions.R")
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/kmeans_model.RData")
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/annotatorsummary.RData")

# For Train
#df = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df = read_csv("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/tables/EXIST2025_train.csv")

# For DVLABELED
#df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_dev_labeled.csv")
#names(df)[names(df) == "age_group"] <- "age"
#names(df)[names(df) == "annotator"] <- "annotator_id"
#names(df)[names(df) == "study_level"] <- "education"

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

#load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/dfval_after_task1.RData")

#write.csv(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/dftrain_after_task1.csv", row.names = FALSE)

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

clustered_data <- predict_cluster_kmeans(df, kmeans_result, annotator_summary)
print(clustered_data)

df <- cbind(df, clustered_data$cluster)

df_dummies2 <- dummy_cols(df[, 42], remove_first_dummy = TRUE, remove_selected_columns = TRUE)
names(df_dummies2)[names(df_dummies2) == ".data_2"] <- "cluster2"
names(df_dummies2)[names(df_dummies2) == ".data_3"] <- "cluster3"
names(df_dummies2)[names(df_dummies2) == ".data_4"] <- "cluster4"

df <- cbind(df, df_dummies2)

#save(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task_2_3.RData")

#load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task_2_3.RData")

#save(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/dfval_after_task_2_3.RData")

#load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/dfval_after_task_2_3.RData")

#write.csv(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/df_after_task_2_3.csv", row.names = FALSE)


# -------------------------------------------------------------------------------------------------------------------
# Task 4
# -------------------------------------------------------------------------------------------------------------------

# Using the rule confidence of cluster X -> YES label, we created a new collumn 

names(df)[names(df) == "clustered_data$cluster"] <- "cluster"

df$ru_conf <- ifelse(df$cluster == 1, 0.4363751,
                        ifelse(df$cluster == 2, 0.4238484,
                        ifelse(df$cluster == 3, 0.3949825,
                        ifelse(df$cluster == 4, 0.3722572, NA))))

df <- df[, !(names(df) %in% c("cluster", "cluster2", "cluster3", "cluster4"))]

#write.csv(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/trainfinal.csv", row.names = FALSE)
#write.csv(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/valfinal.csv", row.names = FALSE)




