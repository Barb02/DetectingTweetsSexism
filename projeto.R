
# Main - Creating a data set for modeling 

# -------------------------------------------------------------------------------------------------------------------
# Importing
# -------------------------------------------------------------------------------------------------------------------

library(fastDummies)
source("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/functions.R")
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/kmeans_model.RData")
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/annotatorsummary.RData")
source("/home/barbara/MDS/ATDS/DetectingTweetsSexism/functions.R")
load("/home/barbara/MDS/ATDS/DetectingTweetsSexism/variables/kmeans_model.RData")
load("/home/barbara/MDS/ATDS/DetectingTweetsSexism/variables/annotatorsummary.RData")

# For Train
df = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST2025_train.csv")
df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_train.csv")
df = read_csv("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/tables/EXIST2025_train.csv")

# For DVLABELED and Test
df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_dev_labeled.csv")
df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST_test_nolabel.csv")
df = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST2025_dev_labeled.csv")
df = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST_test_nolabel.csv")
names(df)[names(df) == "age_group"] <- "age"
names(df)[names(df) == "annotator"] <- "annotator_id"
names(df)[names(df) == "study_level"] <- "education"

# For all
names(df)[names(df) == "gender"] <- "gender.x"

# -------------------------------------------------------------------------------------------------------------------
# Task 1
# -------------------------------------------------------------------------------------------------------------------

df <- important_words(df, tweet_col = "tweet")
df <- coloc(df)
df <- sent_seq(df)
df <- stats_emot_sent(df)

# For labeled and train
names(df)[11:20] <- paste0("word_", names(df)[11:20])
names(df)[names(df) == "gender.x"] <- "gender"

# For test
names(df)[10:19] <- paste0("word_", names(df)[10:19])
names(df)[names(df) == "gender.x"] <- "gender"

#save(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task1_1.RData")
#save(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/dfval_after_task1.RData")

#write.csv(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/df_after_task1_1.csv", row.names = FALSE)
#write.csv(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/dfval_after_task1.csv", row.names = FALSE)

# -------------------------------------------------------------------------------------------------------------------
# Task 2
# -------------------------------------------------------------------------------------------------------------------

#load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task1_1.RData")
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/dfval_after_task1.RData")

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

# For train and dev labeled
df_dummies2 <- dummy_cols(df[, 42], remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# For test
df_dummies2 <- dummy_cols(df[, 28], remove_first_dummy = TRUE, remove_selected_columns = TRUE)

names(df_dummies2)[names(df_dummies2) == ".data_2"] <- "cluster2"
names(df_dummies2)[names(df_dummies2) == ".data_3"] <- "cluster3"
names(df_dummies2)[names(df_dummies2) == ".data_4"] <- "cluster4"

df <- cbind(df, df_dummies2)

#save(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task_2_3.RData")
#save(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/dfval_after_task_2_3.RData")

#write.csv(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/df_after_task_2_3.csv", row.names = FALSE)
#write.csv(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/dfval_after_task_2_3.csv", row.names = FALSE)

#save(df, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/dftest_after_task_2_3.RData")

# -------------------------------------------------------------------------------------------------------------------
# Task 4
# -------------------------------------------------------------------------------------------------------------------

load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task_2_3.RData")
load("/home/barbara/MDS/ATDS/DetectingTweetsSexism/variables/df_after_task_2_3.RData")

df_train <- df
rm(df)

load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/dfval_after_task_2_3.RData")
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/dftest_after_task_2_3.RData")

df_val <- df
rm(df)

df_after_task4 <- compute_conf_column(df_train, df_train)
df_after_task4 <- subset(df_after_task4, select = -profile_key)

dfval_after_task4 <- compute_conf_column(df_train, df_val)

dftest_after_task4 <- compute_conf_column(df_train, df_val)
dftest_after_task4 <- subset(dftest_after_task4, select = -profile_key)

#write.csv(df_after_task4, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/trainfinal.csv", row.names = FALSE)
#write.csv(dfval_after_task4, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/valfinal.csv", row.names = FALSE)
#write.csv(dftest_after_task4, file = "C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/testfinal.csv", row.names = FALSE)




