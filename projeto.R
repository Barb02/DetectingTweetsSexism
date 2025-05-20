
# Main - Creating a data set for modeling 

# -------------------------------------------------------------------------------------------------------------------
# Importing
# -------------------------------------------------------------------------------------------------------------------

library(fastDummies)
source("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/functions.R")

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

load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_mclust.RData")
load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/annotatorsummary.RData")

c <- annotator_df$cluster
annotator_df <- subset(annotator_df, select = -cluster)

df3 <- df[, 5:9]
# Defina os níveis originais (do dataset completo)
levels_gender <- levels(factor(annotator_summary$gender))
levels_country <- levels(factor(annotator_summary$country))
levels_ethnicity <- levels(factor(annotator_summary$ethnicity))
levels_education <- levels(factor(annotator_summary$education))
levels_age <- levels(factor(annotator_summary$age))

# Aplique esses níveis em df3 antes do model.matrix:
df3$gender <- factor(df3$gender, levels = levels_gender)
df3$country <- factor(df3$country, levels = levels_country)
df3$ethnicity <- factor(df3$ethnicity, levels = levels_ethnicity)
df3$education <- factor(df3$education, levels = levels_education)
df3$age <- factor(df3$age, levels = levels_age)

levels_list <- lapply(annotator_summary, function(x) {
  if (is.factor(x) || is.character(x)) {
    levels(as.factor(x))
  } else {
    NULL
  }
})
levels_list <- levels_list[!sapply(levels_list, is.null)]

t3 <- predict_hclust(df3, annotator_df, c, levels_list)

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





