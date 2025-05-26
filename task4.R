
# Task 4: Association Rule Mining (Apriori Algorithm)

# -------------------------------------------------------------------------------------------------------------------
# Libraries
# -------------------------------------------------------------------------------------------------------------------

library(arules)
library(randomForest)
library(FSelectorRcpp)
library(ggplot2)
library(caret)



#load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task_2_3.RData")
load("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/variables/df_after_task_2_3.RData")

df_train_train <- df
rm(df)

load("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/variables/dfval_after_task_2_3.RData")

df_train_new <- df
rm(df)

str(df_train_train)


#--------------------------------------------------------------------------------------------------------------------
# Function to assign confidence based on characteristics of the annotator
#--------------------------------------------------------------------------------------------------------------------

# For a new annotator there are 2 possibilities:
# 1. If the demographics are the same as a previously know annotator, then assign the same confidence
# 2. Otherwise, assign the confidence by calculating the euclidean distance between the new annotator demographics and the known demographics.

compute_conf_column <- function(df_train_train, df_train_new) {
  demog_cols <- c("gender", "age", "ethnicity", "education", "country")
  
  df_train_train$profile_key <- apply(df_train_train[, demog_cols], 1, paste, collapse = "|")
  df_train_train$profile_key <- as.character(df_train_train$profile_key)
  
  profile_yes <- table(df_train_train$profile_key[df_train_train$label_task1_1 == "YES"])
  profile_total <- table(df_train_train$profile_key)
  
  profile_conf <- mapply(function(key) {
    yes_count <- ifelse(!is.na(profile_yes[key]), profile_yes[key], 0)
    total_count <- profile_total[key]
    round(yes_count / total_count, 4)
  }, names(profile_total))
  names(profile_conf) <- names(profile_total)
  
  df_train_new$profile_key <- apply(df_train_new[, demog_cols], 1, paste, collapse = "|")
  df_train_new$profile_key <- as.character(df_train_new$profile_key)
  
  df_train_new$Conf <- ifelse(df_train_new$profile_key %in% names(profile_conf),
                              profile_conf[df_train_new$profile_key],
                              NA)
  
  if (any(is.na(df_train_new$Conf))) {
    dummies <- dummyVars(~ ., data = df_train_train[, demog_cols])
    train_matrix <- predict(dummies, newdata = df_train_train[, demog_cols])
    new_matrix <- predict(dummies, newdata = df_train_new[, demog_cols])  # <- CORRIGIDO
    new_matrix <- as.data.frame(new_matrix)
    train_matrix <- as.data.frame(train_matrix)
    
    missing_cols <- setdiff(colnames(train_matrix), colnames(new_matrix))
    for (col in missing_cols) {
      new_matrix[[col]] <- 0
    }
    new_matrix <- new_matrix[, colnames(train_matrix)]
    
    train_conf <- df_train_train$profile_key
    train_conf_values <- sapply(train_conf, function(k) profile_conf[[k]])
    
    known_idx <- which(!is.na(train_conf_values))
    unknown_idx <- which(is.na(df_train_new$Conf))
    
    known_matrix <- train_matrix[known_idx, ]
    unknown_matrix <- new_matrix[unknown_idx, ]
    
    dist_mat <- as.matrix(dist(rbind(unknown_matrix, known_matrix)))
    n_unknown <- nrow(unknown_matrix)
    d <- dist_mat[1:n_unknown, (n_unknown + 1):nrow(dist_mat)]
    
    nearest_idx <- apply(d, 1, which.min)
    df_train_new$Conf[unknown_idx] <- train_conf_values[known_idx][nearest_idx]
  }
  
  return(df_train_new)
}


# Test the function
df_new_with_conf <- compute_conf_column(df_train_train, df_train_new)
head(df_new_with_conf)
sum(is.na(df_new_with_conf$Conf))



# -------------------------------------------------------------------------------------------------------------------
# Association Rules (AR)
# -------------------------------------------------------------------------------------------------------------------

df_train_train <- df_train_train[, 5:42]
#df_train_new <- df_new[, 5:42]


# Fix special characters in column names
names(df_train_train) <- gsub("[’‘'`]", "", names(df_train_train))         # remove apostrophes/quotes
names(df_train_train) <- gsub("[- ]", "_", names(df_train_train))          # dashes & spaces → _
names(df_train_train) <- make.names(names(df_train_train), unique = TRUE)  # make valid R names

df_train_train$label_task1_1 <- as.factor(df_train_train$label_task1_1)


#=============================
# AR using only the clusters
#=============================

# Select relevant columns and ensure correct types
df_train_rules <- df_train_train[, c("clustered_data.cluster", "label_task1_1")]
colnames(df_train_rules)[colnames(df_train_rules) == "clustered_data.cluster"] <- "cluster"

# Convert to factors
df_train_rules[] <- lapply(df_train_rules, as.factor)

# Convert to transactions
trans <- as(df_train_rules, "transactions")

# Run Apriori: lower thresholds to catch even weak cluster rules
rules <- apriori(trans, parameter = list(supp = 0.01, conf = 0.1, target = "rules"))

# Filter: only rules where LHS is cluster and RHS is label
cluster_rules <- subset(rules, 
                        lhs %pin% "cluster=" & 
                          rhs %in% c("label_task1_1=YES", "label_task1_1=NO"))

# Show all available cluster → label rules, or as many as exist
sorted_cluster_rules <- sort(cluster_rules, by = "confidence", decreasing = TRUE)
inspect(head(sorted_cluster_rules, n = min(10, length(sorted_cluster_rules))))

# clusters 3 and 4 stand out slightly for labeling tweets as not sexist.
# Clusters 1, 2 are not strongly polarized.



#=================================================
# Feature importance on our personalized features
#=================================================

#Define preselected features
preselected <- c(
  "word_woman", "word_women", "word_men", "word_girl", "word_sex",
  "word_bitch", "word_fuck", "word_love", "word_peopl", "word_gender",
  "colloc_yes", "colloc_no", "all_pos", "all_neg", "tweet_sentiment",
  "sadness", "sent_min", "disgust_max", "gender_F", "age_18_22",
  "ethnicity_Middle_Eastern", "ethnicity_other", "ethnicity_Multiracial",
  "ethnicity_Black_or_African_American", "education_Bachelors_degree",
  "education_Doctorate", "country_Algeria", "country_Canada", "country_Cyprus",
  "country_Ireland", "country_Israel"
)

# For Association Rule Mining (ARM)
df_train_arm <- df_train_train
binary_cols <- c(
  "word_woman", "word_women", "word_men", "word_girl", "word_sex",
  "word_bitch", "word_fuck", "word_love", "word_peopl", "word_gender",
  "colloc_yes", "colloc_no", "gender_F", "age_18_22", "all_pos", "all_neg",
  "ethnicity_Middle_Eastern", "ethnicity_other", "ethnicity_Multiracial",
  "ethnicity_Black_or_African_American", "education_Bachelors_degree",
  "education_Doctorate", "country_Algeria", "country_Canada", "country_Cyprus",
  "country_Ireland", "country_Israel"
)
df_train_arm[binary_cols] <- lapply(df_train_arm[binary_cols], function(x) factor(ifelse(x == 1, "yes", "no")))

# Discretize continuous variables
df_train_arm$tweet_sentiment <- ifelse(df_train_train$tweet_sentiment > median(df_train_train$tweet_sentiment), "sentiment_high", "sentiment_low")
df_train_arm$sadness         <- ifelse(df_train_train$sadness > median(df_train_train$sadness), "sadness_high", "sadness_low")
df_train_arm$sent_min        <- ifelse(df_train_train$sent_min > median(df_train_train$sent_min), "sent_min_high", "sent_min_low")
df_train_arm$disgust_max     <- ifelse(df_train_train$disgust_max > median(df_train_train$disgust_max), "disgust_high", "disgust_low")

df_train_arm <- df_train_arm[, c(preselected, "label_task1_1")]
names(df_train_arm) <- make.names(names(df_train_arm), unique = TRUE)

# For Modeling Information Gain
df_train_model <- df_train_train[, c(preselected, "label_task1_1")]
df_train_model$label_task1_1 <- as.factor(df_train_model$label_task1_1)
names(df_train_model) <- make.names(names(df_train_model), unique = TRUE)


info_gain <- information_gain(label_task1_1 ~ ., df_train_model)
info_gain <- info_gain[order(-info_gain$importance), , drop = FALSE]
info_gain_top <- info_gain[order(-info_gain$importance), ][1:15, ]
info_gain_top_features <- rownames(info_gain_top)


cat("\n Top 15 Features by Information Gain:\n")
print(info_gain_top)


# plot
info_plot_df_train <- data.frame(
  Feature = info_gain_top$attributes,
  Importance = info_gain_top$importance
)

# Make sure the plot uses feature names, ordered by importance
info_plot_df_train$Feature <- factor(info_plot_df_train$Feature, levels = info_plot_df_train$Feature[order(info_plot_df_train$Importance)])

# Plot
ggplot(info_plot_df_train, aes(x = Feature, y = Importance)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 15 Features by Information Gain", x = "Feature", y = "Information Gain") +
  theme_minimal()



# =========================
# COLLOC_YES Impact Check
# =========================

# Run Apriori without colloc_yes
cat("\n Now discovering rules with colloc_yes removed...\n")

# Remove colloc_yes from the preselected features
preselected_no_colloc <- setdiff(preselected, "colloc_yes")

# Prepare a fresh df_train_arm without colloc_yes
df_train_arm_nocolloc <- df_train_train

# Convert relevant binary features
binary_cols_nocolloc <- intersect(preselected_no_colloc, colnames(df_train_arm_nocolloc))[1:29]
df_train_arm_nocolloc[binary_cols_nocolloc] <- lapply(df_train_arm_nocolloc[binary_cols_nocolloc], function(x) factor(ifelse(x == 1, "yes", "no")))

# Discretize numeric variables again
df_train_arm_nocolloc$tweet_sentiment <- ifelse(df_train_train$tweet_sentiment > median(df_train_train$tweet_sentiment), "sentiment_high", "sentiment_low")
df_train_arm_nocolloc$sadness         <- ifelse(df_train_train$sadness > median(df_train_train$sadness), "sadness_high", "sadness_low")
df_train_arm_nocolloc$sent_min        <- ifelse(df_train_train$sent_min > median(df_train_train$sent_min), "sent_min_high", "sent_min_low")
df_train_arm_nocolloc$disgust_max     <- ifelse(df_train_train$disgust_max > median(df_train_train$disgust_max), "disgust_high", "disgust_low")

# Subset relevant features
df_train_arm_nocolloc <- df_train_arm_nocolloc[, c(preselected_no_colloc, "label_task1_1")]
df_train_arm_nocolloc[] <- lapply(df_train_arm_nocolloc, as.factor)
names(df_train_arm_nocolloc) <- make.names(names(df_train_arm_nocolloc), unique = TRUE)

# Convert to transactions
trans_nocolloc <- as(df_train_arm_nocolloc, "transactions")

# Run Apriori
rules_nocolloc <- apriori(trans_nocolloc,
                          parameter = list(supp = 0.02, conf = 0.7, maxlen = 4))

# Filter and sort
rules_nocolloc_yes <- subset(rules_nocolloc, rhs %in% "label_task1_1=YES" & lift > 1)
rules_nocolloc_yes <- sort(rules_nocolloc_yes, by = "lift", decreasing = TRUE)

# Show results
cat("\n Top 10 Rules WITHOUT colloc_yes:\n")
if (length(rules_nocolloc_yes) > 0) {
  inspect(head(rules_nocolloc_yes, 10))
} else {
  cat("No strong rules found without colloc_yes.\n")
}

# By retrieving colloc_yes we observe that there is another feature that contributes to the high values of lift and confidence (word_women).
# However, when this variable is removed, the same pattern emerge (another variable dominate the rules).
# This means that these features are individually strong.



# =========================================================
# Combined annotator + personalized Features (LABEL = YES)
# =========================================================

# Define full feature set (annotator + personalized)
combined_features <- c(
  "gender", "age", "ethnicity", "education", "country",
  "word_woman", "word_women", "word_men", "word_girl", "word_sex",
  "word_bitch", "word_fuck", "word_love", "word_peopl", "word_gender",
  "colloc_yes", "colloc_no",
  "all_pos", "all_neg", "tweet_sentiment",
  "sadness", "sent_min", "disgust_max",
  "label_task1_1"
)

# Subset and prepare data
df_train_combined <- df_train_train[, combined_features]

# Convert binary tweet columns to yes/no
binary_tweet_cols <- c(
  "word_woman", "word_women", "word_men", "word_girl", "word_sex",
  "word_bitch", "word_fuck", "word_love", "word_peopl", "word_gender",
  "colloc_yes", "colloc_no", "all_pos", "all_neg"
)
df_train_combined[binary_tweet_cols] <- lapply(df_train_combined[binary_tweet_cols], function(x) factor(ifelse(x == 1, "yes", "no")))

# Discretize numeric variables
df_train_combined$tweet_sentiment <- ifelse(df_train_train$tweet_sentiment > median(df_train_train$tweet_sentiment), "sentiment_high", "sentiment_low")
df_train_combined$sadness         <- ifelse(df_train_train$sadness > median(df_train_train$sadness), "sadness_high", "sadness_low")
df_train_combined$sent_min        <- ifelse(df_train_train$sent_min > median(df_train_train$sent_min), "sent_min_high", "sent_min_low")
df_train_combined$disgust_max     <- ifelse(df_train_train$disgust_max > median(df_train_train$disgust_max), "disgust_high", "disgust_low")

# Convert all columns to factor
df_train_combined[] <- lapply(df_train_combined, as.factor)
names(df_train_combined) <- make.names(names(df_train_combined), unique = TRUE)

# Convert to transaction format
trans_all <- as(df_train_combined, "transactions")

# Mine rules
rules_all_yes <- apriori(trans_all,
                         parameter = list(supp = 0.02, conf = 0.7, maxlen = 4))

# Filter rules for label = YES
rules_all_yes <- subset(rules_all_yes, rhs %in% "label_task1_1=YES" & lift > 1)
rules_all_yes <- sort(rules_all_yes, by = "lift", decreasing = TRUE)

# Output
cat("\n Top 10 Rules (Combined features, label = YES):\n")
if (length(rules_all_yes) > 0) {
  inspect(head(rules_all_yes, 10))
} else {
  cat(" No strong rules found for label = YES.\n")
}

# Most of the rules have personalized features on the LHS (antecedent), while annotator features are rarely included.
# This confirms that they carry stronger, more direct signals for predicting labeling behavior than annotator demographics.
