
# Task 4: Association Rule Mining (Apriori Algorithm)

# -------------------------------------------------------------------------------------------------------------------
# Libraries
# -------------------------------------------------------------------------------------------------------------------

library(arules)
library(randomForest)
library(FSelectorRcpp)
library(ggplot2)



#load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task_2_3.RData")
load("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/variables/df_after_task_2_3.RData")

df_train <- df
rm(df)

load("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/variables/dfval_after_task_2_3.RData")

df_new <- df
rm(df)
#df_train <- df_train[, 5:42]
#df_new <- df_new[, 5:42]


write.csv(df_train, file = "C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/variables/df_after_task_2_3.csv", row.names = FALSE)
write.csv(df_new, file = "C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/variables/dfval_after_task_2_3.csv", row.names = FALSE)


# -------------------------------------------------------------------------------------------------------------------
# Association Rules
# -------------------------------------------------------------------------------------------------------------------
#str(df_train)

# Fix special characters in column names
#names(df_train) <- gsub("[’‘'`]", "", names(df_train))         # remove apostrophes/quotes
#names(df_train) <- gsub("[- ]", "_", names(df_train))          # dashes & spaces → _
#names(df_train) <- make.names(names(df_train), unique = TRUE)  # make valid R names

#df_train$label_task1_1 <- as.factor(df_train$label_task1_1)




#==============
# prof
#==============

# STEP 1: Create demographic profile key
df_train$profile_key <- apply(df_train[, c("gender", "age", "ethnicity", "education", "country")], 1, paste, collapse = "|")
df_train$profile_key <- as.character(df_train$profile_key)  # ensure it's not a factor

# STEP 2: Count YES and total occurrences for each profile
profile_yes <- table(df_train$profile_key[df_train$label_task1_1 == "YES"])
profile_total <- table(df_train$profile_key)

# STEP 3: Compute empirical confidence
df_train$Conf <- mapply(function(key) {
  yes_count <- ifelse(!is.na(profile_yes[key]), profile_yes[key], 0)
  total_count <- profile_total[key]
  round(yes_count / total_count, 4)
}, df_train$profile_key)

# Optional: preview a few examples
head(df_train[, c("gender", "age", "ethnicity", "education", "country", "label_task1_1", "Conf")])




sum(is.na(df_train$Conf))




# FINALLLLLLLLLLLLLLLLLLLl=====================================================



library(caret)



compute_conf_column <- function(df_train_train, df_train_new) {
  # Demographic features to use
  demog_cols <- c("gender", "age", "ethnicity", "education", "country")
  
  # STEP 1: Compute empirical Conf from training set
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
  
  # STEP 2: Create profile_key for new dataset
  df_train_new$profile_key <- apply(df_train_new[, demog_cols], 1, paste, collapse = "|")
  df_train_new$profile_key <- as.character(df_train_new$profile_key)
  
  # STEP 3: Assign Conf for exact profile matches
  df_train_new$Conf <- ifelse(df_train_new$profile_key %in% names(profile_conf),
                        profile_conf[df_train_new$profile_key],
                        NA)
  
  # STEP 4: Fallback for unknown profiles using Euclidean similarity
  if (any(is.na(df_train_new$Conf))) {
    # One-hot encode demographic features
    dummies <- dummyVars(~ ., data = df_train_train[, demog_cols])
    train_matrix <- predict(dummies, newdata = df_train_train[, demog_cols])
    new_matrix <- predict(dummies, newdata = df_new[, demog_cols])
    new_matrix <- as.data.frame(new_matrix)
    train_matrix <- as.data.frame(train_matrix)
    
    # Align columns manually
    missing_cols <- setdiff(colnames(train_matrix), colnames(new_matrix))
    for (col in missing_cols) {
      new_matrix[[col]] <- 0  # fill missing cols with 0s
    }
    new_matrix <- new_matrix[, colnames(train_matrix)]  # reorder to match
    
    
    # Keep only rows with known Conf
    train_conf <- df_train_train$profile_key
    train_conf_values <- sapply(train_conf, function(k) profile_conf[[k]])
    
    known_idx <- which(!is.na(train_conf_values))
    unknown_idx <- which(is.na(df_train_new$Conf))
    
    known_matrix <- train_matrix[known_idx, ]
    unknown_matrix <- new_matrix[unknown_idx, ]
    
    # Euclidean distance
    dist_mat <- as.matrix(dist(rbind(unknown_matrix, known_matrix)))
    n_unknown <- nrow(unknown_matrix)
    d <- dist_mat[1:n_unknown, (n_unknown + 1):nrow(dist_mat)]
    
    # Nearest neighbor Conf assignment
    nearest_idx <- apply(d, 1, which.min)
    df_train_new$Conf[unknown_idx] <- train_conf_values[known_idx][nearest_idx]
  }
  
  return(df_train_new)
}

#TESTAR

# Suppose df_train has label_task1_1 and df_new does not
df_new_with_conf <- compute_conf_column(df_train, df_new)

head(df_new_with_conf)

sum(is.na(df_new_with_conf$Conf))





















#-------------------------------------------

#=========================
# Just whith clusters
#==========================

# Select relevant columns and ensure correct types
df_train_rules <- df_train[, c("clustered_data.cluster", "label_task1_1")]
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

# cluster 4 = NO
# cluster 5 = N0











# ==============================
#DEFINE PRESELECTED FEATURES
# ==============================
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

# ==============================
# PREPARE DATA
# ==============================

# For Association Rule Mining (ARM)
df_train_arm <- df_train
binary_cols <- c(
  "word_woman", "word_women", "word_men", "word_girl", "word_sex",
  "word_bitch", "word_fuck", "word_love", "word_peopl", "word_gender",
  "colloc_yes", "colloc_no", "gender_F", "age_18_22",
  "ethnicity_Middle_Eastern", "ethnicity_other", "ethnicity_Multiracial",
  "ethnicity_Black_or_African_American", "education_Bachelors_degree",
  "education_Doctorate", "country_Algeria", "country_Canada", "country_Cyprus",
  "country_Ireland", "country_Israel"
)
df_train_arm[binary_cols] <- lapply(df_train_arm[binary_cols], function(x) factor(ifelse(x == 1, "yes", "no")))

# Discretize continuous variables
df_train_arm$tweet_sentiment <- ifelse(df_train$tweet_sentiment > median(df_train$tweet_sentiment), "sentiment_high", "sentiment_low")
df_train_arm$all_pos         <- ifelse(df_train$all_pos > median(df_train$all_pos), "pos_high", "pos_low")
df_train_arm$all_neg         <- ifelse(df_train$all_neg > median(df_train$all_neg), "neg_high", "neg_low")
df_train_arm$sadness         <- ifelse(df_train$sadness > median(df_train$sadness), "sadness_high", "sadness_low")
df_train_arm$sent_min        <- ifelse(df_train$sent_min > median(df_train$sent_min), "sent_min_high", "sent_min_low")
df_train_arm$disgust_max     <- ifelse(df_train$disgust_max > median(df_train$disgust_max), "disgust_high", "disgust_low")

#------------------------------------------------
df_train_arm <- df_train_arm[, c(preselected, "label_task1_1")]
names(df_train_arm) <- make.names(names(df_train_arm), unique = TRUE)

# For Modeling (Random Forest / Info Gain)
df_train_model <- df_train[, c(preselected, "label_task1_1")]
df_train_model$label_task1_1 <- as.factor(df_train_model$label_task1_1)
names(df_train_model) <- make.names(names(df_train_model), unique = TRUE)

# ==============================
# RANDOM FOREST (Option 2)
# ==============================
set.seed(123)
rf_model <- randomForest(label_task1_1 ~ ., data = df_train_model, ntree = 100, importance = TRUE)
rf_importance <- importance(rf_model)[, "MeanDecreaseGini"]
rf_top <- sort(rf_importance, decreasing = TRUE)[1:15]
rf_top_features <- names(rf_top)

# ==============================
# INFORMATION GAIN (Option 3)
# ==============================
info_gain <- information_gain(label_task1_1 ~ ., df_train_model)
info_gain <- info_gain[order(-info_gain$importance), , drop = FALSE]
info_gain_top <- info_gain[order(-info_gain$importance), ][1:15, ]
info_gain_top_features <- rownames(info_gain_top)

# ==============================
# OUTPUT & PLOTS
# ==============================

cat("\n Top 15 Features by Random Forest:\n")
print(rf_top)

cat("\n Top 15 Features by Information Gain:\n")
print(info_gain_top)

common_features <- intersect(rf_top_features, info_gain_top_features)
cat("\n features Common to Both Methods:\n")
print(common_features)

# Random Forest plot
rf_plot_df_train <- data.frame(
  Feature = names(rf_top),
  Importance = as.numeric(rf_top)
)

ggplot(rf_plot_df_train, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Features by Random Forest", x = "Feature", y = "MeanDecreaseGini")

# Information Gain plot
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


# ==============================
# Combine Top Features (UNION + INTERSECT)
# ==============================

# Use actual named attributes from info gain
info_gain_top_features <- info_gain_top$attributes  # Fixes "X1", "X2", etc.

# ---------- UNION strategy ----------------------------------------------
final_features_union <- union(rf_top_features, info_gain_top_features)
final_features_union <- make.names(final_features_union, unique = TRUE)
final_features_union <- intersect(final_features_union, colnames(df_train_arm))
if (!"label_task1_1" %in% final_features_union) {
  final_features_union <- c(final_features_union, "label_task1_1")
}

# Subset ARM dataset
df_train_arm_union <- df_train_arm[, final_features_union]

# Convert to transactions
# converts every column in df_train_arm_final into a factor
df_train_arm_union[] <- lapply(df_train_arm_union, as.factor)
trans_union <- as(df_train_arm_union, "transactions")

# Apriori Algorithm
rules_union <- apriori(trans_union,
                       parameter = list(supp = 0.05, conf = 0.75, maxlen = 4))

# Filter for 'label_task1_1=YES' and lift > 1
rules_union_yes <- subset(rules_union, rhs %in% "label_task1_1=YES" & confidence > 0.75 & lift > 1)

# Top rules by LIFT
rules_union_lift <- sort(rules_union_yes, by = "lift", decreasing = TRUE)
cat("\n Top 10 Rules (UNION features, sorted by LIFT):\n")
if (length(rules_union_lift) > 0) inspect(head(rules_union_lift, 10)) else cat("No strong rules found (LIFT).\n")

# Top rules by COUNT
rules_union_count <- sort(rules_union_yes, by = "count", decreasing = TRUE)
cat("\n Top 10 Rules (UNION features, sorted by COUNT):\n")
if (length(rules_union_count) > 0) inspect(head(rules_union_count, 10)) else cat("No strong rules found (COUNT).\n")

# ---------- INTERSECT strategy -----------------------------------
final_features_intersect <- intersect(rf_top_features, info_gain_top_features)
final_features_intersect <- make.names(final_features_intersect, unique = TRUE)
final_features_intersect <- intersect(final_features_intersect, colnames(df_train_arm))
if (!"label_task1_1" %in% final_features_intersect) {
  final_features_intersect <- c(final_features_intersect, "label_task1_1")
}

# Subset ARM dataset
df_train_arm_intersect <- df_train_arm[, final_features_intersect]

# Convert to transactions
# converts every column in df_train_arm_final into a factor
df_train_arm_intersect[] <- lapply(df_train_arm_intersect, as.factor)
trans_intersect <- as(df_train_arm_intersect, "transactions")

# Apriori Algorithm
rules_intersect <- apriori(trans_intersect,
                           parameter = list(supp = 0.05, conf = 0.75, maxlen = 4))

# Filter for 'label_task1_1=YES' and lift > 1
rules_intersect_yes <- subset(rules_intersect, rhs %in% "label_task1_1=YES" & confidence > 0.75 & lift > 1)

# Top rules by LIFT
rules_intersect_lift <- sort(rules_intersect_yes, by = "lift", decreasing = TRUE)
cat("\n Top 10 Rules (INTERSECT features, sorted by LIFT):\n")
if (length(rules_intersect_lift) > 0) inspect(head(rules_intersect_lift, 10)) else cat("No strong rules found (LIFT).\n")

# Top rules by COUNT
rules_intersect_count <- sort(rules_intersect_yes, by = "count", decreasing = TRUE)
cat("\n Top 10 Rules (INTERSECT features, sorted by COUNT):\n")
if (length(rules_intersect_count) > 0) inspect(head(rules_intersect_count, 10)) else cat("No strong rules found (COUNT).\n")

#tried with rules_no but there weren't any rules ("default" label, more varience)




#====================================
# cluster + our personalized features
#====================================

# Remove demographic and original label columns
exclude_cols <- c("gender", "age", "ethnicity", "education", "colloc_yes", "colloc_no", "word_women", "word_men", "country", "label_task1_1")
df_train_arm_filtered <- df_train_arm[, !(names(df_train_arm) %in% exclude_cols)]

# Add the label back just for rule mining
df_train_arm_filtered$label <- as.factor(df_train$label_task1_1)

# Ensure everything is a factor
df_train_arm_filtered[] <- lapply(df_train_arm_filtered, as.factor)

# Convert to transactions
trans <- as(df_train_arm_filtered, "transactions")

# Run Apriori algorithm
rules <- apriori(trans, parameter = list(supp = 0.01, conf = 0.7, target = "rules"))

# Filter: rules that predict label YES or NO
label_rules <- subset(rules, rhs %in% c("label=YES", "label=NO"))

# Sort and inspect
inspect(sort(label_rules, by = "confidence", decreasing = TRUE)[1:20])




#_____________________________________________________________



# ==============================
# COLL0C_YES Impact Check
# ==============================


# PART 2 — Run Apriori without colloc_yes
cat("\n Now discovering rules with colloc_yes removed...\n")

# Remove colloc_yes from the preselected features
preselected_no_colloc <- setdiff(preselected, c("colloc_yes", "word_women"))

# Prepare a fresh df_train_arm without colloc_yes
df_train_arm_nocolloc <- df_train

# Convert relevant binary features
binary_cols_nocolloc <- intersect(preselected_no_colloc, colnames(df_train_arm_nocolloc))[1:29]
df_train_arm_nocolloc[binary_cols_nocolloc] <- lapply(df_train_arm_nocolloc[binary_cols_nocolloc], function(x) factor(ifelse(x == 1, "yes", "no")))

# Discretize numeric variables again
df_train_arm_nocolloc$tweet_sentiment <- ifelse(df_train$tweet_sentiment > median(df_train$tweet_sentiment), "sentiment_high", "sentiment_low")
df_train_arm_nocolloc$sadness         <- ifelse(df_train$sadness > median(df_train$sadness), "sadness_high", "sadness_low")
df_train_arm_nocolloc$sent_min        <- ifelse(df_train$sent_min > median(df_train$sent_min), "sent_min_high", "sent_min_low")
df_train_arm_nocolloc$disgust_max     <- ifelse(df_train$disgust_max > median(df_train$disgust_max), "disgust_high", "disgust_low")

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




#________________________________________________________________________________________

#===========================
#BASIC
#===========================
# ==============================
# Step 1: Select Original Annotator Demographic Features
# ==============================
basic_features <- c("gender", "age", "ethnicity", "education", "country", "label_task1_1")

# ==============================
# Step 2: Subset and Prepare Dataset
# ==============================
df_train_basic <- df_train[, basic_features]

# Convert all columns to factors
df_train_basic[] <- lapply(df_train_basic, as.factor)

# Clean column names for safety
names(df_train_basic) <- make.names(names(df_train_basic), unique = TRUE)

# ==============================
# Step 3: Convert to Transactions
# ==============================
trans_basic <- as(df_train_basic, "transactions")

# ==============================
# Step 4: Run Apriori Algorithm
# ==============================
rules_basic <- apriori(trans_basic,
                       parameter = list(supp = 0.01, conf = 0.5, minlen = 3))

# ==============================
# Step 5: Filter Rules for label_task1_1 = YES
# ==============================
rules_basic_yes <- subset(rules_basic, rhs %in% "label_task1_1=YES" & lift > 1)
rules_basic_yes <- sort(rules_basic_yes, by = "lift", decreasing = TRUE)

# ==============================
# Step 6: Inspect Top Rules
# ==============================
cat("\n Top 10 Rules (Demographics only, sorted by LIFT):\n")
if (length(rules_basic_yes) > 0) {
  inspect(head(rules_basic_yes, 10))
} else {
  cat(" No strong rules found for label_task1_1 = YES.\n")
}

# ==============================
# Step 7: (Optional) Also Show by COUNT
# ==============================
rules_basic_yes_count <- sort(rules_basic_yes, by = "count", decreasing = TRUE)
cat("\n Top 10 Rules (Demographics only, sorted by COUNT):\n")
if (length(rules_basic_yes_count) > 0) {
  inspect(head(rules_basic_yes_count, 10))
} else {
  cat(" No strong rules by count.\n")
}

#-------------------------------------------------------------------------------------------

#======================
# Nao me lembro
#======================

df_train[binary_cols] <- lapply(df_train[binary_cols], function(x) factor(ifelse(x == 1, "yes", "no")))


# Convert label to factor
df_train$label_task1_1 <- as.factor(df_train$label_task1_1)

# Subset relevant features for Apriori
library(arules)
selected <- c(binary_cols, "tweet_sentiment", "all_pos", "sadness", "sent_min", "disgust_max", "label_task1_1")
df_train_apriori <- df_train[, selected]

# Convert to transaction format
trans <- as(df_train_apriori, "transactions")

# Run Apriori algorithm
rules <- apriori(trans, parameter = list(supp = 0.02, conf = 0.6))

# View top rules by lift
rules_sorted <- sort(rules, by = "lift", decreasing = TRUE)
inspect(rules_sorted[1:15])



#---------------------------------------------------------------------

# ==============================
# Combined Annotator + Tweet Features (LABEL = NO)
# ==============================


# Step 1: Define full feature set (annotator + tweet)
combined_features <- c(
  "gender", "age", "ethnicity", "education", "country",
  "word_woman", "word_women", "word_men", "word_girl", "word_sex",
  "word_bitch", "word_fuck", "word_love", "word_peopl", "word_gender",
  "colloc_yes", "colloc_no",
  "all_pos", "all_neg", "tweet_sentiment",
  "sadness", "sent_min", "disgust_max",
  "label_task1_1"
)

# Step 2: Subset and prepare data
df_train_combined <- df_train[, combined_features]

# Binary tweet columns to convert
binary_tweet_cols <- c(
  "word_woman", "word_women", "word_men", "word_girl", "word_sex",
  "word_bitch", "word_fuck", "word_love", "word_peopl", "word_gender",
  "colloc_yes", "colloc_no"
)
df_train_combined[binary_tweet_cols] <- lapply(df_train_combined[binary_tweet_cols], function(x) factor(ifelse(x == 1, "yes", "no")))

# Discretize numeric features
df_train_combined$tweet_sentiment <- ifelse(df_train$tweet_sentiment > median(df_train$tweet_sentiment), "sentiment_high", "sentiment_low")
df_train_combined$all_pos         <- ifelse(df_train$all_pos > median(df_train$all_pos), "pos_high", "pos_low")
df_train_combined$all_neg         <- ifelse(df_train$all_neg > median(df_train$all_neg), "neg_high", "neg_low")
df_train_combined$sadness         <- ifelse(df_train$sadness > median(df_train$sadness), "sadness_high", "sadness_low")
df_train_combined$sent_min        <- ifelse(df_train$sent_min > median(df_train$sent_min), "sent_min_high", "sent_min_low")
df_train_combined$disgust_max     <- ifelse(df_train$disgust_max > median(df_train$disgust_max), "disgust_high", "disgust_low")

# Convert all to factors
df_train_combined[] <- lapply(df_train_combined, as.factor)
names(df_train_combined) <- make.names(names(df_train_combined), unique = TRUE)

trans_all <- as(df_train_combined, "transactions")

rules_all_no <- apriori(trans_all,
                        parameter = list(supp = 0.02, conf = 0.7, maxlen = 4))

rules_all_no <- subset(rules_all_no, rhs %in% "label_task1_1=NO" & lift > 1)
rules_all_no <- sort(rules_all_no, by = "lift", decreasing = TRUE)

cat("\n Top 10 Rules WITH colloc_yes (label = NO):\n")
if (length(rules_all_no) > 0) {
  inspect(head(rules_all_no, 10))
} else {
  cat(" No strong rules found (with colloc_yes).\n")
}
# O colloc=yes apenas aparece na primeira regra para NO


#---------------------------------------------------------------------
# ==============================
# Combined Annotator + Tweet Features (LABEL = YES)
# ==============================

# Step 1: Define full feature set (annotator + tweet)
combined_features <- c(
  "gender", "age", "ethnicity", "education", "country",
  "word_woman", "word_women", "word_men", "word_girl", "word_sex",
  "word_bitch", "word_fuck", "word_love", "word_peopl", "word_gender",
  "colloc_yes", "colloc_no",
  "all_pos", "all_neg", "tweet_sentiment",
  "sadness", "sent_min", "disgust_max",
  "label_task1_1"
)

# Step 2: Subset and prepare data
df_train_combined <- df_train[, combined_features]

# Convert binary tweet columns to yes/no
binary_tweet_cols <- c(
  "word_woman", "word_women", "word_men", "word_girl", "word_sex",
  "word_bitch", "word_fuck", "word_love", "word_peopl", "word_gender",
  "colloc_yes", "colloc_no"
)
df_train_combined[binary_tweet_cols] <- lapply(df_train_combined[binary_tweet_cols], function(x) factor(ifelse(x == 1, "yes", "no")))

# Discretize numeric variables
df_train_combined$tweet_sentiment <- ifelse(df_train$tweet_sentiment > median(df_train$tweet_sentiment), "sentiment_high", "sentiment_low")
df_train_combined$all_pos         <- ifelse(df_train$all_pos > median(df_train$all_pos), "pos_high", "pos_low")
df_train_combined$all_neg         <- ifelse(df_train$all_neg > median(df_train$all_neg), "neg_high", "neg_low")
df_train_combined$sadness         <- ifelse(df_train$sadness > median(df_train$sadness), "sadness_high", "sadness_low")
df_train_combined$sent_min        <- ifelse(df_train$sent_min > median(df_train$sent_min), "sent_min_high", "sent_min_low")
df_train_combined$disgust_max     <- ifelse(df_train$disgust_max > median(df_train$disgust_max), "disgust_high", "disgust_low")

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

