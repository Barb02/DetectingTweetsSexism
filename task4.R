
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

df <- df[, 5:41]

# -------------------------------------------------------------------------------------------------------------------
# Association Rules
# -------------------------------------------------------------------------------------------------------------------
str(df)

# Fix special characters in column names
names(df) <- gsub("[’‘'`]", "", names(df))         # remove apostrophes/quotes
names(df) <- gsub("[- ]", "_", names(df))          # dashes & spaces → _
names(df) <- make.names(names(df), unique = TRUE)  # make valid R names

df$label_task1_1 <- as.factor(df$label_task1_1)


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
df_arm <- df
binary_cols <- c(
  "word_woman", "word_women", "word_men", "word_girl", "word_sex",
  "word_bitch", "word_fuck", "word_love", "word_peopl", "word_gender",
  "colloc_yes", "colloc_no", "gender_F", "age_18_22",
  "ethnicity_Middle_Eastern", "ethnicity_other", "ethnicity_Multiracial",
  "ethnicity_Black_or_African_American", "education_Bachelors_degree",
  "education_Doctorate", "country_Algeria", "country_Canada", "country_Cyprus",
  "country_Ireland", "country_Israel"
)
df_arm[binary_cols] <- lapply(df_arm[binary_cols], function(x) factor(ifelse(x == 1, "yes", "no")))

# Discretize continuous variables
df_arm$tweet_sentiment <- ifelse(df$tweet_sentiment > median(df$tweet_sentiment), "sentiment_high", "sentiment_low")
df_arm$all_pos         <- ifelse(df$all_pos > median(df$all_pos), "pos_high", "pos_low")
df_arm$all_neg         <- ifelse(df$all_neg > median(df$all_neg), "neg_high", "neg_low")
df_arm$sadness         <- ifelse(df$sadness > median(df$sadness), "sadness_high", "sadness_low")
df_arm$sent_min        <- ifelse(df$sent_min > median(df$sent_min), "sent_min_high", "sent_min_low")
df_arm$disgust_max     <- ifelse(df$disgust_max > median(df$disgust_max), "disgust_high", "disgust_low")

df_arm <- df_arm[, c(preselected, "label_task1_1")]
names(df_arm) <- make.names(names(df_arm), unique = TRUE)

# For Modeling (Random Forest / Info Gain)
df_model <- df[, c(preselected, "label_task1_1")]
df_model$label_task1_1 <- as.factor(df_model$label_task1_1)
names(df_model) <- make.names(names(df_model), unique = TRUE)

# ==============================
# RANDOM FOREST (Option 2)
# ==============================
set.seed(123)
rf_model <- randomForest(label_task1_1 ~ ., data = df_model, ntree = 100, importance = TRUE)
rf_importance <- importance(rf_model)[, "MeanDecreaseGini"]
rf_top <- sort(rf_importance, decreasing = TRUE)[1:15]
rf_top_features <- names(rf_top)

# ==============================
# INFORMATION GAIN (Option 3)
# ==============================
info_gain <- information_gain(label_task1_1 ~ ., df_model)
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
rf_plot_df <- data.frame(
  Feature = names(rf_top),
  Importance = as.numeric(rf_top)
)

ggplot(rf_plot_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Features by Random Forest", x = "Feature", y = "MeanDecreaseGini")

# Information Gain plot
info_plot_df <- data.frame(
  Feature = info_gain_top$attributes,
  Importance = info_gain_top$importance
)

# Make sure the plot uses feature names, ordered by importance
info_plot_df$Feature <- factor(info_plot_df$Feature, levels = info_plot_df$Feature[order(info_plot_df$Importance)])

# Plot
ggplot(info_plot_df, aes(x = Feature, y = Importance)) +
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
final_features_union <- intersect(final_features_union, colnames(df_arm))
if (!"label_task1_1" %in% final_features_union) {
  final_features_union <- c(final_features_union, "label_task1_1")
}

# Subset ARM dataset
df_arm_union <- df_arm[, final_features_union]

# Convert to transactions
# converts every column in df_arm_final into a factor
df_arm_union[] <- lapply(df_arm_union, as.factor)
trans_union <- as(df_arm_union, "transactions")

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
final_features_intersect <- intersect(final_features_intersect, colnames(df_arm))
if (!"label_task1_1" %in% final_features_intersect) {
  final_features_intersect <- c(final_features_intersect, "label_task1_1")
}

# Subset ARM dataset
df_arm_intersect <- df_arm[, final_features_intersect]

# Convert to transactions
# converts every column in df_arm_final into a factor
df_arm_intersect[] <- lapply(df_arm_intersect, as.factor)
trans_intersect <- as(df_arm_intersect, "transactions")

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


#_____________________________________________________________



# ==============================
# COLL0C_YES Impact Check
# ==============================


# PART 2 — Run Apriori without colloc_yes
cat("\n Now discovering rules with colloc_yes removed...\n")

# Remove colloc_yes from the preselected features
preselected_no_colloc <- setdiff(preselected, "colloc_yes")

# Prepare a fresh df_arm without colloc_yes
df_arm_nocolloc <- df

# Convert relevant binary features
binary_cols_nocolloc <- intersect(preselected_no_colloc, colnames(df_arm_nocolloc))[1:29]
df_arm_nocolloc[binary_cols_nocolloc] <- lapply(df_arm_nocolloc[binary_cols_nocolloc], function(x) factor(ifelse(x == 1, "yes", "no")))

# Discretize numeric variables again
df_arm_nocolloc$tweet_sentiment <- ifelse(df$tweet_sentiment > median(df$tweet_sentiment), "sentiment_high", "sentiment_low")
df_arm_nocolloc$all_pos         <- ifelse(df$all_pos > median(df$all_pos), "pos_high", "pos_low")
df_arm_nocolloc$all_neg         <- ifelse(df$all_neg > median(df$all_neg), "neg_high", "neg_low")
df_arm_nocolloc$sadness         <- ifelse(df$sadness > median(df$sadness), "sadness_high", "sadness_low")
df_arm_nocolloc$sent_min        <- ifelse(df$sent_min > median(df$sent_min), "sent_min_high", "sent_min_low")
df_arm_nocolloc$disgust_max     <- ifelse(df$disgust_max > median(df$disgust_max), "disgust_high", "disgust_low")

# Subset relevant features
df_arm_nocolloc <- df_arm_nocolloc[, c(preselected_no_colloc, "label_task1_1")]
df_arm_nocolloc[] <- lapply(df_arm_nocolloc, as.factor)
names(df_arm_nocolloc) <- make.names(names(df_arm_nocolloc), unique = TRUE)

# Convert to transactions
trans_nocolloc <- as(df_arm_nocolloc, "transactions")

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
df_basic <- df[, basic_features]

# Convert all columns to factors
df_basic[] <- lapply(df_basic, as.factor)

# Clean column names for safety
names(df_basic) <- make.names(names(df_basic), unique = TRUE)

# ==============================
# Step 3: Convert to Transactions
# ==============================
library(arules)
trans_basic <- as(df_basic, "transactions")

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

df[binary_cols] <- lapply(df[binary_cols], function(x) factor(ifelse(x == 1, "yes", "no")))


# Convert label to factor
df$label_task1_1 <- as.factor(df$label_task1_1)

# Subset relevant features for Apriori
library(arules)
selected <- c(binary_cols, "tweet_sentiment", "all_pos", "sadness", "sent_min", "disgust_max", "label_task1_1")
df_apriori <- df[, selected]

# Convert to transaction format
trans <- as(df_apriori, "transactions")

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
df_combined <- df[, combined_features]

# Binary tweet columns to convert
binary_tweet_cols <- c(
  "word_woman", "word_women", "word_men", "word_girl", "word_sex",
  "word_bitch", "word_fuck", "word_love", "word_peopl", "word_gender",
  "colloc_yes", "colloc_no"
)
df_combined[binary_tweet_cols] <- lapply(df_combined[binary_tweet_cols], function(x) factor(ifelse(x == 1, "yes", "no")))

# Discretize numeric features
df_combined$tweet_sentiment <- ifelse(df$tweet_sentiment > median(df$tweet_sentiment), "sentiment_high", "sentiment_low")
df_combined$all_pos         <- ifelse(df$all_pos > median(df$all_pos), "pos_high", "pos_low")
df_combined$all_neg         <- ifelse(df$all_neg > median(df$all_neg), "neg_high", "neg_low")
df_combined$sadness         <- ifelse(df$sadness > median(df$sadness), "sadness_high", "sadness_low")
df_combined$sent_min        <- ifelse(df$sent_min > median(df$sent_min), "sent_min_high", "sent_min_low")
df_combined$disgust_max     <- ifelse(df$disgust_max > median(df$disgust_max), "disgust_high", "disgust_low")

# Convert all to factors
df_combined[] <- lapply(df_combined, as.factor)
names(df_combined) <- make.names(names(df_combined), unique = TRUE)

trans_all <- as(df_combined, "transactions")

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
df_combined <- df[, combined_features]

# Convert binary tweet columns to yes/no
binary_tweet_cols <- c(
  "word_woman", "word_women", "word_men", "word_girl", "word_sex",
  "word_bitch", "word_fuck", "word_love", "word_peopl", "word_gender",
  "colloc_yes", "colloc_no"
)
df_combined[binary_tweet_cols] <- lapply(df_combined[binary_tweet_cols], function(x) factor(ifelse(x == 1, "yes", "no")))

# Discretize numeric variables
df_combined$tweet_sentiment <- ifelse(df$tweet_sentiment > median(df$tweet_sentiment), "sentiment_high", "sentiment_low")
df_combined$all_pos         <- ifelse(df$all_pos > median(df$all_pos), "pos_high", "pos_low")
df_combined$all_neg         <- ifelse(df$all_neg > median(df$all_neg), "neg_high", "neg_low")
df_combined$sadness         <- ifelse(df$sadness > median(df$sadness), "sadness_high", "sadness_low")
df_combined$sent_min        <- ifelse(df$sent_min > median(df$sent_min), "sent_min_high", "sent_min_low")
df_combined$disgust_max     <- ifelse(df$disgust_max > median(df$disgust_max), "disgust_high", "disgust_low")

# Convert all columns to factor
df_combined[] <- lapply(df_combined, as.factor)
names(df_combined) <- make.names(names(df_combined), unique = TRUE)

# Convert to transaction format
trans_all <- as(df_combined, "transactions")

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

