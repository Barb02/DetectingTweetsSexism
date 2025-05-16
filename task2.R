
# Task 2: Exploratory Analysis of Annotator Behavior

# -------------------------------------------------------------------------------------------------------------------
# Libraries
# -------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(rcompanion)
library(sf)
library(rpart)
library(rpart.plot)
library(scales)
library(tidyr)
library(fastDummies)
library(corrplot)

# -------------------------------------------------------------------------------------------------------------------
# Importing
# -------------------------------------------------------------------------------------------------------------------

# -- To have the df without the count YES = NO tweets
#load("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/variables/df_after_task1.RData")
load("/home/barbara/MDS/ATDS/DetectingTweetsSexism/variables/df_after_task1.RData")

df <- df[ , 1:10]

names(df)
table(df$label_task1_1)
table(df$label_task1_1)/dim(df)[1] 

# columns for demographics: gender, age, ethnicity, education, country

# -------------------------------------------------------------------------------------------------------------------
# 1. Gender
# -------------------------------------------------------------------------------------------------------------------

unique(df$gender)

table_feature_label <- table(df$gender, df$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # do not reject null hypothesis, label is independent from gender

# normalize by dividing row-wise: distributions of yes x no within gender
table_feature_label_norm <- prop.table(table_feature_label, 1)
table_feature_label_norm

df_plot <- as.data.frame(table_feature_label_norm)
df_plot

# Plot normalized proportions for Gender
ggplot(df_plot, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", position = "dodge") +  # 'stat="identity"' to use the calculated proportions
  theme_minimal() +
  labs(
    title = "Proportion of Labels by Gender",
    x = "Gender", 
    y = "Proportion",
    fill = "Label"
  ) +
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))

# If significant: see cramerV

# -------------------------------------------------------------------------------------------------------------------
# 2. Age
# -------------------------------------------------------------------------------------------------------------------

unique(df$age)

table_feature_label <- table(df$age, df$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # reject null hypothesis, label is associated to age

# degrees of freedom
min(dim(table_feature_label)) - 1
cramerV(table_feature_label) # weak - With large samples, even tiny differences between observed and expected counts can become statistically significant.

table_feature_label_norm <- prop.table(table_feature_label, 1)
table_feature_label_norm
df_plot <- as.data.frame(table_feature_label_norm)
df_plot

ggplot(df_plot, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  theme_minimal() +
  labs(
    title = "Proportion of Labels by Age",
    x = "Age", 
    y = "Proportion",
    fill = "Label"
  ) +
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))

# -------------------------------------------------------------------------------------------------------------------
# 3. Age x Gender
# -------------------------------------------------------------------------------------------------------------------

df$age_gender <- interaction(df$age, df$gender)
table_feature_label <- table(df$age_gender, df$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # reject null hypothesis, label is associated to age x gender. might be due to age being significant
cramerV(table_feature_label) # weak

table_feature_label_norm <- prop.table(table_feature_label, 1)
df_plot <- as.data.frame(table_feature_label_norm)

ggplot(df_plot, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  theme_minimal() +
  labs(
    title = "Proportion of Labels by Age x Gender",
    x = "Age x Gender", 
    y = "Proportion",
    fill = "Label"
  ) +
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))

# -------------------------------------------------------------------------------------------------------------------
# 4. Ethnicity
# -------------------------------------------------------------------------------------------------------------------

unique(df$ethnicity)

table_feature_label <- table(df$ethnicity, df$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # reject null hypothesis, label is associated to ethnicity
cramerV(table_feature_label) # weak

table_feature_label_norm <- prop.table(table_feature_label, 1)
df_plot <- as.data.frame(table_feature_label_norm)

ggplot(df_plot, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  theme_minimal() +
  labs(
    title = "Proportion of Labels by Ethnicity",
    x = "Ethnicity", 
    y = "Proportion",
    fill = "Label"
  ) +
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))

# -------------------------------------------------------------------------------------------------------------------
# 5. Education
# -------------------------------------------------------------------------------------------------------------------

unique(df$education)

table_feature_label <- table(df$education, df$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # reject null hypothesis, label is associated to education
cramerV(table_feature_label) # weak

table_feature_label_norm <- prop.table(table_feature_label, 1)
df_plot <- as.data.frame(table_feature_label_norm)

ggplot(df_plot, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  theme_minimal() +
  labs(
    title = "Proportion of Labels by Education",
    x = "Education", 
    y = "Proportion",
    fill = "Label"
  ) +
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))

# -------------------------------------------------------------------------------------------------------------------
# 6. Country
# -------------------------------------------------------------------------------------------------------------------

unique(df$country)

table_feature_label <- table(df$country, df$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # reject null hypothesis, label is associated to country
cramerV(table_feature_label) # weak

table_feature_label_norm <- prop.table(table_feature_label, 1)
df_plot <- as.data.frame(table_feature_label_norm)

ggplot(df_plot, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  theme_minimal() +
  labs(
    title = "Proportion of Labels by Country",
    x = "Country", 
    y = "Proportion",
    fill = "Label"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))

# -------------------------------------------------------------------------------------------------------------------
# 7. Continent/Region
# -------------------------------------------------------------------------------------------------------------------

continent_map <- c('Algeria'= 'Middle East & North Africa',
  'Argentina'= 'Latin America & Caribbean',
  'Australia'= 'East Asia and Pacific',
  'Belgium'= 'Europe',
  'Canada'= 'North America',
  'Chile'= 'Latin America & Caribbean',
  'Cyprus'= 'Europe',
  'Czech Republic'= 'Europe',
  'Estonia'= 'Europe',
  'Finland'= 'Europe',
  'Germany'= 'Europe',
  'Greece'= 'Europe',
  'Hungary'= 'Europe',
  'Ireland'= 'Europe',
  'Israel'= 'Middle East & North Africa',
  'Italy'= 'Europe',
  'Latvia'= 'Europe',
  'Mexico'= 'Latin America & Caribbean',
  'Nepal'= 'Southern Asia',
  'Netherlands'= 'Europe',
  'New Zealand'= 'East Asia and Pacific',
  'Norway'= 'Europe',
  'Poland'= 'Europe',
  'Portugal'= 'Europe',
  'Romania'= 'Europe',
  'Russian Federation'= 'Eurasia & Central Asia',
  'Slovenia'= 'Europe',
  'South Africa'= 'Sub-Saharan Africa',
  'Spain'= 'Europe',
  'United Kingdom'= 'Europe',
  'United States'= 'North America',
  'Venezuela'= 'Latin America & Caribbean',
  'Viet Nam'= 'East Asia and Pacific')

df$continent <- continent_map[df$country]

table_feature_label <- table(df$continent, df$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # reject null hypothesis, label is associated to continent
cramerV(table_feature_label) # weak

table_feature_label_norm <- prop.table(table_feature_label, 1)
df_plot <- as.data.frame(table_feature_label_norm)

ggplot(df_plot, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  theme_minimal() +
  labs(
    title = "Proportion of Labels by Region",
    x = "Region", 
    y = "Proportion",
    fill = "Label"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))
# country is better, region generalizes too much

# -------------------------------------------------------------------------------------------------------------------
# 8. Country ~ Gender (see if the people from one country are women or men)
# -------------------------------------------------------------------------------------------------------------------

table_feature_label <- table(df$gender, df$country)
table_feature_label
table_feature_label_norm <- prop.table(table_feature_label, 2) # by col
df_plot <- as.data.frame(table_feature_label_norm)

ggplot(df_plot, aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  theme_minimal() +
  labs(
    title = "Gender proportion by Country",
    x = "Country", 
    y = "Proportion",
    fill = "Label"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("F" = "pink", "M" = "lightblue"))

# -------------------------------------------------------------------------------------------------------------------
# Correlation
# -------------------------------------------------------------------------------------------------------------------

df_cor <- df[ , 5:10]

df_cor$label_task1_1 <- ifelse(df_cor$label_task1_1 == "YES", 1, 0)

df_dummies <- dummy_cols(df_cor[, 1:5], remove_selected_columns = TRUE)

df_all  <- cbind(df_dummies, df_cor[ , !(names(df_cor) %in% names(df_cor[, 1:5]))])

cor_matrix <- cor(df_all, use = "complete.obs")

corrplot(cor_matrix,
         method = "circle",
         type = "upper",
         tl.cex = 0.4,
         tl.srt = 45,
         cl.cex = 1.0,
         title = "Correlation Matrix of Features",
         mar = c(0, 0, 0, 0))

cor_label <- cor_matrix[,"label_task1_1"]
cor_label <- cor_label[!names(cor_label) %in% "label_task1_1"]
cor_label <- cor_label[order(abs(cor_label), decreasing = TRUE)]
print(cor_label)

# -------------------------------------------------------------------------------------------------------------------
# Distribution
# -------------------------------------------------------------------------------------------------------------------

df_all <- df_all %>% 
  select(label_task1_1, everything())

n_yes <- sum(df_all$label_task1_1 == 1)
n_no  <- sum(df_all$label_task1_1 == 0)

yes_counts <- colSums(df_all[df_all$label_task1_1 == 1, 2:52] >= 1)
no_counts  <- colSums(df_all[df_all$label_task1_1 == 0, 2:52] >= 1)

yes_prop <- yes_counts / n_yes
no_prop  <- no_counts  / n_no

result <- data.frame(
  Column = names(df_all)[2:52],
  Count_1 = yes_counts,
  Proportion_1 = round(yes_prop, 3),
  Count_0 = no_counts,
  Proportion_0 = round(no_prop, 3)
)

result$Difference <- abs(result$Proportion_1 - result$Proportion_0)
result <- result[order(-result$Difference), ]

print(result)

# -------------------------------------------------------------------------------------------------------------------
# Logistic Regression
# -------------------------------------------------------------------------------------------------------------------

df_cor <- df[ , 5:10]

df_cor$label_task1_1 <- ifelse(df_cor$label_task1_1 == "YES", 1, 0)

# df_cor$continent <- continent_map[df_cor$country]

# df_cor <- df_cor[, !(names(df_cor) %in% c("country"))]
df_cor$age <- factor(df_cor$age) 
df_cor$country <- factor(df_cor$country) 
df_cor$education <- factor(df_cor$education)
df_cor$ethnicity <- factor(df_cor$ethnicity)
df_cor$age <- relevel(df_cor$age, ref = "23-45")
df_cor$country <- relevel(df_cor$country, ref = "Nepal")
df_cor$education <- relevel(df_cor$education, ref = "Less than high school diploma")
df_cor$ethnicity <- relevel(df_cor$ethnicity, ref = "White or Caucasian")

model_logit <- glm(label_task1_1 ~ ., data = df_cor, family = binomial)
summary(model_logit)

model_logit <- glm(label_task1_1 ~ . + age * gender + country * gender, data = df_cor, family = binomial)
summary(model_logit)

# -------------------------------------------------------------------------------------------------------------------
# Conclusions from Task 2 
# -------------------------------------------------------------------------------------------------------------------

# Here is what we took from: 

# 1. Gender -> Not significant but might be important for interactions (ex: age x gender)

# 2. Age -> age23-45 , age46+

# 4. Ethnicity -> ethnicityMiddle Eastern, ethnicityother, ethnicityMultiracial, ethnicityBlack or African American

# 5. Education -> educationBachelor’s degree, educationDoctorate

# 6. Country -> Algeria, Canada, Cyprus, Ireland, Israel







