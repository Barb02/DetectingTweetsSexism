library(dplyr)
library(readr)
library(ggplot2)
library(rcompanion)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

df = read_csv("/home/barbara/MDS/ATDS/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df = read_csv("C:/Users/claud/OneDrive/Ambiente de Trabalho/TACD/Projeto/DetectingTweetsSexism/tables/EXIST2025_train.csv")
#df = read_csv("C:/Users/marta/OneDrive/Documentos/FCUP/TACD/project/DetectingTweetsSexism/tables/EXIST2025_train.csv")

names(df)
table(df$label_task1_1)
table(df$label_task1_1)/dim(df)[1] # not very unbalanced

# columns for demographics: gender, age, ethnicity, education, country

annotators = unique(df$annotator_id)
length(annotators)

df_annotators <- df[!duplicated(df$annotator_id), ]
dim(df_annotators)


annotator_stats <- df %>%
  group_by(annotator_id, gender, age, country) %>%
  summarise(
    total_labels = n(),
    yes_labels = sum(label_task1_1 == "YES"),
    yes_rate = yes_labels / total_labels
  )


ggplot(annotator_stats, aes(x = age, y = yes_rate)) +
  geom_boxplot() +
  labs(title = "Sexist Labeling Rate by Gender", y = "Proportion of YES Labels")

table(annotator_stats$age, annotator_stats$yes_rate > 0.5)

# GENDER

unique(df_annotators$gender)
table(df_annotators$gender)

table_feature_label <- table(df$gender, df$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # do not reject null hypothesis, label is independent from gender

ggplot(df, aes(x = gender, fill = label_task1_1)) + 
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Label count by Gender",
    x = "Gender", 
    y = "Count",
    fill = "Label"
  ) +
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))

# If significant:
# cramerV(table_feature_label)
# degrees of freedom
# min(dim(table_feature_label))
# https://www.statology.org/interpret-cramers-v/

# AGE

unique(df_annotators$age)
table(df_annotators$age)

table_feature_label <- table(df$age, df$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # reject null hypothesis, label is associated to age
cramerV(table_feature_label) # weak 

ggplot(df, aes(x = age, fill = label_task1_1)) + 
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Label count by Age",
    x = "Age", 
    y = "Count",
    fill = "Label"
  ) +
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))

# AGE + GENDER

df$age_gender <- interaction(df$age, df$gender)
table_feature_label <- table(df$age_gender, df$label_task1_1)
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # reject null hypothesis, label is associated to age x gender
cramerV(table_feature_label) # weak

ggplot(df, aes(x = interaction(age, gender), fill = label_task1_1)) + 
  geom_bar(position = "dodge", width = 0.7) +
  theme_minimal() +
  labs(
    title = "Label count by Age and Gender",
    x = "Age x Gender", 
    y = "Count",
    fill = "Label"
  ) +
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))


# ETHNICITY

unique(df_annotators$ethnicity)
table(df_annotators$ethnicity)

table_feature_label <- table(df$ethnicity, df$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # reject null hypothesis, label is associated to ethnicity
cramerV(table_feature_label) # weak

ggplot(df, aes(x = ethnicity, fill = label_task1_1)) + 
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Label count by Ethnicity",
    x = "Ethnicity", 
    y = "Count",
    fill = "Label"
  ) +
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))

# EDUCATION

unique(df_annotators$education)
table(df_annotators$education)

table_feature_label <- table(df$education, df$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # reject null hypothesis, label is associated to education
cramerV(table_feature_label) # weak

ggplot(df, aes(x = education, fill = label_task1_1)) + 
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Label count by Education",
    x = "Education", 
    y = "Count",
    fill = "Label"
  ) +
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))

# COUNTRY

unique(df_annotators$country)
table(df_annotators$country)

table_feature_label <- table(df$country, df$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # reject null hypothesis, label is associated to country
cramerV(table_feature_label) # weak

ggplot(df, aes(x = country, fill = label_task1_1)) + 
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Label count by Education",
    x = "Education", 
    y = "Count",
    fill = "Label",
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))

# log count
ggplot(df, aes(x = country, fill = label_task1_1)) + 
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Label count by Education",
    x = "Education", 
    y = "Count",
    fill = "Label",
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato")) +
  scale_y_log10()
# TODO: fix



df_map <- df %>%
  group_by(country) %>%
  summarise(
    yes_percent = mean(label_task1_1 == "YES") * 100
  )


# CONTINENT

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

ggplot(df, aes(x = continent, fill = label_task1_1)) + 
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Label count by Education",
    x = "Education", 
    y = "Count",
    fill = "Label",
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("YES" = "lightgreen", "NO" = "tomato"))  +
  scale_y_log10()

# COUNTRY + GENDER

df$country_gender <- interaction(df$country, df$gender)
table_feature_label <- table(df$country_gender, df$label_task1_1)


ggplot(df, aes(x = country, fill = gender)) + 
  geom_bar(position = "dodge", width = 0.7) +
  theme_minimal() +
  labs(
    title = "Label count by Age and Gender",
    x = "Age x Gender", 
    y = "Count",
    fill = "Label"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("F" = "lightgreen", "M" = "tomato"))
