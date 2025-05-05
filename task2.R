library(dplyr)
library(readr)
library(ggplot2)
library(rcompanion)

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


# GENDER

unique(df_annotators$gender)
table(df_annotators$gender)

table_feature_label <- table(df_annotators$gender, df_annotators$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # do not reject null hypothesis, label is independent from gender

# If significant:
# cramerV(table_feature_label)
# degrees of freedom
# min(dim(table_feature_label))
# https://www.statology.org/interpret-cramers-v/

# AGE

unique(df_annotators$age)
table(df_annotators$age)

table_feature_label <- table(df_annotators$age, df_annotators$label_task1_1)
table_feature_label
chisq_result <- chisq.test(table_feature_label, correct=FALSE)
chisq_result  # do not reject null hypothesis, label is independent from age

# AGE + GENDER
table(df_annotators$gender, df_annotators$age, df_annotators$label_task1_1)
df_annotators$age_gender <- interaction(df_annotators$age, df_annotators$gender)
table(df_annotators$age_gender, df_annotators$label_task1_1)

# ETHNICITY

unique(df_annotators$ethnicity)
table(df_annotators$ethnicity)
table(df_annotators$ethnicity, df_annotators$label_task1_1)
# cannot use chisquared test or cramer'sV

# EDUCATION

unique(df_annotators$education)
table(df_annotators$education)
table(df_annotators$education, df_annotators$label_task1_1)

# COUNTRY

unique(df_annotators$country)
table(df_annotators$country)
table(df_annotators$country, df_annotators$label_task1_1)


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

df_annotators$continent <- continent_map[df_annotators$country]

table(df_annotators$continent, df_annotators$label_task1_1)


