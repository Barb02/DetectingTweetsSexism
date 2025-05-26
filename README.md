# DetectingTweetsSexism

In this project, we aim to build a binary classifier to detect sexism in tweets using the EXIST 2025 Tweet Dataset, which contains tweets labeled as sexist or not sexist by different annotators. In it, each tweet was labeled by multiple annotators, and each annotator labeled a different set of tweets. In addition, the dataset includes demographic information for each annotator, such as gender, age, ethnicity, education level, and country.  Using  this information, we apply NLP techniques such as TF_IDF, sentiment analysis and emotion recognition to retrieve features from the tweets, and exploratory analysis, clustering, association rules and recommendation system concepts to retrieve features from the annotator’s demographics.


The organization of the documents used for this project is as follows:

- Task1.R -> Sexism Identification Classifier: Develop a set of binary classifiers (Yes/No) that accurately identify whether a tweet contains sexist content. 

- Task2.R -> Exploratory Analysis of Annotator Behavior: Investigate the annotators’ labeling patterns. Consider annotator demographics (gender, age, country, etc.) and analyze how these might affect labeling decisions

- Task3.R -> Clustering Annotators Based on Labeling Patterns: Group annotators based on their labeling behavior (e.g., annotators who frequently label tweets as sexist, those who rarely label tweets as sexist, or balanced annotators). 

- Task4.R -> Association Rule Mining (Apriori Algorithm): Discover interesting relationships between annotator characteristics and their annotation patterns using association rule mining

- functions.R -> Functions created to add the features we chose to implement based on the analysis made in each task 

- projeto.R -> Starting with a raw dataset, using the functions created in functions.R to add the necessary features

- modeling -> Jupiter notebook used to choose the model based on Task 1 and then tuning the model after all the tasks and applying the final best model to the test dataset

- svm_rbf_pipeline.pkl -> our best model 

- folder tables -> contains the original datasets, the datasets after the processing and the final results 
 
- folder variables -> contains some variables we used to keep record of what was changing to the dataset after each task and some variables from the tasks we needed to implement the functions 