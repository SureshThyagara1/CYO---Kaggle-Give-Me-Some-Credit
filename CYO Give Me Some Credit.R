# Title : CYO Project - Give Me Some Credit from Kaggle
# Author : Suresh Thyagarajan
# Script & data info : This script helps a bank decide on sanctioning a loan
#                      by predicting the probability that somebody will experience 
#                      financial distress in the next two years.
# Data set consits of historical data with 12 features of borrowers
# Also, there is a test dataset for competition submimission 
# Datasets are included in the github directory link

# Project work was done in the following PC:

print('Operating System:')
version

## Install missing packages

if(require(tidyverse)) install.packages('tidyverse', 
                                        repos = 'http://cran.us.r-project.org')
if(require(ggplot2)) install.packages('ggplot2', 
                                        repos = 'http://cran.us.r-project.org')
if(require(epiDisplay)) install.packages('epiDisplay', 
                                      repos = 'http://cran.us.r-project.org')
if(require(dplyr)) install.packages('dplyr', 
                                    repos = 'http://cran.us.r-project.org')
if(require(caret)) install.packages('caret', 
                                    repos = 'http://cran.us.r-project.org')
if(require(dataCompareR)) install.packages('dataCompareR', 
                                    repos = 'http://cran.us.r-project.org')
if(require(nnet)) install.packages('nnet', 
                                           repos = 'http://cran.us.r-project.org')
if(require(rpart)) install.packages('rpart', 
                                   repos = 'http://cran.us.r-project.org')

## Introduction

# The aim of the project is to assist a bank in predicting that somebody will 
# face financial distress in the next 2 years. This projects has been selected 
# from Kaggle with the name Give Me Some Credit. Originally, this was a competition 
# with the challenge to better the credit score algorithm. The purpose of this project
# is to model predictions using various methods.

# Submission for competition in the format as desired by the organizers is also done.

# A overview of the challenge is in the below URL:
# https://www.kaggle.com/c/GiveMeSomeCredit/overview

## Methods & Analysis

### Data import

# Data set consists of taining set and and a test set. However, the
# test set was only for evaluating competition challenge by organizer. Our objective
# here is to establish a prediction model. Hence, we use the training
# dataset to predict the model

# The training dataset is available in the following link:
# https://www.kaggle.com/c/GiveMeSomeCredit/data?select=cs-training.csv

# The link is not directly dowloadable and hence the file needs to be downloaded
# into the respective working directory

# First, we set the working directory
setwd("C:\\Data Science\\CYO\\Credit Score")
# Check the the working directory
getwd()

# Download the compressed folder with the datasets into the working directory

# All file paths from now on would be relative

# The data file is available in the following github repository

## https://github.com/SureshThyagara1/CYO---Kaggle-Give-Me-Some-Credit/blob/master/Data/cs-training.csv.zip

# Following codes are used to unzip a compressed folder and download data which is 
# in csv format & create an object called credit_data with the dowloaded dataset
credit_data <- read.csv(unz("cs-training.csv.zip","cs-training.csv"))

# Download the test dataset, which is essentially used for submission in the 
# competition. Following github link contains the test dataset

# https://github.com/SureshThyagara1/CYO---Kaggle-Give-Me-Some-Credit/blob/master/Data/cs-test.csv.zip

# Following codes are used to unzip a compressed folder and download data which is 
# in csv format & create an object called credit_data with the dowloaded dataset
credit_data_comp <- read.csv(unz("cs-test.csv.zip","cs-test.csv"))

# Check structure of downloaded trainig dataset
str(credit_data)
# Dataset contains 150,000 observations with 12 variables

### Data Processing

# A quick overview of downloaded data
head(credit_data)
# Names of the variables in the data
names(credit_data)

#### Data Exploration & Visualization

# The credit_data data set has 12 variables in total. The first variable is a serial 
# number which does not contribute to analysis. 

# Hence, removing "X" which is for serial number and does not qualify to be a 
# explaratory or a predictor variable
credit_data$X = NULL
# Check if the "X" variable has been removed from dataset
names(credit_data)

#### Data Summary

# 1) "SeriousDlqin2yrs" - Person experienced 90 days past due delinquency or worse 
# Values in this variable are 0 for no and 1 for yes on serious deliquency in
# the last 2 years
library(epiDisplay)
tab1(credit_data$SeriousDlqin2yrs,sort.group = "decreasing",
     cum.percent = FALSE, bar.values = "frequency", cex = 1, cex.names = 1,
     main = "Serious Deliquency in 2 years", xlab = "Deliquency",
     ylab = "count", col = c("blue", "yellow"))
# This shows 6.7 % had serious deliquency in last 2 years.

# 2) "RevolvingUtilizationofUnsecuredLines" - Total balance on credit cards 
# and personal lines of credit except real estate and no installment debt 
# like car loans divided by the sum of credit limits
summary(credit_data$RevolvingUtilizationOfUnsecuredLines)
plot(credit_data$RevolvingUtilizationOfUnsecuredLines,col = "green4")
sum(credit_data$RevolvingUtilizationOfUnsecuredLines > 1)
# This data should be around 1 being a proportion. However, we find that
# a number of data (3321) have significantly high values. This outlier treatment
# would be dealt while handing outliers in one of the following sections.

# 3) "age" - Age of borrower in years
summary(credit_data$age)
library(dplyr)
library(ggplot2)
credit_data %>% ggplot(aes(age)) + geom_density()
# This data gives the age profile of the borrowers with the maximum number
# being in the 30 to 60 years range

# 4) "NumberOfTime30-59DaysPastDueNotWorse" - Number of times borrower has been 
# 30-59 days past due but no worse in the last 2 years.
summary(credit_data$NumberOfTime30.59DaysPastDueNotWorse)
credit_data %>% ggplot(aes(NumberOfTime30.59DaysPastDueNotWorse)) + geom_density()
table(credit_data$NumberOfTime30.59DaysPastDueNotWorse)
# Vast majority of the borrowers did not have past dues for 30 to 59 days

# 5) "DebtRatio" - Monthly debt payments, alimony,living costs divided by monthly 
# gross income
summary(credit_data$DebtRatio)
plot(credit_data$DebtRatio, col = "red")
sum(credit_data$DebtRatio > 1)
# Debt ratio is a proportion/percentage. There are a possible 35137 outliers
# in this predictor variable. This would be dealt with in outlier treatment section

# 6) "MonthlyIncome" - Monthly income
summary(credit_data$MonthlyIncome)
plot(credit_data$MonthlyIncome,main = "Monthly Income Distribution", 
     xlab = "Monthly Income", col = "turquoise")
# "MonthlyIncome" varaible consists of missing values as well as outliers. They will
# be dealt with in a later section

# 7) "NumberOfOpenCreditLinesAndLoans" - Number of Open loans (installment like
# car loan or mortgage) and Lines of credit (e.g. credit cards)
summary(credit_data$NumberOfOpenCreditLinesAndLoans)
hist(credit_data$NumberOfOpenCreditLinesAndLoans,
 main = "Number of Open Credit Lines and loans", col = "yellow4")
table(credit_data$NumberOfOpenCreditLinesAndLoans)
# The number of open credit lines and loans average 8.453 and has a maximum value
# of 58. There are outliers in this as well with very high number of NumberOfOpenCreditLinesAndLoans 

# 8) "NumberOfTimes90DaysLate" - Number of times borrower has been 90 days or more past due.
summary(credit_data$NumberOfTimes90DaysLate)
credit_data %>% ggplot(aes(NumberOfTimes90DaysLate)) + geom_density()
table(credit_data$NumberOfTimes90DaysLate)
# Again, a vast majority of the borrowers did not have even a single occassion
# when they have been past due for 90 days or more

# 9) "NumberRealEstateLoansOrLines" - Number of mortgage and real estate loans 
# including home equity lines of credit
summary(credit_data$NumberRealEstateLoansOrLines)
library(epiDisplay)
tab1(credit_data$NumberRealEstateLoansOrLines , sort.group = "decreasing", cum.percent = FALSE,bar.values = "percent", 
     main = "NumberRealEstateLoansOrLines", 
     xlab = "NumberRealEstateLoansOrLines",
     ylab = "count",horiz = TRUE, cex = .6,cex.names = 0.6)
table(credit_data$NumberRealEstateLoansOrLines)
# A significant majority of the borrowers had upto 2 mortgage loans. There are a
# few outliers with very high number of RealEstateLoansorLines

# 10) "NumberOfTime60-89DaysPastDueNotWorse" - Number of times borrower has been 
# 60-89 days past due but no worse in the last 2 years.
summary(credit_data$NumberOfTime60.89DaysPastDueNotWorse)
credit_data %>% ggplot(aes(NumberOfTime60.89DaysPastDueNotWorse)) + geom_density()
table(credit_data$NumberOfTime60.89DaysPastDueNotWorse)
# Again,a vast majority do not any record of past due in 60 to 89 days

# 11) "NumberOfDependents" - Number of dependents in family excluding themselves 
# (spouse, children etc.)
summary(credit_data$NumberOfDependents)
library(epiDisplay)
tab1(credit_data$NumberOfDependents, sort.group = "decreasing", cum.percent = FALSE,
     bar.values = "percent", main = "Distribution of dependents", 
     xlab = "Number of Dependents",
     ylab = "count",horiz = TRUE)
# A significant number of 57.9% do not have any dependents. Also, important
# to note that about 2.6% values are missing in this predictor variable.

### Data Validation

# Checks include checking for missing values and data ranges for outliers

#### Missing values treatment

# Identify number of missing values in dataset
sum(is.na(credit_data))

# There are 33655 missing values in the train dataset
# Identify, how significant as a proportion of the dataset are the missing values
sum(is.na(credit_data))/(nrow(credit_data)*ncol(credit_data))
# About 2% of the data are missing

# Identify number of missing values in each column
sapply(credit_data, function(x) sum(is.na(x)))
# Only "MonthlyIncome" and "NumberOfDependents" have missing values

# Proportion of missing values for "MonthlyIncome" explaratory variable
29731/150000
# About 20% of the "MonthlyIncome" variable has missing values. 
summary(credit_data$MonthlyIncome)
# Following are critical observations for determing imputation method for missing
# values of "MonthlyIncome" variable
# 1) 75% of the values are 8249 or below
# 2) Max value is very high - an oulier
# 3) Imputing mean value distorts the MonthlyIncome of the data set
 
# The preferred approach for "MonthlyIncome" variable is Median value imputations
# for missing values
# Replace missing values with median values in "MonthlyIncome" explaratory variable
credit_data$MonthlyIncome[is.na(credit_data$MonthlyIncome)] <- median(credit_data$MonthlyIncome,na.rm = TRUE)
# Check if the missing values have been imputed
sum(is.na(credit_data$MonthlyIncome))
# Missing values in "MonthlyIncome" explaratory variable have been imputed 
# successfully
 
# Imputation of missing values of "NumberOfDependents" explaratory variable
summary(credit_data$NumberOfDependents)
# There are 3924 missing values in "NumberofDependents" predictor variable
# Mean value cannot be used for imputation. 
# Identify unique values in "NumberOfDependents" explaratory variable
unique(credit_data$NumberOfDependents)
# Values vary from 0 to 20
# Establish the frequency of each of these values
table(credit_data$NumberOfDependents)
# Vast majority of values are 0
sum(is.na(credit_data$NumberOfDependents))/nrow(credit_data)
# Only 2% of the values are missing in "NumberOfDependents" variable
# % of "NumberOfDependents" variable values are 0 is
86902/150000
# About 58% of value in this variable are 0. 
# Impute missing values with 0
credit_data$NumberOfDependents[is.na(credit_data$NumberOfDependents)] <- 0
sum(is.na(credit_data$NumberOfDependents))

# Outlier Analysis

# Outliers in the dataset
boxplot(credit_data, main = "Identification of Outliers")
# "MonthlyIncome" & "DebtRatio" clearly have outliers. During data visualisation
# other outliers "RevolvingUtilizationOfUnsecuredLines", 
# "NumberOfOpenCreditLinesAndLoans" and "NumberRealEstateLoansOrLines" were also
# identified

# Outliers can be identified and replaced using percentile method. 

# Outlier replacement for "RevolvingUtilizationOfUnsecuredLines"
quantile(credit_data$RevolvingUtilizationOfUnsecuredLines, c(.1,.5,.8,.9,.98,.99,1))
# Replace all values above 99th percentile with the value of 99th percentile.
credit_data$RevolvingUtilizationOfUnsecuredLines[credit_data$RevolvingUtilizationOfUnsecuredLines > 
  + quantile(credit_data$RevolvingUtilizationOfUnsecuredLines,c(.99))] <- 
  + quantile(credit_data$RevolvingUtilizationOfUnsecuredLines,c(.99))
summary(credit_data$RevolvingUtilizationOfUnsecuredLines)

# Outlier replacement for "DebtRatio"
quantile(credit_data$DebtRatio, c(.1,.5,.8,.9,.98,.99,1))
# Replace all values above 80th percentile with the value of 80th percentile.
credit_data$DebtRatio[credit_data$DebtRatio > 
  + quantile(credit_data$DebtRatio,c(.8))] <- 
  + quantile(credit_data$DebtRatio,c(.8))
summary(credit_data$DebtRatio)

# Outlier replacement for "MonthlyIncome"
quantile(credit_data$MonthlyIncome, c(.1,.5,.8,.9,.98,.99,.999,1))
# Replace all values above 99th percentile with the value of 99th percentile.
credit_data$MonthlyIncome[credit_data$MonthlyIncome > 
  + quantile(credit_data$MonthlyIncome,c(.99))] <- 
  + quantile(credit_data$MonthlyIncome,c(.99))
summary(credit_data$MonthlyIncome)

# Outlier replacement for "NumberOfOpenCreditLinesAndLoans"
quantile(credit_data$NumberOfOpenCreditLinesAndLoans, c(.1,.5,.8,.9,.98,.99,1))
# Replace all values above 99th percentile with the value of 99th percentile
credit_data$NumberOfOpenCreditLinesAndLoans[credit_data$NumberOfOpenCreditLinesAndLoans > 
 + quantile(credit_data$NumberOfOpenCreditLinesAndLoans, c(.99))] <-
 + quantile(credit_data$NumberOfOpenCreditLinesAndLoans, c(.99))
summary(credit_data$NumberOfOpenCreditLinesAndLoans)

# Outlier replacement for "NumberRealEstateLoansOrLines"
quantile(credit_data$NumberRealEstateLoansOrLines,c(.1,.5,.8,.9,.98,.99,.999,1))
# Replace all values above 99th percentile with the value of 99th percentile
credit_data$NumberRealEstateLoansOrLines[credit_data$NumberRealEstateLoansOrLines >
  + quantile(credit_data$NumberRealEstateLoansOrLines, c(.99))] <-
  + quantile(credit_data$NumberRealEstateLoansOrLines, c(.99))
summary(credit_data$NumberRealEstateLoansOrLines)

# Conversion of dependendent variable into categorical
credit_data$SeriousDlqin2yrs <- as.factor(credit_data$SeriousDlqin2yrs)
class(credit_data$SeriousDlqin2yrs)

# Numerical features normalisation 
head(credit_data %>% mutate_if(is.numeric, scale))

### Building Models

### Splitting of Data

# Dataset will be partitioned into 70% training & 30% validation. The logic
# behind this split is for this dataset size at 70% training set, classification
# model is better and at at 30% test set error estimates are more accurate

# Set the seed 
set.seed(1, sample.kind = "Rounding")
# Partition the data & store it in index_test
library(caret)
index_test <- createDataPartition(y = credit_data$SeriousDlqin2yrs,
                                  times = 1, p = 0.7, list = FALSE) 
# Create train set
train_models <- credit_data[index_test, ]
# there are 105001 observations in the training set

# Create validation set
validate_models <- credit_data[-index_test,]
# there are 44999 observations in the validation set

# comparison of both training and validation datasets
library(dataCompareR)
comparison_train_val <- rCompare(train_models, validate_models)
comp_summ <- summary(comparison_train_val)
comp_summ[c("datasetSummary", "ncolInAOnly", "ncolInBOnly", "ncolCommon",
            "rowsInAOnly", "rowsInBOnly", "nrowCommon")]

## Logistics Regression Model

# Model fitting
glm_model <- glm(SeriousDlqin2yrs ~ ., family = 'binomial', data = train_models)
# Significance value using Summary
summary(glm_model)
# Almost all features are statistically significant. 

# Prediction
pred_logit <- predict(glm_model, newdata = validate_models, type = "response")

# To convert pred_logit to predictions, we define a cut-off at 0.5
y_hat_glm <- factor(ifelse(pred_logit > 0.5, 1, 0))
# Confusion Matrix
library(caret)
confusionMatrix(y_hat_glm, reference = validate_models$SeriousDlqin2yrs,
                positive = "1")

# Accuracy of GLM model
confusionMatrix(y_hat_glm, reference = validate_models$SeriousDlqin2yrs,
                positive = "1")$overall["Accuracy"]
# Accuracy of the model is a very good at 93%. Specifity (a measure of True
# Positive Rate)is at 99%. However, the sensitivity is at only 5%. Also, the cut-off could be 
# tuned to reflect the actual event rate instead of 0.5.

#### Classification Tree Model

# Decision tree
library(rpart)
# Build the model
dec_tree <- rpart(SeriousDlqin2yrs ~ . , method='class', data=train_models)
# Plot the decision tree
library(rpart.plot)
rpart.plot(dec_tree, uniform = TRUE, extra = 4)

# Predictions
pred_tree <- predict(dec_tree,newdata = validate_models, type = 'class')

# Confusion Matrix
confusionMatrix(pred_tree,
                reference=validate_models$SeriousDlqin2yrs,
                positive='1')

# Accuracy of Decision Tree Model
confusionMatrix(pred_tree, reference = validate_models$SeriousDlqin2yrs,
                positive = "1")$overall["Accuracy"]

# Accuracy of the Decision Tree Model is a very good 93%. The specificity is a
# very good 99% and sensitivity is 17%.

#### Receiver Operating Characteristic (ROC) curve model

# Logistics Regression Model (for ROC curve model)
library(nnet)
library(tidyverse)

# Model fitting
lr_model <- multinom(SeriousDlqin2yrs~.,train_models)

# Logistic Model prediction
p <- predict(lr_model,validate_models)

# Model performance evaluation using ROCR
library(ROCR)
pred <- predict(lr_model,newdata=validate_models, type = 'prob')
pred <- prediction(pred,validate_models$SeriousDlqin2yrs)
eval <- performance(pred, "acc")
plot(eval)

# Identification of cut-off based on eye estimate
abline(h=0.93, v=0.40)
eval

# Identifying best cut-off and accuracy
max <- which.max(slot(eval, "y.values")[[1]])
acc <- slot(eval, "y.values")[[1]][max]
cut <- slot(eval, "x.values")[[1]][max]
print(c(Accuracy = acc, Cutoff = cut))
# Accuracy value is 0.9348 and cut off is at 0.4187

# ROC curve gives performance at different cut off values
roc <- performance(pred, "tpr", "fpr")
plot(roc, colorize = T, main = "ROC Curve", ylab = "Sentivity", xlab = "1 - Specificity")
abline(a=0, b=1)
# Visually, it is clear that the performance is better than at a cut-off of 0.5

# Area under the curve
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
auc
legend(.7,.5, auc, title = "AUC", cex = 0.6)
# Area under the curve is a very good 0.8074

# Competition Submission

# We choose to use the ROC Curve model for competition submission

# Download competition test data
credit_data_comp <- read.csv(unz("cs-test.csv.zip","cs-test.csv"))
# Make predictions based on the ROCR Curve model 
pred_comp <- predict(lr_model, credit_data_comp,type = 'prob')
# We now, write the prediction into csv format as required for competition submission
write.csv(pred_comp, "submission.csv")

## Results Summary

# Logistics Regression Model - Accuracy of the model is a very good at 93%. 
# Specifity is at 99%. However, the sensitivity is at only 5%. The NPV is 93% 
# and PPV 58%.

# Decision Tree Model - Accuracy of the Decision Tree Model is a very good 
# 93%. The specificity is a very good 99% and sensitivity is 17%. NPV is at 94% 
# and PPV is at 55%.

# ROC Curve Model - Accuracy value is 0.9348 and cut off is at 0.4187. Area under 
# the curve is also a very good 0.8074.

## Conclusion

# Accuracy levels have been high in all models. It is essential to predict
# deliquencies in 2 years more accurately than no deliquencies. With an AUC of
# 80.74% ROC Curve model does present a very good case for the prediction
# model.

# This algorithm based approach is important for banks to identify potential defaulters at an
# early stage. The above models could be improvised upon iteratively by building
# in more variable transformations and including more features like dependents, 
# account history etc.