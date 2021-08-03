# -------------- Adding Libraries -------------- 

library(dplyr)
library(ggplot2)
library(psych)
library(tidyverse)
library(randomForest)
library(GGally)
library(caret)
library(naivebayes)
library(Amelia)

# -------------- Data preparing and cleaning -------------- 

# Data located in same level with parent folder under heart.csv file name
data <- read.csv("../heart.csv")

# Rename first column from i..age to age for more simplicity
colnames(data)[1] <- "age"

# Change the values of the "target" attribute from 0,1 to False,True
# data$target <- factor(data$target, levels = c(0,1), labels = c("False", "True"))

# Display the Data set structure
str(data)
# Display first observations of the Data set
head(data)
# Display statistics of the Data set
describe(data)
# Display and visualize the missing data of the Data set
missmap(data)

# Count target elements number
table(data$target)


# ------------------- Data visualizations ------------------- 

# Frequency polygon histogram | Display the distribution of the "target" by "age"
data %>% ggplot(aes(age, colour = target)) +
  geom_freqpoly(binwidth = 1) +
  labs(title = "Age distribution by target")

# Frequency histogram | Display the distribution of the the "target" by "Nombre de vaisseaux principaux"
data %>% ggplot(aes(x=ca, fill=target, color=target)) +
  geom_histogram(binwidth = 1) +
  labs(title="Nombre de vaisseaux principaux Distribution by target")

# Some statistics of pairing data attributes
paired_data <- data.frame(data["thalach"],  data["oldpeak"], data["target"])
ggpairs(paired_data)


# ---------------- Data Modeling ---------------

# Split data into training data and testing data
data <- arrange(data, age, by_group = FALSE)
data <- select(data, age, sex, cp, chol, ca, thal, target)

colnames(data);

# training data
train_data <- data[1:212, ]
# testing data
test_data <- data[213:303, ]

# Prop initial data set
noquote("Initial Data set target value props : ")
prop.table(table(data$target)) * 100

# Prop training data set
noquote("Training Data set target value props : ")
prop.table(table(train_data$target)) * 100

# Prop testing data set
noquote("Testing Data set target value props : ")
prop.table(table(test_data$target)) * 100


# ---------------------- Naive Bayes Model - Library(naivebayes) --------------

# Fitting Naive Bayes Model to training data set

model <- naive_bayes(as.factor(target) ~ ., data = train_data, usekernel = T, laplace = 1)
tables(model)
plot(model)


# -------------------------- Prediction --------------

# Confustion Matrix - Test Data
pred <- predict(model, select(test_data, -target))
tab <- table(pred, test_data$target)

# Mis-Classification test - Erreur
paste("Erreur sur le test :", round(1 - sum(diag(tab)) / sum(tab), digits = 2), "%")
cat("\n")

# Model Evaluation
confusion_matrix <- table(test_data$target, pred)
confusionMatrix(confusion_matrix)








