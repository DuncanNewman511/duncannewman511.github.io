install.packages("tm")
install.packages("caret")
install.packages("e1071")
install.packages("naivebayes")
install.packages("dplyr")
install.ackages("kernlab")

library(tm)
library(caret)
library(e1071)
library(naivebayes)
library(dplyr)
library(kernlab)


# LOAD YOUR CSV FILE

data <- read.csv("C:/Users/dunca/OneDrive/Desktop/ENMA 754/HW3/spam-detection.csv", stringsAsFactors = FALSE)

# Rename for convenience
colnames(data) <- c("label", "text")
data$label <- factor(data$label)


# TEXT PREPROCESSING

corpus <- VCorpus(VectorSource(data$text))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

dtm <- DocumentTermMatrix(corpus)

# Keep most common terms
dtm <- removeSparseTerms(dtm, 0.99)

df <- as.data.frame(as.matrix(dtm))
df$label <- data$label


# TRAIN/TEST SPLIT

set.seed(123)
trainIndex <- createDataPartition(df$label, p = 0.8, list = FALSE)

train <- df[trainIndex, ]
test  <- df[-trainIndex, ]


# CROSS-VALIDATION CONTROL

ctrl <- trainControl(method = "cv", number = 5)


# MODEL 1: NAIVE BAYES

model_nb <- train(label ~ ., 
                  data = train,
                  method = "naive_bayes",
                  trControl = ctrl)


# MODEL 2: SVM (Linear)

model_svm <- train(label ~ ., 
                   data = train,
                   method = "svmLinear",
                   trControl = ctrl)


# EVALUATE MODELS

pred_nb  <- predict(model_nb,  test)
pred_svm <- predict(model_svm, test)

cat("\n===== NAIVE BAYES RESULTS =====\n")
print(confusionMatrix(pred_nb, test$label))

cat("\n===== SVM RESULTS =====\n")
print(confusionMatrix(pred_svm, test$label))


# PRINT ACCURACIES SIDE BY SIDE

cat("\n===== ACCURACY SUMMARY =====\n")
cat("Naive Bayes Accuracy:",
    confusionMatrix(pred_nb, test$label)$overall["Accuracy"], "\n")
cat("SVM Accuracy:",
    confusionMatrix(pred_svm, test$label)$overall["Accuracy"], "\n")