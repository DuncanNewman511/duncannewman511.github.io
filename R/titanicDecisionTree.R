## https://www.guru99.com/r-decision-trees.html

rm(list=ls())

library(dplyr)
library(rpart)
library(rpart.plot)

getData <- function(fileName="./titanic.rdat")
{
    if (file.exists(fileName) == FALSE)
    {
        path <- "https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv"
        rawData <-read.csv(path)
        save(rawData, file=fileName)
    }
    load(fileName)

    return(rawData)
}


cleanData <- function(d)
{
    returnValue <- d %>%
        select(-c(home.dest, cabin, name, x, ticket)) %>%
	mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
               survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
        na.omit()

    for (col in c("age", "fare"))
    {
        indices <- which(returnValue[[col]] == "?")
        returnValue <- returnValue[-indices, ]

        returnValue[[col]] <- as.numeric(returnValue[[col]])
    }

    return(returnValue)
}

create_train_test <- function(data, size = 0.8, train = TRUE) {
    n_row <- nrow(data)
    total_row <- size * n_row
    train_sample <- 1:total_row
    if (train == TRUE) {
        returnValue <- data[train_sample, ]
    } else {
        returnValue <- data[-train_sample, ]
    }

    return(returnValue)
}

accuracy <- function(model, data)
{
    predict_unseen <- predict(model, data, type = 'class')
    table_mat <- table(data$survived, predict_unseen)
    accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
    return(accuracy_Test)
}

rocCurve <- function(model, data, title, formula=NULL)
{
    ## Practical Statistics for Data Scientists, pages 198 - 199
    predict_unseen <- predict(model, data, type = 'class')

    df <- data.frame(predicted=predict_unseen,
                     true=data$survived)


    df$p <- 0
    indices <- which(df$predicted == "Yes")
    df$p[indices] <- 1

    df$t <- 0
    indices <- which(df$true == "Yes")
    df$t[indices] <- 1

    indices <- order(-df$p)

    recall <- cumsum(df$t[indices] == 1)/sum(df$t == 1)
    specificity <- (sum(df$t == 0) - cumsum(df$t[indices] == 0)) / sum(df$t == 0)
    plot( (1 - specificity), recall,
         xlab = "False positive rate",
         ylab = "True positive rate",
         main=sprintf("%s\n(%s data points)",
                      title,
                      formatC(nrow(df), big.mark=",")),
         sub=sprintf("AUC = %.3f", sum(recall)/nrow(df))
         )

    abline(coef=c(0,1), col="blue")

    if (is.null(formula) == TRUE)
    {
    }
    else
    {
        text(x=0.75, y=0.25,
             label=sprintf("Formula = \n%s", format(formula))
             )
    }
}



set.seed(678)

titanic <- getData()

shuffle_index <- sample(1:nrow(titanic))

titanic <- titanic[shuffle_index, ]

titanic <- cleanData(titanic)

data_train <- create_train_test(titanic, 0.8, train = TRUE)
data_test <- create_train_test(titanic, 0.8, train = FALSE)

formula <- survived ~ embarked

fit <- rpart(formula, data=data_train, method='class')

rpart.plot(fit, extra = 106)

print(sprintf('Accuracy for train: %.3f', accuracy(fit, data_train)))
print(sprintf('Accuracy for test: %.3f', accuracy(fit, data_test)))

control <- rpart.control(minsplit = 4, ## minimum number of observations per node
                         minbucket = round(5 / 3), ## minimum number of observations per leaf
                         maxdepth = 3, ## max depth of the tree
                         cp = 0 ## Threshold for minimum model fit complexity improvement to terminate processing
                         )

tune_fit <- rpart(formula , data = data_train, method = 'class', control = control)

rpart.plot(tune_fit, extra = 106)


print(sprintf('Accuracy for tune_fit train: %.3f', accuracy(tune_fit, data_train)))
print(sprintf('Accuracy for tune_fit test: %.3f', accuracy(tune_fit, data_test)))


rocCurve(fit, data_train, "Training data", formula)
rocCurve(fit, data_test, "Testing data", formula)

rocCurve(tune_fit, data_train, "Training data (tuned)", formula)
rocCurve(tune_fit, data_test, "Testing data (tuned)", formula)
