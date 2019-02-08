# load the libraries
library(caret)
library(gbm)

set.seed(3433)

# load the dataset
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

# split the data into training and testing data sets
adData = data.frame(diagnosis, predictors)
inTrain <- createDataPartition(adData$diagnosis, p=3/4)[[1]]
training <- adData[inTrain, ]
testing <- adData[-inTrain, ]

set.seed(62433)

# fit the initial models and predict the outcomes. Then check the accuracy of each prediction
fitRF <- train(diagnosis~. , data = training, method = "rf")
predictRF <- predict(fitRF, testing)
accRF <- confusionMatrix(predictRF, testing$diagnosis)
accRF$overall["Accuracy"]

fitGBM <- train(diagnosis~. , data = training, method = "gbm", verbose = FALSE)
predictGBM <- predict(fitGBM, testing)
accGBM <- confusionMatrix(predictGBM, testing$diagnosis)
accGBM$overall["Accuracy"]

fitLDA <- train(diagnosis~. , data = training, method = "lda")
predictLDA <- predict(fitLDA, testing)
accLDA <- confusionMatrix(predictLDA, testing$diagnosis)
accLDA$overall["Accuracy"]

# create a new data frame with the three predictors and the testing outcomes
stacked <- data.frame(predictRF, predictGBM, predictLDA, diagnosis = testing$diagnosis)

# train a new model on this data set and try to predict the outcome. Then check the accuracy
fitSTA <- train(diagnosis~. , data = stacked, method = "rf")
predictSTA <- predict(fitSTA, testing)
accSTA <- confusionMatrix(predictSTA, testing$diagnosis)
accSTA$overall["Accuracy"]