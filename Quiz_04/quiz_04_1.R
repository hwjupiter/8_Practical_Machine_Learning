# load the required data
library(ElemStatLearn)
library(caret)
training <- data.frame(vowel.train)
testing <- data.frame(vowel.test)

# convert to factor variable
training$y <- as.factor(training$y)
testing$y <- as.factor(testing$y)

# fit a random forest model and predict on the testing data
set.seed(33833)
fit01 <- train(y~. , data = training, method = "rf")
prediction01 <- predict(fit01, testing)

# fit a gradient boosting modeland predict on the testing data
set.seed(33833)
fit02 <- train(y~. , data = training, method = "gbm", verbose = FALSE)
prediction02 <- predict(fit02, testing)

# check the model accuracies
acc01 <- confusionMatrix(prediction01, testing$y)
acc02 <- confusionMatrix(prediction02, testing$y)
acc03 <- confusionMatrix(prediction01, prediction02)

acc01$overall["Accuracy"]
acc02$overall["Accuracy"]
acc03$overall["Accuracy"]
