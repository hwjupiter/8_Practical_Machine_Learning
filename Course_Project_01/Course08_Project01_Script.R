# load the libraries
library(caret)
# read ion the data
training01 <- read.csv("~/R Exercises/8_Practical_Machine_Learning/Course_Project_01/pml-training.csv")
# remove variables not related to the measurements
training02 <- training01[,-(1:7)]
# identify vaiables with low variability
zeroVar <- nearZeroVar(training02)
# remove these variables
training03 <- training02[,-zeroVar]
# calculate the mean values of the NA values in the remaining variables
# note: these are exploratory - it doesn't really matter what cut-off value is chosen - the variables appear to
# either have numbers or be NAs so the lowest value of 0.1 can be used
# all_NA <- sapply(training03, function(x) mean(is.na(x))) > 0.80
# all_NA <- sapply(training03, function(x) mean(is.na(x))) > 0.40
# all_NA1 <- sapply(training03, function(x) mean(is.na(x))) > 0.80
# all_NA2 <- sapply(training03, function(x) mean(is.na(x))) > 0.40
all_NA3 <- sapply(training03, function(x) mean(is.na(x))) > 0.10
# all_NA4 <- sapply(training03, function(x) mean(is.na(x)))  # numbers only
# summary(all_NA4)
# remove the NA variables. training 04 is now the final data set for initial model building
training04 <- training03[,all_NA3==FALSE]
# set the seed
set.seed(1986)
# split the data into training and testing sets
inTrain <- createDataPartition(y=training04$classe, p=0.6, list=F)
final_train <- training04[inTrain,]
final_test <- training04[-inTrain,]
# use cross-validation as a fitting control for the model to prevent overfitting. Will use 5 fold
fit_Control <- trainControl(method = "cv", number = 5, verboseIter = FALSE)
# now train a random forest model on the training data det that was split out above
fit_model <- train(classe~., data = final_train, method = "rf", trControl = fit_Control)
fit_model$finalModel  # show the statistics
# now use the model to predict the outputs of the testing data set
# note: still only using final_test, NOT THE EVALUATION CSV
pred_model <- predict(fit_model, newdata = final_test)
# generate the confusion matrix to get the accuracy and error rates
confusionMatrix(final_test$classe, pred_model)
# accuracy = 0.9935 = 99.4%
# therefore error rate = 0.6%
# this is acceptable therefore will use this model
# before applying it to the final evaluation data, a new model must be constructed using these parameters.
# it must be constructed on the WHOLE training data set <- training04
# NOTE the evealution test data set must be processed in the same way as the training data set
testing01 <- read.csv("~/R Exercises/8_Practical_Machine_Learning/Course_Project_01/pml-testing.csv")
testing01 <- read.csv("~/R Exercises/8_Practical_Machine_Learning/Course_Project_01/pml-testing.csv")
testing02 <- testing01[,-(1:7)]
zeroVar02 <- nearZeroVar(testing02)
testing03 <- testing02[,-zeroVar02]
all_NA_test <- sapply(testing03, function(x) mean(is.na(x))) > 0.10
testing04 <- testing03[,all_NA_test==FALSE] # this is now the final testing data set
# note that at the end of processing both the training and testing data sets have the same number of variables
# now, fit the model to the complete training data set. The same fit_Control ethod will be used as previously
fit_model_01 <- train(classe~., data = training04, method = "rf", trControl = fit_Control)
fit_model_01$finalModel
# now run the predict function using the new model to predict the classe output of the full testing set
pred_model_01 <- predict(fit_model_01, newdata = testing04)
# this is now the final model and the final predictions for submitting. 
