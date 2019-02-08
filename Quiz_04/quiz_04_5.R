set.seed(3523)

# load the library and required data set
library(AppliedPredictiveModeling)
data(concrete)
library(caret)

# create the training and testing data sets
inTrain <- createDataPartition(concrete$CompressiveStrength, p=3/4)[[1]]
training <- concrete[inTrain, ]
testing <- concrete[-inTrain, ]

set.seed(325)

# install the SVM library
library(e1071)

# build the SVM model and predict the outcomes
fit01 <- svm(CompressiveStrength~. , data = training)
predict01 <- predict(fit01, testing)

# now check the accuracy to find the RMSE
acc01 <- accuracy(predict01, testing$CompressiveStrength)
acc01
