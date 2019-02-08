set.seed(3523)

# load the library and required data set
library(AppliedPredictiveModeling)
data(concrete)

# create the training and testing data sets
inTrain <- createDataPartition(concrete$CompressiveStrength, p=3/4)[[1]]
training <- concrete[inTrain, ]
testing <- concrete[-inTrain, ]

set.seed(233)
# fit the required model using the lasso method
fit01 <- train(CompressiveStrength~. , data = concrete, method = "lasso")
library(elasticnet)  # necessary to use the plot.enet function
plot.enet(fit01$finalModel, xvar = "penalty", use.color = TRUE)