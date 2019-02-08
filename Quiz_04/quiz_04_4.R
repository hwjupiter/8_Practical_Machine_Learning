#install.packages("lubridate")
library(lubridate)  # for the year() function used below

#load the data and create the training and testing data sets
dat = read.csv("~/R Exercises/8_Practical_Machine_Learning/Quiz_04/gaData.csv")
training = dat[year(dat$date) < 2012, ]
testing <- dat[(year(dat$date)) > 2011, ]
tstrain = ts(training$visitsTumblr)

# load the necessary library
library(forecast)

# build the BATS model and fit it to the training data
fit01 <- bats(tstrain)
# do a forecast using the new model
fore01 <- forecast(fit01, nrow(testing))
plot(fore01)

# identify the upper and lower prediction boundaries
fore01_upper <- fore01$upper[,2]
fore01_upper
fore01_lower <- fore01$lower[,2]
fore01_lower

# identify how many reading s fall bewteen the upper and lower limits
table((testing$visitsTumblr > fore01_lower) & (testing$visitsTumblr < fore01_upper))

# to get the percentage, divide the number in the TRUE column of the table by
# the number of rows in the testing data set