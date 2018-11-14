# https://www.youtube.com/watch?v=7Jbb2ItbTC4

library(C50)
install.packages("C50")
data(churn)

predictors <- names(churnTrain) [names(churnTrain) != "churn"]

set.seed(1)
inTrainingSet2 <- createDataPartition(allData$churn, p = 0.75, list = FALSE)
churnTrain <- allData[]