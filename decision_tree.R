library(mosaic)
library(haven)
library(rpart)
library(rpart.plot)

# run the model
tree1 = rpart(
    DEATH_EVENT ~ serum_sodium + serum_creatinine + age + ejection_fraction,
    data=train,
    method="class"
)

rpart.plot(
  tree1,
  digits=4,
  nn=TRUE,
  roundint=FALSE,
  type=2,
  cex=.7
)

# get accuracy of the model

# get training accuracy

tree_train <- mutate(train, predicted=as.numeric(predict(tree1, train, type="class")))
tree_train <- mutate(tree_train, predicted=as.numeric(recode(predicted, "1"="0", "2"="1")))

tree_test <- mutate(test, predicted=as.numeric(predict(tree1, test, type="class")))
tree_test <- mutate(tree_test, predicted=as.numeric(recode(predicted, "1"="0", "2"="1")))

train_accuracy_ctree = mean(tree_train$predicted == train$DEATH_EVENT)
test_accuracy_ctree = mean(tree_test$predicted == test$DEATH_EVENT)
