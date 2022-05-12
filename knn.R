library(caret)

# ensure the dependent variable is loaded as a factor
train_factor <- mutate(train, DEATH_EVENT = as.factor(DEATH_EVENT)) 

test_factor <- mutate(test, DEATH_EVENT = as.factor(DEATH_EVENT)) 


# create level variables for 0 and 1 categories
levels(train_factor$DEATH_EVENT) <- make.names(levels(factor(train_factor$DEATH_EVENT)))

knn1 = train(
        DEATH_EVENT ~ serum_sodium + serum_creatinine + age + ejection_fraction,
        data = train_factor,
        method = "knn",
        preProcess=c("center", "scale"),
        tuneLength=15
)

plot(knn1)

# predict training and tests sets
train_predictions_knn = predict(knn1, train, type="prob") %>% round()
test_predictions_knn = predict(knn1, test, type="prob") %>% round()


# calculate accuracy on train set
knn_train_acc = mean(train_predictions_knn$X1 == train$DEATH_EVENT)

# calculate accuracy on test set
knn_test_acc = mean(test_predictions_knn$X1 == test$DEATH_EVENT)