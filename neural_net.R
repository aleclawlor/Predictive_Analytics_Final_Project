library(mosaic)
library(haven)
library(neuralnet)
library(dplyr)

set.seed(2)

nn1 <- neuralnet(
  DEATH_EVENT ~ serum_sodium + serum_creatinine + age + ejection_fraction,
  act.fct="logistic",
  linear.output=FALSE,
  hidden=10,
  data=train_normalized
)

plot(nn1)

# predict training and tests sets
train_predictions_nn = predict(nn1, train_normalized, type="prob") %>% round()
test_predictions_nn = predict(nn1, test_normalized, type="prob") %>% round()


# calculate accuracy on train set
nn_train_acc = mean(train_predictions_nn == train_normalized$DEATH_EVENT)

# calculate accuracy on test set
nn_test_acc = mean(test_predictions_nn == test_normalized$DEATH_EVENT)
