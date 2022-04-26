#heart health
library(mosaic)
library(haven)
library(car)
library(caret)

# desription of each variable
## anaemia: Decrease of red blood cells or hemoglobin (boolean)
## creatinine_phosphorylation: Level of the CPK enzyme in the blood (mcg/L)
## diabetes: If the patient has diabetes (boolean)
## ejection_fraction: Percentage of blood leaving the heart at each contraction (percentage)
## high_blood_pressure: If the patient has hypertension (boolean)
## platelets: Platelets in the blood (kiloplatelets/mL)
## serum_creatinine: Level of serum creatinine in the blood (mg/dL)
## serum_sodium: Level of serum sodium in the blood (mEq/L)
## sex: Woman or man (binary)
## smoking: If the patient smokes or not (boolean)
## time: Follow-up period (days) #irrelevent
## DEATH_EVENT: If the patient deceased during the follow-up period (boolean)

#data dictionary:
# Sex - Gender of patient Male = 1, Female =0
# Age - Age of patient
# Diabetes - 0 = No, 1 = Yes
# Anaemia - 0 = No, 1 = Yes
# High_blood_pressure - 0 = No, 1 = Yes
# Smoking - 0 = No, 1 = Yes
# DEATH_EVENT - 0 = No, 1 = Yes

#testing which variables affect probability of death
favstats(~DEATH_EVENT, data= heart_failure_clinical_records_dataset)
# min Q1 median Q3 max      mean        sd   n missing
#0  0      0  1   1 0.3210702 0.4676704 299       0
tally(~DEATH_EVENT, data= heart_failure_clinical_records_dataset)


t.test(DEATH_EVENT~anaemia, data=heart_failure_clinical_records_dataset, mu=0, alternative="l")
#	Welch Two Sample t-test

#data:  DEATH_EVENT by anaemia
#t = -1.1366, df = 268.17, p-value = 0.1284
#alternative hypothesis: true difference in means between group 0 and group 1 is less than 0
#95 percent confidence interval:
#  -Inf 0.02824787
#sample estimates:
#  mean in group 0 mean in group 1 
#0.2941176       0.3565891 

t.test(DEATH_EVENT~diabetes, data=heart_failure_clinical_records_dataset, mu=0)
#data:  DEATH_EVENT by diabetes
#t = 0.033485, df = 267.34, p-value = 0.9733
#alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
#95 percent confidence interval:
#  -0.1062960  0.1099742
#sample estimates:
#  mean in group 0 mean in group 1 
#0.3218391       0.3200000 


t.test(DEATH_EVENT~high_blood_pressure, data=heart_failure_clinical_records_dataset, mu=0, alternative="l")
#data:  DEATH_EVENT by high_blood_pressure
#t = -1.347, df = 202.44, p-value = 0.08974
#alternative hypothesis: true difference in means between group 0 and group 1 is less than 0
#95 percent confidence interval:
#  -Inf 0.01759657
#sample estimates:
#  mean in group 0 mean in group 1 
#0.2938144       0.3714286 

t.test(DEATH_EVENT~smoking, data=heart_failure_clinical_records_dataset, mu=0, alternative="l")
#data:  DEATH_EVENT by smoking
#t = 0.21817, df = 187.8, p-value = 0.5862
#alternative hypothesis: true difference in means between group 0 and group 1 is less than 0
#95 percent confidence interval:
#  -Inf 0.1082656
#sample estimates:
#  mean in group 0 mean in group 1 
#0.3251232       0.3125000 


## scientific inquiry: levels of which compoud has the highest death rate
t.test(creatinine_phosphokinase~DEATH_EVENT, data=heart_failure_clinical_records_dataset, mu=0, alternative="l")
#data:  creatinine_phosphokinase by DEATH_EVENT
#t = -0.90119, df = 125.32, p-value = 0.1846
#alternative hypothesis: true difference in means between group 0 and group 1 is less than 0
#95 percent confidence interval:
#  -Inf 109.1639
#sample estimates:
#  mean in group 0 mean in group 1 
#540.0542        670.1979 

t.test(platelets~DEATH_EVENT, data=heart_failure_clinical_records_dataset, mu=0, alternative="g")
#data:  platelets by DEATH_EVENT
#t = 0.84479, df = 184.79, p-value = 0.1997
#alternative hypothesis: true difference in means between group 0 and group 1 is greater than 0
#95 percent confidence interval:
#  -9833.3     Inf
#sample estimates:
#  mean in group 0 mean in group 1 
#266657.5        256381.0 

t.test(serum_creatinine~DEATH_EVENT, data=heart_failure_clinical_records_dataset, mu=0, alternative="l") # serum_creatinine and death event has a great p-value
t.test(serum_sodium~DEATH_EVENT, data=heart_failure_clinical_records_dataset, mu=0, alternative="g") # good p-value
##### severity of the heart attack
t.test(ejection_fraction~DEATH_EVENT, data=heart_failure_clinical_records_dataset, mu=0, alternative="g") # great p-value
t.test(age~DEATH_EVENT, data=heart_failure_clinical_records_dataset, mu=0)

#seeing if sex affects different levels 
t.test(diabetes~sex, data=heart_failure_clinical_records_dataset, mu=0, alternative="g") 
t.test(high_blood_pressure~sex, data=heart_failure_clinical_records_dataset, mu=0, alternative="g")


# regression models

gf_point(serum_creatinine~age, data=heart_failure_clinical_records_dataset, alpha=0.5) %>% gf_lm()
model1 <- lm(serum_creatinine~age+DEATH_EVENT+serum_sodium, data=heart_failure_clinical_records_dataset)
msummary(model1)

binarymodel <- glm(DEATH_EVENT~age, data=heart_failure_clinical_records_dataset)
msummary(binarymodel)
binarymodel2 <- glm(DEATH_EVENT~age+serum_creatinine+serum_sodium+ejection_fraction, data=heart_failure_clinical_records_dataset)
msummary(binarymodel2) #the best regression model

binarymodel3 <- glm(DEATH_EVENT~age+serum_creatinine+serum_sodium+ejection_fraction+creatinine_phosphokinase, data=heart_failure_clinical_records_dataset)
msummary(binarymodel3) 



heart_failure_clinical_records_dataset1 <- mutate(heart_failure_clinical_records_dataset, DEATH_EVENT = as.factor(DEATH_EVENT))

levels(heart_failure_clinical_records_dataset1$DEATH_EVENT) <- make.names(levels(factor(heart_failure_clinical_records_dataset1$DEATH_EVENT))) 

kNNModelName <- train(DEATH_EVENT ~ .-training, data = heart_failure_clinical_records_dataset1[heart_failure_clinical_records_dataset1$training==1, 1:12], method = "knn", preProcess=c("center", "scale"), tuneLength = 15) 

plot(kNNModelName)

MortalityRate <- predict(kNNMortalityRateModel, heart_failure_clinical_records_dataset1, type = "prob") %>% round() 
MR3 <- mutate(heart_failure_clinical_records_dataset1, predicted = MortalityRate$X1) 
Train_Mortality_Rate_a <- 
  NewDataName$YVariableName[NewDataName$training==1] 




pred_MortalityRate <- predict(kNNMortalityRate, heart_failure_clinical_records_dataset1, type = "prob") %>% round() 
heartfailure_2 <- mutate(heart_failure_clinical_records_dataset1, predicted = DEATH_EVENT$X1) 

TrainActualName <- heartfailure_2$DEATH_EVENT[heartfailure_2$training==1]
TrainPredictMortalityRate <- heartfailure_2$predicted[heartfailure_2$training==1] 
 
mean(TrainActualName==TrainPredictName) 

  ˃ TestActualName <- 
  heartfailure_2$YVariableName[heartfailure_2$training==0] 
˃ TestPredictName <- 
  heartfailure_2$predicted[heartfailure_2$training==0] 
˃ mean(TestActualName==TestPredictName) 




pred_val <- predict(knnmodel1, NS1, type = "prob") %>% 
  round() 
˃ NS3 <- mutate(NS1, predicted = pred_val$X1) 
˃ train_a <- NS3$default[NS3$training==1] 
˃ train_p <- NS3$predicted[NS3$training==1] 
˃ mean(train_a==train_p) #gives you the % correct for 
training data 
˃ test_a <- NS3$default[NS3$training==0] 
˃ test_p <- NS3$predicted[NS3$training==0] 
˃ mean(test_a==test_p) #gives you the % correct for test data 





nullmodel <- glm(DEATH_EVENT ~ 1, data = heart_failure_clinical_records_dataset, family = binomial) 
fullmodel <- glm(DEATH_EVENT ~ ., data = heart_failure_clinical_records_dataset, family = binomial) 
stepmodel <- step(nullmodel, scope = list(upper = fullmodel))

msummary(stepmodel)


NullDeathPred <- glm(DEATH_EVENT ~ 1, data = 
                       heart_failure_clinical_records_dataset[heart_failure_clinical_records_dataset$training == 1, 1:12], 
                     family=binomial) 


