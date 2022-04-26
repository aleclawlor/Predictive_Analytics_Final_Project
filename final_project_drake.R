#heart health

library(mosaic)
library(haven)
library(car)

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
## time: Follow-up period (days)
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
t.test(DEATH_EVENT~anaemia, data=heart_failure_clinical_records_dataset, mu=0, alternative="l")
t.test(DEATH_EVENT~diabetes, data=heart_failure_clinical_records_dataset, mu=0)
t.test(DEATH_EVENT~high_blood_pressure, data=heart_failure_clinical_records_dataset, mu=0, alternative="l")
t.test(DEATH_EVENT~smoking, data=heart_failure_clinical_records_dataset, mu=0, alternative="l")
# anova test with it
anova2 <- aov(DEATH_EVENT~anaemia+high_blood_pressure, data=heart_failure_clinical_records_dataset)
summary(anova2)


## scientific inquiry: levels of which compoud has the highest death rate
t.test(creatinine_phosphokinase~DEATH_EVENT, data=heart_failure_clinical_records_dataset, mu=0, alternative="l")
t.test(platelets~DEATH_EVENT, data=heart_failure_clinical_records_dataset, mu=0, alternative="g")
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

TestActualName <- heartfailure_2$YVariableName[heartfailure_2$training==0] 
TestPredictName <- heartfailure_2$predicted[heartfailure_2$training==0] 
mean(TestActualName==TestPredictName) 


nullmodel <- glm(DEATH_EVENT ~ 1, data = heart_failure_clinical_records_dataset, family = binomial) 
fullmodel <- glm(DEATH_EVENT ~ ., data = heart_failure_clinical_records_dataset, family = binomial) 
stepmodel <- step(nullmodel, scope = list(upper = fullmodel))

msummary(stepmodel)













