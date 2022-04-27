#ANOVA TESTS



anova1 <- aov(DEATH_EVENT~as.factor(sex)*as.factor(smoking), data=heart_dataset)
summary(anova1)
TukeyHSD(anova1)
# shows an interesting result: someone who is a certain sex and smokes has a statistically different mortality rate

anova2 <- aov(DEATH_EVENT~as.factor(smoking)*as.factor(high_blood_pressure), data=heart_dataset)
summary(anova2)

anova3 <- aov(DEATH_EVENT~as.factor(anaemia)*as.factor(high_blood_pressure), data=heart_dataset)
summary(anova3)
