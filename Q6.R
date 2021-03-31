#Q6 Penicillin Data Set 
load("datasets.RData")
library("lme4")

head(penicillin)

model2 <- lmer(yield~treat+(1|blend),data=penicillin)
summary(model2)








