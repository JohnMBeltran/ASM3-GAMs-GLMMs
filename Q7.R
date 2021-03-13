# Q7 pupils data Set
load("datasets.RData")


#Part A) Normal Lienar GLM 
head(pupils)
model1 <- glm(test~IQ+ses+Class, data=pupils, family=gaussian("identity"))
summary(model1)
drop1(model1, test="Chisq")
