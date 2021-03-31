install.packages("mgcv")
library(tidyverse)
library(mgcv)
load("datasets.RData")
# V Similar to Global-Temp Example

#Part A
head(carbonD)
par(mfrow=c(1,1))
plot(carbonD$timeStep, carbonD$co2, main="CO2 COncentration over time in Hawai 1959-97")

#Part B;Get a Good Fit: 1 DF between k' and EDF; k-index>=1, p-value non significant
model1 <- gam(co2~s(timeStep,k=90,bs="cs")+s(month,bs="cc",k=12),
              data=carbonD,family=Gamma(link="identity"))
par(mfrow=c(1,1))
qq.gam(model1, type = "deviance", rl.col = 2, rep.col = "gray80")
plot(napredict(model1$na.action, model1$linear.predictors), 
     residuals(model1, type = "deviance"), xlab = "linear predictor", ylab = "residuals")
gam.check(model1)
par(mfrow=c(1,2))
plot(model1)


#Part C
par(mfrow=c(1,1))
xts <- seq(min(carbonD$timeStep),max(carbonD$timeStep),length=nrow(carbonD))
xmon <- rep(1:12,nrow(carbonD)/12)
yfitAM <- predict(model1,newdata=data.frame(timeStep=xts,month=xmon))
plot(carbonD$timeStep,carbonD$co2, pch=1)
lines(xts, yfitAM,lwd=3,lty=1,col="blue")

  # Plottin g Individual Functions 
par(mfrow=c(1,2))
yfitAM2 <- predict(model1, type="terms")
plot(seq(min(carbonD$timeStep), max(carbonD$timeStep)), yfitAM2[,1], 
     lty=1, lwd=1, xlab = "TimeStep", ylab="s_1(timestep)", las=1, type="l")
plot(rep(1:12,nrow(carbonD)/12), yfitAM2[,2], type="l", lty=1, lwd=1, xlab="Month", ylab="s_2(month)", las=1)


#Part D
nextyr_jan <- max(carbonD$timeStep)+1
nextyr_dec <- max(carbonD$timeStep)+12
tst2 <- seq(from=nextyr_jan, to=nextyr_dec, by=1)
xmon2 <- rep(1:12,1)
new <- predict(model1, newdata=data.frame(timeStep=tst2,month=xmon2), se.fit=T)
plot(xmon2, new$fit, ylim = c(min(new$fit-1.96*new$se.fit), max(new$fit+1.96*new$se.fit)),
     ylab = "CO2 concentration (ppm)", xlab = "Month in 1998"); grid(); axis(1)
lines(xmon2,new$fit,lty=3,lwd=0.9)
lines(xmon2,new$fit+1.96*new$se.fit,lty=2,lwd=1)
lines(xmon2,new$fit-1.96*new$se.fit,lty=2,lwd=1)

n <- 38
plot(rep(1:12, 1), carbonD$co2[(1+n*12):(12+n*12)])





