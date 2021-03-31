# Fit a GAM & describe aids cases
library(tidyverse)
library(mgcv)
load("datasets.RData")
head(aids)
nrow(aids)

plot(aids$date, aids$cases)

#Model Development 

model1 <- gam(cases~s(date, bs="cs", k=5),data=aids,family=Gamma(link="identity"))
par(mfrow=c(2,2))
gam.check(model1)

par(mfrow=c(1,2))
qq.gam(model1, type = "deviance", rl.col = 2, rep.col = "gray80")
plot(napredict(model1$na.action, model1$linear.predictors), 
     residuals(model1, type = "deviance"), main = "Resids vs. linear pred.", 
     xlab = "linear predictor", ylab = "residuals")

pchisq(model1$deviance/model1$sig2,model1$df.residual,lower.tail=F)



## Part B:  Plotting Mean & Confidence Intervals Based on Model Above
par(mfrow=c(1,1))
xts <- seq(min(aids$date),max(aids$date),length=nrow(aids))
ypred <- predict(model1,newdata=data.frame(date=aids$date), se.fit=T)
plot(aids$date,aids$cases, pch=1, ylab="Cases of Aids", xlab="Date")
lines(xts,(ypred$fit),lwd=1.5,lty=1,col="blue")
upper <- ypred$fit+1.96*ypred$se.fit
lower <- ypred$fit-1.96*ypred$se.fit
lines(xts, (ypred$fit+1.96*ypred$se.fit), col='purple', lty=2)
lines(xts, (ypred$fit-1.96*ypred$se.fit), col='purple', lty=2)



