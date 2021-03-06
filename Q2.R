install.packages("mgcv")
library(tidyverse)
library(mgcv)
load("datasets.RData")
# V Similar to Global-Temp Example

head(carbonD)
plot(carbonD$timeStep, carbonD$co2, main="CO2 COncentration over time in Hawai 1959-97")

par(mfrow=c(2,2))
model1 <- gam(co2~s(timeStep,k=12,bs="cs")+s(month,bs="cc",k=6),
              data=carbonD,family=Gamma(link="inverse"))
plot(model1)

par(mfrow=c(1,1))
xts <- seq(min(carbonD$timeStep),max(carbonD$timeStep),length=468)
xmon <- rep(1:12,39)
yfitAM <- predict(model1,newdata=data.frame(timeStep=xts,month=xmon))
plot(carbonD$timeStep,carbonD$co2, pch=1)
lines(xts, 1/(yfitAM),lwd=3,lty=1,col="blue")

par(mfrow=c(2,2))
gam.check(model1)







