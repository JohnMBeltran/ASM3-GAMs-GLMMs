# Fit a GAM & describe aids cases
head(aids)

plot(aids$date, aids$cases)
model1 <- gam(cases~s(date,k=100,bs="cs"),data=aids,family=poisson(link="log"))
summary(model1)
par(mfrow=c(2,2))
gam.check(model1)
model1$edf
