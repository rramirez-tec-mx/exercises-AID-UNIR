x <- c(0,0.25,0.5,1,2,3,4,5,6,8,10)
y <- c(2.5,3.6,5.3,9.5,14.0,16.5,18.8,21.5,23.2,26.8,28.4)
# Give the chart file a name.
#png(file = "nls.png")
#plot points
plot(x,y,type="l")
#create linear model
fahmy.lm=lm(y~x)
#verify parameters
summary(fahmy.lm)
cor(y,predict(fahmy.lm))^2
confint(fahmy.lm)
#plot model vs prediction
plot(x,y)
points(x,predict(fahmy.lm),type="l")

#create cuadratic model
fahmy.cuad=lm(y~1+x+I(x^2))
summary(fahmy.cuad)
confint(fahmy.cuad)

#plot model vs prediction
plot(x,y)
points(x,predict(fahmy.cuad),type="l")

#create cubic model
fahmy.cub=lm(y~1+x+I(x^2)+I(x^3))
summary(fahmy.cub)
confint(fahmy.cub)

#plot model vs prediction
plot(x,y)
points(x,predict(fahmy.cub),type="l")

#plotting residuals for all models
plot(fahmy.lm$residuals)
plot(fahmy.cuad$residuals)
plot(fahmy.cub$residuals)

#fit a non-linear model y=C(1-exp(-a*x))
#fahmy.nlm<-nls(y~a*(1-exp(-b*x)))
fahmy.nlm<-nls(y~a*(1-exp(-b*x)),start=list(a=10,b=1))
summary(fahmy.nlm)
confint(fahmy.nlm)
cor(y,predict(fahmy.nlm))^2
plot(x,(y-predict(fahmy.nlm)))

#adding new values to the prediction
new.days <- data.frame(x = c(12, 16, 21))
predict(fahmy.nlm, newdata = new.days)

#plotting new residuals
new.x<-c(12, 16, 21)
new.mgr  <- predict(fahmy.nlm, newdata = new.days)
act.mgr <- c(28.4,28.5,29.5)
plot(new.x,act.mgr-new.mgr)
