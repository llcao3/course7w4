#Course 7 Quiz 4

#Q1
library(MASS)
str(shuttle)
shuttle$use.binary <- as.integer(shuttle$use=="auto")
fit1 <- glm(use.binary~wind, data=shuttle, family=binomial)
summary(fit1)$coef
coef(fit1)
1/exp(coef(fit1[2])

#Q2
fit2 <- glm(use.binary~wind+magn, data=shuttle, family=binomial)
summary(fit2)$coef
coef(fit2)
1/exp(coef(fit2[2]))

#Q3
fit3 <- glm(1-use.binary~wind, data=shuttle, family=binomial)
summary(fit3)$coef

#Q4
fit4 <- glm(count~spray, data=InsectSprays,family=possion)
summary(fit4)
1/exp(coef(fit4)[2])
      
#Q6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
plot(x,y)
#knots <- c(0)
#splineTerms <- sapply(knots,function(knot)(x>knot)*(x-knot))
#x1<- cbind(1,x,splineTerms)
#fit6 <- lm(y~x1)
#y1 <- predict(fit6)
#lines(x,y1)
#y1
#(y1[11]-y[6])/(x[11]-x[6])
z <- (x>0) * x
fit6 <- lm(y~x+z)
y1<- predict(fit6)
lines(x,y1)
sum(coef(fit6)[2:3])
sum
      
