#data(mtcars)
#help(mtcars)

mtcars$cyl <- factor (mtcars$cyl)
mtcars$vs   <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am   <- factor(mtcars$am,labels=c("Automatic","Manual"))


fit <- lm(mpg~am, mtcars)
summary(fit)

fitall <- lm(mpg~.,mtcars)
summary(fitall)

fitbest <- step(fitall, direction ="both")
summary(fitbest)

anova(fitbest, fit, fitall)


par(mfrow=c(2, 2))
plot(fitbest)

hatvalues(fitbest)
dfbetas(fitbest)$amManual

t.test(mpg ~ am, data = mtcars)

#plots
pairs(mpg ~ ., data = mtcars)

library(sm)
sm.density.compare(mtcars$mpg, mtcars$am, xlab="Miles Per Gallon")
title(main="MPG Distribution by Car Type Transmission")

# add legend via mouse click
legend("topright", legend=c("Auto", "Manual"),  text.col = c("red", "green"))


boxplot(mpg ~ am, data = mtcars, col = (c("red","blue")), ylab = "Miles Per Gallon", xlab = "Transmission Type")
