# Question 1

# import data
library(openxlsx)
data1 = read.xlsx("asthma25.xlsx")

# plot data
pairs(data1, col = "darkred", pch = 16, cex = 0.7, main = "Scatterplot Matrix")

# model 1.1
model1.1 = lm(y~ age + x2 + x3, data1)
summary(model1.1)

# standardised residuals for model 1.1
plot(rstandard(model1.1), col = "darkred", pch = 16, 
     las = 1, cex = .7, ylim = c(-3.5,3.5))
abline(h = c(-3, 0,3), col = "blue", lty = c(2,1))

# plot residuals for model 1.1
library(car)
residualPlots(model1.1, ylab = "Residuals", col = "darkred", pch = 16, 
              cex = 0.7)

# outliers for model 1.1
std_res1 = rstandard(model1.1)
which(abs(std_res1) > 3)

# Cook's distance for model 1.1
plot(cooks.distance(model1.1), pch = 16, col = "darkred", cex = 0.7, ylim = c(0, 1.2))
abline(h = 1, col = "blue", lty = 2)

# VIF test for model 1.1
library(car)
vif(model1.1)

# test for normality of residuals for model 1.1
shapiro.test(model1.1$residuals)

# test for homoscedasticity for model 1.1
library(lmtest)
bptest(model1.1)

# test for independence of residuals for model 1.1
car::durbinWatsonTest(model1.1)

# model 1.2 - includes interaction effects
model1.2 = lm(y~ age + x2 + x3 + x2:age + x3:age, data1)
summary(model1.2)

# standardised residuals for model 1.2
plot(rstandard(model1.2), col = "darkred", pch = 16, 
     main = "Standardised Residuals Plot", 
     las = 1, cex = .7, ylim = c(-3.5,3.5))
abline(h = c(-3, 0,3), col = "blue", lty = c(2,1))

# plot residuals for model 1.2
library(car)
residualPlots(model1.2, ylab = "Residuals", col = "darkred", pch = 16, 
              cex = 0.7)

# Cook's distance for model 1.2
plot(cooks.distance(model1.2), pch = 16, col = "darkred", cex = 0.7, ylim = c(0, 1.2), main = "Cook's Distance")
abline(h = 1, col = "blue", lty = 2)

# outliers for model 1.2
std_res2 = rstandard(model1.2)
which(abs(std_res2) > 3)

# VIF test for model 1.2
library(car)
vif(model1.2)

# test for normality of residuals for model 1.2
shapiro.test(model1.2$residuals)

# test for homoscedasticity for model 1.2
library(lmtest)
bptest(model1.2)

# test for independence of residuals for model 1.2
car::durbinWatsonTest(model1.2)

# predict using model 1.2
prediction1 = data.frame(age =c(30, 30, 30), x2 = c(0, 1, 0), x3 = c(0, 0, 1)) 
result1 = predict(model1.2, prediction1)
print(result1)

prediction2 = data.frame(age =c(65, 65, 65), x2 = c(0, 1, 0), x3 = c(0, 0, 1)) 
result2 = predict(model1.2, prediction2)
print(result2)

anova(model1.1, model1.2)

###################################################################################################
# Question 2

# import data
library(openxlsx)
data2 = read.xlsx("timber25.xlsx")

# model 2.1
model2.1 = lm(volume~ diameter, data2)
summary(model2.1)

# plot data
plot(data2, col = "darkred", pch = 16, cex = 0.7, main = "Scatterplot")
abline(model2.1)

# standardised residuals for model 2.1
plot(rstandard(model2.1), col = "darkred", pch = 16, 
     main = "Residuals Plot", 
     las = 1, cex = .7, ylim = c(-3.5,3.5))
abline(h = c(-3, 0,3), col = "blue", lty = c(2,1))

# histogram for model 2.1 residuals
hist(model2.1$residuals)

# plot residuals for model 2.1
library(car)
residualPlots(model2.1, ylab = "Residuals", col = "darkred", pch = 16, 
              cex = 0.7)

# CR plot for model 2.1
crPlots(model2.1, col = "darkred", pch = 16, 
        cex = 0.7)

# identify outliers for model 2.1
std_res3 = rstandard(model2.1)
which(abs(std_res3) > 3)

# check Cook's distance for model 2.1
plot(cooks.distance(model2.1), pch = 16, col = "darkred", cex = 0.7, ylim = c(0, 1.2))
abline(h = 1, col = "blue", lty = 2)

# test for normality of residuals for model 2.1
shapiro.test(model2.1$residuals)

# test for homoscedasticity for model 2.1
library(lmtest)
bptest(model2.1)

# test for independence of residuals for model 2.1
car::durbinWatsonTest(model2.1)

# box cox
library(MASS)
bc = boxcox(model2.1, lambda = seq(-2, 2, by = 0.1))
best_lambda = bc$x[which.max(bc$y)]
print(best_lambda)

# model 2.2 - using results of box cox
model2.2 = lm(volume^(.4)~diameter, data2)
summary(model2.2)

# standardised residuals for model 2.2
plot(rstandard(model2.2), col = "darkred", pch = 16, 
     main = "Standardised Residuals", 
     las = 1, cex = .7, ylim = c(-3.5,3.5))
abline(h = c(-3, 0,3), col = "blue", lty = c(2,1))

# histogram for model 2.2 residuals
hist(model2.2$residuals)

# plot residuals for model 2.2
library(car)
residualPlots(model2.2, ylab = "Residuals", col = "darkred", pch = 16, 
              cex = 0.7)

# CR plot for model 2.2
crPlots(model2.2, col = "darkred", pch = 16, 
        cex = 0.7)

# identify outliers for model 2.2
std_res4 = rstandard(model2.2)
which(abs(std_res4) > 3)

# check Cook's distance for model 2.2
plot(cooks.distance(model2.2), pch = 16, col = "darkred", cex = 0.7, 
     ylim = c(0, 1.2), main = "Cook's Distance")
abline(h = 1, col = "blue", lty = 2)

# test for normality of residuals for model 2.2
shapiro.test(model2.2$residuals)

# test for homoscedasticity for model 2.2
library(lmtest)
bptest(model2.2)

# test for independence of residuals for model 2.2
car::durbinWatsonTest(model2.2)

# predict using model 2.2
prediction3 = data.frame(diameter =c(5, 10, 20)) 
result3 = predict(model2.2, prediction3)
result3 = result3^2.5
print(result3)

# model 2.3 - transform response variable
model2.3 = lm(sqrt(volume)~diameter, data2)
summary(model2.3)

plot(sqrt(data2$volume), data2$diameter) ### try this plot as well

# histogram for model 2.3 residuals
hist(model2.3$residuals, main = "Histogram of Residuals", 
     xlab = "Residuals", las = 1, xlim = c(-1.25, 1.25))

# standardised residuals for model 2.3
plot(rstandard(model2.3), col = "darkred", pch = 16, 
     main = "Standardised Residuals", 
     las = 1, cex = .7, ylim = c(-3.5,3.5))
abline(h = c(-3, 0,3), col = "blue", lty = c(2,1))

# plot residuals for model 2.3
library(car)
residualPlots(model2.3, ylab = "Residuals", col = "darkred", pch = 16, 
              cex = 0.7)

# CR plot for model 2.3
crPlots(model2.3, col = "darkred", pch = 16, 
        cex = 0.7)

# identify outliers for model 2.3
std_res5 = rstandard(model2.3)
which(abs(std_res5) > 3)

# check Cook's distance for model 2.3
plot(cooks.distance(model2.3), pch = 16, col = "darkred", cex = 0.7, 
     ylim = c(0, 1.2), main = "Cook's distance")
abline(h = 1, col = "blue", lty = 2)

# test for normality of residuals for model 2.3
shapiro.test(model2.3$residuals)

# test for homoscedasticity for model 2.3
library(lmtest)
bptest(model2.3)

# test for independence of residuals for model 2.3
set.seed(111)
car::durbinWatsonTest(model2.3)

# predict using model 2.3
prediction4 = data.frame(diameter =c(5, 10, 20)) 
result4 = predict(model2.3, prediction4)
result4 = result4^2
print(result4)


#model 2.4 added after results received
model2.4 = lm(log(volume)~ log(diameter), data2)
summary(model2.4)

# plot data
plot(log(data2$volume), log(data2$diameter)) ###

# standardised residuals for model 2.4
plot(rstandard(model2.4), col = "darkred", pch = 16, 
     main = "Residuals Plot", 
     las = 1, cex = .7, ylim = c(-3.5,3.5))
abline(h = c(-3, 0,3), col = "blue", lty = c(2,1))

# histogram for model 2.4 residuals
hist(model2.4$residuals)

# plot residuals for model 2.4
library(car)
residualPlots(model2.4, ylab = "Residuals", col = "darkred", pch = 16, 
              cex = 0.7)

# CR plot for model 2.4
crPlots(model2.4, col = "darkred", pch = 16, 
        cex = 0.7)

###################################################################################################
# Question 3

# import data
library(openxlsx)
data3 = read.xlsx("blood25.xlsx")
library(dplyr)
data3 = data3 %>% dplyr::select(BP, everything())
data3 = data3[,-c(2)]

# plot data
pairs(data3, col = "darkred", pch = 16, cex = 0.7, main = "Scatterplot Matrix")

# correlation
cor(data3)

# model 3.1
model3.1 = lm(BP~ ., data3)
summary(model3.1)

# VIF test for model 3.1
library(car)
vif(model3.1)

# standardised residuals for model 3.1
plot(rstandard(model3.1), col = "darkred", pch = 16, 
     main = "Standardised Residuals Plot", 
     las = 1, cex = .7, ylim = c(-3.5,3.5))
abline(h = c(-3, 0,3), col = "blue", lty = c(2,1))

# histogram of residuals for model 3.1
hist(model3.1$residuals, main = "Histogram of Residuals", xlab = "Residuals")

# plot residuals for model 3.1
library(car)
residualPlots(model3.1, ylab = "Residuals", col = "darkred", pch = 16, 
              cex = 0.7)

# CR plot for model 3.1
crPlots(model3.1, col = "darkred", pch = 16, 
        cex = 0.7, main = "C+R plot")

# identify outliers for model 3.1
std_res6 = rstandard(model3.1)
which(abs(std_res6) > 3)

# check Cook's distance for model 3.1
plot(cooks.distance(model3.1), pch = 16, col = "darkred", cex = 0.7, 
     ylim = c(0, 1.2),, main = "Cook's Distance")
abline(h = 1, col = "blue", lty = 2)

# test for normality of residuals for model 3.1
shapiro.test(model3.1$residuals)

# test for homoscedasticity for model 3.1
library(lmtest)
bptest(model3.1)

# test for independence of residuals for model 3.1
car::durbinWatsonTest(model3.1)

###

# model 3.2
model3.2 = lm(BP~ + Age + Weight + Pulse, data3)
summary(model3.2)

# VIF test for model 3.2
library(car)
vif(model3.2)

# standardised residuals for model 3.2
plot(rstandard(model3.2), col = "darkred", pch = 16, 
     main = "Standardised Residuals Plot", 
     las = 1, cex = .7, ylim = c(-3.5,3.5))
abline(h = c(-3, 0,3), col = "blue", lty = c(2,1))

# histogram of residuals for model 3.2
hist(model3.2$residuals, main = "Histogram of Residuals", xlab = "Residuals", xlim = c(-1.25, 1.25))

# plot residuals for model 3.2
library(car)
residualPlots(model3.2, ylab = "Residuals", col = "darkred", pch = 16, 
              cex = 0.7)

# CR plot for model 3.2
dev.off()
crPlots(model3.2, col = "darkred", pch = 16, 
        cex = 0.7, main = "C+R plot")

# identify outliers for model 3.2
std_res7 = rstandard(model3.2)
which(abs(std_res7) > 3)

# check Cook's distance for model 3.2
plot(cooks.distance(model3.2), pch = 16, col = "darkred", cex = 0.7, 
     ylim = c(0, 1.2),, main = "Cook's Distance")
abline(h = 1, col = "blue", lty = 2)

# test for normality of residuals for model 3.2
shapiro.test(model3.2$residuals)

# test for homoscedasticity for model 3.2
library(lmtest)
bptest(model3.2)

# test for independence of residuals for model 3.2
car::durbinWatsonTest(model3.2)  

###

# model 3.3
model3.3 = lm(BP~ Age + Weight + Pulse + Stress, data3)
summary(model3.3)

# VIF test for model 3.3
library(car)
vif(model3.3)

# standardised residuals for model 3.3
plot(rstandard(model3.3), col = "darkred", pch = 16, 
     main = "Standardised Residuals Plot", 
     las = 1, cex = .7, ylim = c(-3.5,3.5))
abline(h = c(-3, 0,3), col = "blue", lty = c(2,1))

# histogram of residuals for model 3.3
hist(model3.3$residuals, main = "Histogram of Residuals", xlab = "Residuals")

# plot residuals for model 3.3
library(car)
residualPlots(model3.3, ylab = "Residuals", col = "darkred", pch = 16, 
              cex = 0.7)

# CR plot for model 3.3
crPlots(model3.3, col = "darkred", pch = 16, 
        cex = 0.7, main = "C+R plot")

# identify outliers for model 3.3
std_res8 = rstandard(model3.3)
which(abs(std_res8) > 3)

# check Cook's distance for model 3.3
plot(cooks.distance(model3.3), pch = 16, col = "darkred", cex = 0.7, 
     ylim = c(0, 1.2),, main = "Cook's Distance")
abline(h = 1, col = "blue", lty = 2)

# test for normality of residuals for model 3.3
shapiro.test(model3.3$residuals)

# test for homoscedasticity for model 3.3
library(lmtest)
bptest(model3.3)

# test for independence of residuals for model 3.3
car::durbinWatsonTest(model3.3)

# predict using model 3.3
prediction5 = data.frame(Age =c(45, 50, 55), Weight = c(90, 95, 100), 
                         Pulse = c(65, 70, 75), Stress = c(10, 50, 100)) 
result5 = predict(model3.3, prediction5)
result5 = result5
print(result5)

# compare against stepAIC
null = lm(BP ~ 1, data = data3)

library(MASS) 
stepAIC(null, scope = list(upper = model3.1, lower = null, raw = TRUE), 
        data = data3, direction = "forward")

stepAIC(model3.1, scope = list(upper = model3.1, lower = null, raw = TRUE), 
        data = data3, direction = "backward")

stepAIC(model3.1, scope = list(upper = model3.1, lower = null, raw = TRUE), 
        data = data3, directin = "both")


