# import data
library(openxlsx)
data1 = read.xlsx("marks2025.xlsx")

# move 'final' variable to first column
library(dplyr)
data = data1 %>% dplyr::select(final, everything())

# create scatterplot matrix
pairs(data, col = "darkred", pch = 16, cex = 0.7, main = "Scatterplot Matrix for Final Mark")

# check correlation
cor(data)

#### model 1

# create model1
model1 = lm(final ~ . - missed, data)
summary(model1)

# plot residuals for model1
library(car)
residualPlots(model1, ylab = "Residuals", col = "darkred", pch = 16, 
              cex = 0.7)

# CR plot for model1
crPlots(model1, col = "darkred", pch = 16, 
        cex = 0.7)

# standardised residuals for model1
plot(rstandard(model1), col = "darkred", pch = 16, 
     main = "Standardised Residuals Plot", xlab = "Fitted Values", ylab = "Residuals",
     las = 1, cex = .7, ylim = c(-3.5,3.5))
abline(h = c(-3, 0,3), col = "blue", lty = c(2,1))

# VIF test
vif(model1)

# stepwise AIC
null = lm(final ~ 1, data = data)

library(MASS) 
stepAIC(null, scope = list(upper = model1, lower = null, raw = TRUE), 
        data = data, direction = "forward")

stepAIC(model1, scope = list(upper = model1, lower = null, raw = TRUE), 
        data = data, direction = "backward")

stepAIC(model1, scope = list(upper = model1, lower = null, raw = TRUE), 
        data = data, directin = "both")

#### model 2

# create model2
model2 = lm(final ~ I(finalfirst^3) + atndrte, data)
summary(model2)

# confidence intervals
confint(model2)

# predict using model2
prediction = data.frame(finalfirst =c(32, 20, 10), atndrte = c(100, 50, 6.25)) 
result2 = predict(model2, prediction)
print(result2)

# plot residuals for model2
library(car)
residualPlots(model2, ylab = "Residuals", col = "darkred", pch = 16, 
              cex = 0.7)

# CR plot for model2
crPlots(model2, col = "darkred", pch = 16, 
        cex = 0.7)

# standardised residuals for model2
plot(rstandard(model2), col = "darkred", pch = 16, 
     main = "Residuals Plot", 
     las = 1, cex = .7, ylim = c(-3.5,3.5))
abline(h = c(-3, 0,3), col = "blue", lty = c(2,1))

# identify outliers for model2
std_res = rstandard(model2)
which(abs(std_res) > 3)

# check Cook's distance for model2
plot(cooks.distance(model2), pch = 16, col = "darkred", cex = 0.7, ylim = c(0, 1.2))
abline(h = 1, col = "blue", lty = 2)

### tests of assumptions
# test for normality of residuals
shapiro.test(model2$residuals)

# test for homoscedasticity
library(lmtest)
bptest(model2)

# test for independence of residuals
car::durbinWatsonTest(model2)

#### model 3 (outliers removed)

# create model3
data2 = data[-c(7,498),]
model3 = lm(final ~ I(finalfirst^3) + atndrte, data2)
summary(model3)


