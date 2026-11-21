# Question 1

# import data
library(openxlsx)
data1 = read.xlsx("injury25.xlsx")
data1 = data1[,-1]

data2 = data1[,-c(3,7)]
summary(data2)

# check for missing values
!complete.cases(data1)

# create table for fatalities vs non-fatalities
fatality_table = table(data1$fatality)
names(fatality_table) = c("Non-fatal", "Fatal")
print(fatality_table)

# scatterplot matrix
pairs(data1, pch = 16, cex = 0.7, col = "darkred", main = "Scatterplot Matrix")

# create models
model1.1 = glm(fatality ~ ., family = "binomial", data1)
summary(model1.1)

# stepwise AIC model 
model1.2 = step(model1.1, trace = 0)
summary(model1.2)

# model 1.3
model1.3 = glm(fatality ~ age + area + race + inhal , family = "binomial", data1)
summary(model1.3) # final model

# model 1.4
model1.4 = glm(fatality ~ age + area + inhal + flame + age:inhal + area:flame, 
               family = "binomial", data1)
summary(model1.4)

# model 1.5
model1.5 = glm(fatality ~ age + area + race + inhal + age:inhal, 
               family = "binomial", data1)
summary(model1.5)

# model 1.6
model1.6 = glm(fatality ~ age + area + race + inhal  + age:inhal + area:inhal, 
               family = "binomial", data1)
summary(model1.6)

# model 1.7
model1.7 = glm(fatality ~ age + area + race + inhal + flame + age:inhal + 
                 area:inhal + area:flame, family = "binomial", data1)
summary(model1.7)

# Calculate VIFs
library(car)
vif(model1.1)
vif(model1.3)

#classification tables
library(pscl)
hitmiss(model1.1)
hitmiss(model1.2)
hitmiss(model1.3) # final model
hitmiss(model1.4)
hitmiss(model1.5)
hitmiss(model1.6)
hitmiss(model1.7)

# calculate R^2
R2_1.7 = 1 - (model1.7$deviance / model1.7$null.deviance)
print(R2_1.7)
R2_1.6 = 1 - (model1.6$deviance / model1.6$null.deviance)
print(R2_1.6)
R2_1.5 = 1 - (model1.5$deviance / model1.5$null.deviance)
print(R2_1.5)
R2_1.4 = 1 - (model1.4$deviance / model1.6$null.deviance)
print(R2_1.4)
R2_1.3 = 1 - (model1.3$deviance / model1.3$null.deviance)  # final model
print(R2_1.3)
R2_1.2 = 1 - (model1.2$deviance / model1.2$null.deviance)
print(R2_1.2)
R2_1.1 = 1 - (model1.1$deviance / model1.1$null.deviance)
print(R2_1.1)

# use ANOVA to compare two models
anova(model1.2, model1.3, test = "LRT") # AIC model vs final model
anova(model1.6, model1.4, test = "LRT")
anova(model1.7, model1.3, test = "LRT")
anova(model1.7, model1.2, test = "LRT")

###
#Predict probability of fatality
predict1 = predict(model1.3, data.frame(age = 30, race = 1, area= 15, 
                               inhal = 1),type = "response")
predict2 = predict(model1.3, data.frame(age = 30, race = 1, area= 15, 
                             inhal = 0),type = "response")
predict3 = predict(model1.3, data.frame(age = 30, race = 0, area= 15, 
                             inhal = 1),type = "response")
predict4 = predict(model1.3, data.frame(age = 30, race = 0, area= 15, 
                              inhal = 0),type = "response")
predict5 = predict(model1.3, data.frame(age = 70, race = 1, area= 15, 
                             inhal = 1),type = "response")
predict6 = predict(model1.3, data.frame(age = 70, race = 1, area= 15, 
                             inhal = 0),type = "response")
predict7 = predict(model1.3, data.frame(age = 70, race = 0, area= 15, 
                             inhal = 1),type = "response")
predict8 = predict(model1.3, data.frame(age = 70, race = 0, area= 15, 
                             inhal = 0),type = "response")
predict9 = predict(model1.3, data.frame(age = 30, race = 1, area= 50, 
                             inhal = 1),type = "response")
predict10 = predict(model1.3, data.frame(age = 30, race = 1, area= 50, 
                             inhal = 0),type = "response")
predict11 = predict(model1.3, data.frame(age = 30, race = 0, area= 50, 
                             inhal = 1),type = "response")
predict12 = predict(model1.3, data.frame(age = 30, race = 0, area= 50, 
                             inhal = 0),type = "response")
predict13 = predict(model1.3, data.frame(age = 70, race = 1, area= 50, 
                             inhal = 1),type = "response")
predict14 = predict(model1.3, data.frame(age = 70, race = 1, area= 50, 
                             inhal = 0),type = "response")
predict15 = predict(model1.3, data.frame(age = 70, race = 0, area= 50, 
                             inhal = 1),type = "response")
predict16 = predict(model1.3, data.frame(age = 70, race = 0, area= 50, 
                             inhal = 0),type = "response")

# Step 1: Define the column titles
titles = c("Age", "Area", "White", "Inhale", "Probability")

# Step 2: Create an empty data frame with the column titles
table1 = data.frame(matrix(ncol = length(titles), nrow = 0))
colnames(table1) = titles

# Add a row to the table
row1 = data.frame(Age = 30, Area = 15, White = "Yes", Inhale = "Yes", Probability = predict1)
row2 = data.frame(Age = 30, Area = 15, White = "Yes", Inhale = "No", Probability = predict2)
row3 = data.frame(Age = 30, Area = 15, White = "No", Inhale = "Yes", Probability = predict3)
row4 = data.frame(Age = 30, Area = 15, White = "No", Inhale = "No", Probability = predict4)
row5 = data.frame(Age = 70, Area = 15, White = "Yes", Inhale = "Yes", Probability = predict5)
row6 = data.frame(Age = 70, Area = 15, White = "Yes", Inhale = "No", Probability = predict6)
row7 = data.frame(Age = 70, Area = 15, White = "No", Inhale = "Yes", Probability = predict7)
row8 = data.frame(Age = 70, Area = 15, White = "No", Inhale = "No", Probability = predict8)
row9 = data.frame(Age = 30, Area = 50, White = "Yes", Inhale = "Yes", Probability = predict9)
row10 = data.frame(Age = 30, Area = 50, White = "Yes", Inhale = "No", Probability = predict10)
row11 = data.frame(Age = 30, Area = 50, White = "No", Inhale = "Yes", Probability = predict11)
row12 = data.frame(Age = 30, Area = 50, White = "No", Inhale = "No", Probability = predict12)
row13 = data.frame(Age = 70, Area = 50, White = "Yes", Inhale = "Yes", Probability = predict13)
row14 = data.frame(Age = 70, Area = 50, White = "Yes", Inhale = "No", Probability = predict14)
row15 = data.frame(Age = 70, Area = 50, White = "No", Inhale = "Yes", Probability = predict15)
row16 = data.frame(Age = 70, Area = 50, White = "No", Inhale = "No", Probability = predict16)

table1 = rbind(table1, row1,row2,row3,row4,row5,row6,row7,row8,row9,
                         row10,row11,row12,row13,row14,row15,row16)
rownames(table1) = NULL

# Print the table
print(table1)
###

###
# Predict for model containing interaction terms (see predictions 15b to 18b)
predict1b = predict(model1.7, data.frame(age = 30, race = 1, area= 15, 
                                        inhal = 1, flame = 1),type = "response")
predict2b = predict(model1.7, data.frame(age = 30, race = 1, area= 15, 
                                        inhal = 0, flame = 0),type = "response")
predict3b = predict(model1.7, data.frame(age = 30, race = 0, area= 15, 
                                        inhal = 1, flame = 1),type = "response")
predict4b = predict(model1.7, data.frame(age = 30, race = 0, area= 15, 
                                        inhal = 0, flame = 0),type = "response")
predict5b = predict(model1.7, data.frame(age = 70, race = 1, area= 15, 
                                        inhal = 1, flame = 1),type = "response")
predict6b = predict(model1.7, data.frame(age = 70, race = 1, area= 15, 
                                        inhal = 0, flame = 0),type = "response")
predict7b = predict(model1.7, data.frame(age = 70, race = 0, area= 15, 
                                        inhal = 1, flame = 1),type = "response")
predict8b = predict(model1.7, data.frame(age = 70, race = 0, area= 15, 
                                        inhal = 0, flame = 0),type = "response")
predict9b = predict(model1.7, data.frame(age = 30, race = 1, area= 50, 
                                        inhal = 1, flame = 1),type = "response")
predict10b = predict(model1.7, data.frame(age = 30, race = 1, area= 50, 
                                         inhal = 0, flame = 0),type = "response")
predict11b = predict(model1.7, data.frame(age = 30, race = 0, area= 50, 
                                         inhal = 1, flame = 1),type = "response")
predict12b = predict(model1.7, data.frame(age = 30, race = 0, area= 50, 
                                         inhal = 0, flame = 0),type = "response")
predict13b = predict(model1.7, data.frame(age = 70, race = 1, area= 50, 
                                         inhal = 1, flame = 1),type = "response")
predict14b = predict(model1.7, data.frame(age = 70, race = 1, area= 50, 
                                         inhal = 0, flame = 0),type = "response")
predict15b = predict(model1.7, data.frame(age = 70, race = 0, area= 50, # prob. of fatality lower for inhale       
                                         inhal = 1, flame = 1),type = "response")
predict16b = predict(model1.7, data.frame(age = 70, race = 0, area= 50, # prob. of fatality lower for inhale
                                         inhal = 0, flame = 1),type = "response")
predict17b = predict(model1.7, data.frame(age = 70, race = 0, area= 50, # prob. of fatality lower for inhale
                                         inhal = 1, flame = 0),type = "response")
predict18b = predict(model1.7, data.frame(age = 70, race = 0, area= 50, # prob. of fatality lower for inhale
                                         inhal = 0, flame = 0),type = "response")
# Step 1: Define the column titles
titles2 = c("Age", "Area", "White", "Inhale", "Probability", "Flame")

# Step 2: Create an empty data frame with the column titles
table2 = data.frame(matrix(ncol = length(titles2), nrow = 0))
colnames(table2) = titles2

# Add a row to the table
row1b = data.frame(Age = 30, Area = 15, White = "Yes", Inhale = "Yes", Flame = "Yes", Probability = predict1b)
row2b = data.frame(Age = 30, Area = 15, White = "Yes", Inhale = "No", Flame = "No", Probability = predict2b)
row3b = data.frame(Age = 30, Area = 15, White = "No", Inhale = "Yes", Flame = "Yes", Probability = predict3b)
row4b = data.frame(Age = 30, Area = 15, White = "No", Inhale = "No", Flame = "No", Probability = predict4b)
row5b = data.frame(Age = 70, Area = 15, White = "Yes", Inhale = "Yes", Flame = "Yes", Probability = predict5b)
row6b = data.frame(Age = 70, Area = 15, White = "Yes", Inhale = "No", Flame = "No", Probability = predict6b)
row7b = data.frame(Age = 70, Area = 15, White = "No", Inhale = "Yes", Flame = "Yes", Probability = predict7b)
row8b = data.frame(Age = 70, Area = 15, White = "No", Inhale = "No", Flame = "No", Probability = predict8b)
row9b = data.frame(Age = 30, Area = 50, White = "Yes", Inhale = "Yes", Flame = "Yes", Probability = predict9b)
row10b = data.frame(Age = 30, Area = 50, White = "Yes", Inhale = "No", Flame = "No", Probability = predict10b)
row11b = data.frame(Age = 30, Area = 50, White = "No", Inhale = "Yes", Flame = "Yes", Probability = predict11b)
row12b = data.frame(Age = 30, Area = 50, White = "No", Inhale = "No", Flame = "No", Probability = predict12b)
row13b = data.frame(Age = 70, Area = 50, White = "Yes", Inhale = "Yes", Flame = "Yes", Probability = predict13b)
row14b = data.frame(Age = 70, Area = 50, White = "Yes", Inhale = "No", Flame = "No", Probability = predict14b)
row15b = data.frame(Age = 70, Area = 50, White = "No", 
                    Inhale = "Yes", Flame = "Yes", Probability = predict15b) # prob. of fatality lower for inhale 
row16b = data.frame(Age = 70, Area = 50, White = "No",                      
                    Inhale = "No", Flame = "Yes", Probability = predict16b)  # prob. of fatality lower for inhale 
row17b = data.frame(Age = 70, Area = 50, White = "No",                       
                    Inhale = "Yes", Flame = "No", Probability = predict17b)  # prob. of fatality lower for inhale
row18b = data.frame(Age = 70, Area = 50, White = "No",                      
                    Inhale = "No", Flame = "No", Probability = predict18b)   # prob. of fatality lower for inhale

table2 = rbind(table2, row1b,row2b,row3b,row4b,row5b,row6b,row7b,row8b,row9b,
               row10b,row11b,row12b,row13b,row14b,row15b,row16b, row17b, row18b)
rownames(table2) = NULL

print(table2)
###

# Create graphs
library(ggplot2)

## For age (holding burn area constant)

# Generate a new data frame of all combinations of race/inhal and a range of ages
new_data = expand.grid(
  age = seq(0, 100, length.out = 100),
  race = c(0, 1),
  inhal = c(0, 1)
)

# Hold 'area' constant at median value
new_data$area = median(data1$area, na.rm = TRUE)

# Predict probabilities
new_data$prob = predict(model1.3, newdata = new_data, type = "response")

# Create a grouping label
new_data$group = with(new_data, paste0("Race=", race, ", Inhal=", inhal))

ggplot(new_data, aes(x = age, y = prob, color = group)) +
  geom_line(size = 1.2) +
  labs(
    title = "Predicted Probability of Fatality by Age",
    x = "Age",
    y = "Probability of Fatality",
    color = "Group"
  ) +
  theme_minimal()

## For burn area (holding age constant)

# Generate a new data frame of all combinations of race/inhal and a range of areas
new_data = expand.grid(
  area = seq(0, 100, length.out = 100),
  race = c(0, 1),
  inhal = c(0, 1)
)

# Hold 'age' constant at median value
new_data$age = median(data1$age, na.rm = TRUE)

# Predict probabilities
new_data$prob = predict(model1.3, newdata = new_data, type = "response")

# Create a grouping label
new_data$group = with(new_data, paste0("Race=", race, ", Inhal=", inhal))

ggplot(new_data, aes(x = area, y = prob, color = group)) +
  geom_line(size = 1.2) +
  labs(
    title = "Predicted Probability of Fatality by Burn Area",
    x = "Burn Area",
    y = "Probability of Fatality",
    color = "Group"
  ) +
  theme_minimal()

# training vs test
library(knitr)

set.seed(123)  # Set seed for reproducibility
sample_index = sample(1:nrow(data1), size = 0.8 * nrow(data1))  

# Create the training and test sets
train_data = data1[sample_index, ]
test_data = data1[-sample_index, ]

# create model 1.11
model1.11 = glm(fatality ~ ., family = "binomial", train_data)
summary(model1.11)

# stepwise method to select model 1.21 
model1.21 = step(model1.11, trace = 0)
summary(model1.21)

# model 1.31
model1.31 = glm(fatality ~ age + area + race + inhal, family = "binomial", train_data)
summary(model1.31)

# model 1.41
model1.41 = glm(fatality ~ age + area + inhal, family = "binomial", train_data)
summary(model1.41)

# model 1.51
model1.51 = glm(fatality ~ age + area + race + inhal + age:inhal , 
                family = "binomial", train_data)
summary(model1.51)

# model 1.61
model1.61 = glm(fatality ~ age + area + race + inhal + age:inhal + area:inhal, 
                family = "binomial", train_data)
summary(model1.61)

# model 1.71
model1.71 = glm(fatality ~ age + area + race + inhal + flame + age:inhal 
                + area:inhal + area:flame, family = "binomial", train_data)
summary(model1.71)

#classification table
library(pscl)
hitmiss(model1.11)
hitmiss(model1.21)
hitmiss(model1.31)
hitmiss(model1.41)
hitmiss(model1.51)
hitmiss(model1.61)
hitmiss(model1.71)

# predict probabilities of fatality
pred_probs1 = predict(model1.11, newdata = test_data, type = "response")
pred_probs2 = predict(model1.21, newdata = test_data, type = "response")
pred_probs3 = predict(model1.31, newdata = test_data, type = "response")
pred_probs4 = predict(model1.41, newdata = test_data, type = "response")
pred_probs5 = predict(model1.51, newdata = test_data, type = "response")
pred_probs6 = predict(model1.61, newdata = test_data, type = "response")
pred_probs7 = predict(model1.71, newdata = test_data, type = "response")

# assign to classes
pred_class1 = ifelse(pred_probs1 > 0.5, 1, 0)
pred_class2 = ifelse(pred_probs2 > 0.5, 1, 0)
pred_class3 = ifelse(pred_probs3 > 0.5, 1, 0)
pred_class4 = ifelse(pred_probs4 > 0.5, 1, 0)
pred_class5 = ifelse(pred_probs5 > 0.5, 1, 0)
pred_class6 = ifelse(pred_probs6 > 0.5, 1, 0)
pred_class7 = ifelse(pred_probs7 > 0.5, 1, 0)

# compare with test data
actual = test_data$fatality
table(Predicted = pred_class1, Actual = actual)
table(Predicted = pred_class2, Actual = actual)
table(Predicted = pred_class3, Actual = actual) # final model
table(Predicted = pred_class4, Actual = actual)
table(Predicted = pred_class5, Actual = actual)
table(Predicted = pred_class6, Actual = actual)
table(Predicted = pred_class7, Actual = actual)

# calculate accuracy
accuracy1 = mean(pred_class1 == actual)
print(paste("Accuracy:", round(accuracy1, 3)))
accuracy2 = mean(pred_class2 == actual)
print(paste("Accuracy:", round(accuracy2, 3)))
accuracy3 = mean(pred_class3 == actual)  # final model
print(paste("Accuracy:", round(accuracy3, 3)))
accuracy4 = mean(pred_class4 == actual)
print(paste("Accuracy:", round(accuracy4, 3)))
accuracy5 = mean(pred_class5 == actual)
print(paste("Accuracy:", round(accuracy5, 3)))
accuracy6 = mean(pred_class6 == actual)
print(paste("Accuracy:", round(accuracy6, 3)))
accuracy7 = mean(pred_class7 == actual)
print(paste("Accuracy:", round(accuracy7, 3)))

###################################################################################################
# Question 2

# import data
library(openxlsx)
data2 = read.xlsx("schools25.xlsx")

# check for missing values
!complete.cases(data2)

library(dplyr)
data2.1 = data2 %>% dplyr::select(score1, everything())
data2.2 = data2 %>% dplyr::select(score2, everything())
data2.1 = data2.1[,-c(2,10)] # remove score 2
data2.2 = data2.2[,-c(2,10)] # remove score 1

# create scatterplot matrices
pairs(data2.1, pch = 16, cex = 0.7, col = "darkred", 
      main = "Scatterplot Matrix - Score 1")
pairs(data2.2, pch = 16, cex = 0.7, col = "darkred", 
      main = "Scatterplot Matrix - Score 2")

# correlation
cor(data2.1)
cor(data2.2)

# create score 1 models
model2.11 = lm(score1~., data2.1) 
summary(model2.11)

model2.12 = lm(score1 ~ special + free + stratio + income + noteng, data2.1)
summary(model2.12) # AIC preferred model

model2.13 = lm(score1 ~ special + free + stratio + income, data2.1)
summary(model2.13) # score1 final model

model2.14 = lm(score1~ free + stratio + income, data2.1)
summary(model2.14)

# create score 2 models
model2.15 = lm(score2~., data2.2)
summary(model2.15)

model2.16 = lm(score2~ special + spendspec + free + stratio + income, data2.2)
summary(model2.16) # AIC preferred model

model2.17 = lm(score2~ spendspec + free + stratio + income, data2.2)
summary(model2.17)

confint(model2.17)

model2.18 = lm(score2~ free + stratio + income, data2.2)
summary(model2.18)

model2.19 = lm(score2~ special + free + stratio + income, data2.2)
summary(model2.19) # score2 final model

confint(model2.19)

# ANOVA
anova(model2.12, model2.13)
anova(model2.13, model2.14)

anova(model2.16, model2.17)
anova(model2.16, model2.19)
anova(model2.17, model2.18)
anova(model2.18, model2.19)

# VIF
library(car)
vif(model2.11)
vif(model2.13)

vif(model2.15)
vif(model2.19)

# standardised residuals
plot(rstandard(model2.11),  pch = 16, cex = 0.7, col = "darkred", 
     main = "Standardised Residuals", ylim = c(-3.5, 3.5))
abline(h = c(-3, 0,3), col = "blue", lty = c(2,1))
plot(rstandard(model2.13),pch = 16, cex = 0.7, col = "darkred", 
     main = "Standardised Residuals - Score 1", ylim = c(-3.5, 3.5))
abline(h = c(-3,0,3), col = "blue", lty = c(2,1))

plot(rstandard(model2.15),pch = 16, cex = 0.7, col = "darkred", 
     main = "Standardised Residuals", ylim = c(-5, 5))
abline(h = c(-3,0,3), col = "blue", lty = c(2,1))
plot(rstandard(model2.19),pch = 16, cex = 0.7, col = "darkred", 
     main = "Standardised Residuals - Score 2", ylim = c(-5, 5))
abline(h = c(-3,0,3), col = "blue", lty = c(2,1))

# residual plot
residualPlots(model2.11, pch = 16, cex = 0.7, col = "darkred", 
              main = "Residual Plots")
residualPlots(model2.13, pch = 16, cex = 0.7, col = "darkred", 
              main = "Residual Plots - Score 1")

residualPlots(model2.15, pch = 16, cex = 0.7, col = "darkred", 
              main = "Residual Plots")
residualPlots(model2.17, pch = 16, cex = 0.7, col = "darkred", 
              main = "Residual Plots")
residualPlots(model2.19, pch = 16, cex = 0.7, col = "darkred", 
              main = "Residual Plots - Score 2")

# C+R plot
crPlots(model2.11, pch = 16, cex = 0.7, col = "darkred", main = "CR Plots")
crPlots(model2.13, pch = 16, cex = 0.7, col = "darkred", main = "CR Plots - Score 1")

crPlots(model2.15, pch = 16, cex = 0.7, col = "darkred", main = "CR Plots")
crPlots(model2.17, pch = 16, cex = 0.7, col = "darkred", main = "CR Plots")
crPlots(model2.19, pch = 16, cex = 0.7, col = "darkred", main = "CR Plots - Score 2") 

# identify outliers 
std_res1 = rstandard(model2.11)
which(abs(std_res1) > 3)
std_res2 = rstandard(model2.13)
which(abs(std_res2) > 3)

std_res3 = rstandard(model2.15)
which(abs(std_res3) > 3)
std_res4 = rstandard(model2.19)
which(abs(std_res4) > 3)

# check Cook's distance 
plot(cooks.distance(model2.11), pch = 16, col = "darkred", cex = 0.7, 
     ylim = c(0, 1.2), main = "Cook's distance")
abline(h = 1, col = "blue", lty = 2)
plot(cooks.distance(model2.13), pch = 16, col = "darkred", cex = 0.7, 
     ylim = c(0, 1.2), main = "Cook's distance - Score 1")
abline(h = 1, col = "blue", lty = 2)

plot(cooks.distance(model2.15), pch = 16, col = "darkred", cex = 0.7, 
     ylim = c(0, 1.2), main = "Cook's distance")
abline(h = 1, col = "blue", lty = 2)
plot(cooks.distance(model2.19), pch = 16, col = "darkred", cex = 0.7, 
     ylim = c(0, 1.2), main = "Cook's distance - Score 2")
abline(h = 1, col = "blue", lty = 2)

# test for normality of residuals
shapiro.test(model2.11$residuals)
shapiro.test(model2.13$residuals)

qqnorm(model2.13$residuals, pch = 16, cex = 0.7, col = "darkred", 
       main = "Normal Q-Q Plot - Score 1")       # Q-Q plot of residuals
qqline(model2.13$residuals, col = "blue", lwd = 2) 

shapiro.test(model2.15$residuals)
shapiro.test(model2.19$residuals) 

qqnorm(model2.19$residuals, pch = 16, cex = 0.7, col = "darkred",
       main = "Normal Q-Q Plot - Score 2")       # Q-Q plot of residuals
qqline(model2.19$residuals, col = "blue", lwd = 2) 

# test for homoscedasticity 
library(lmtest)
bptest(model2.11)
bptest(model2.13)

bptest(model2.15)
bptest(model2.19)

# test for independence of residuals 
set.seed(111)
car::durbinWatsonTest(model2.11)
car::durbinWatsonTest(model2.13)

car::durbinWatsonTest(model2.15)
car::durbinWatsonTest(model2.19)

# step AIC
null1 = lm(score1~ 1, data2.1)
null2 = lm(score2~ 1, data2.2)

library(MASS) 
  # Score 1
stepAIC(null1, scope = list(upper = model2.11, lower = null1, raw = TRUE), 
        data = data2.1, direction = "forward")

stepAIC(model2.11, scope = list(upper = model2.11, lower = null1, raw = TRUE), 
        data = data2.1, direction = "backward")

stepAIC(model2.11, scope = list(upper = model2.11, lower = null1, raw = TRUE), 
        data = data2.1, direction = "both")

# score1 ~ special + free + stratio + income + noteng

  # Score 2
stepAIC(null2, scope = list(upper = model2.15, lower = null2, raw = TRUE), 
        data = data2.2, direction = "forward")

stepAIC(model2.15, scope = list(upper = model2.15, lower = null2, raw = TRUE), 
        data = data2.2, direction = "backward")

stepAIC(model2.15, scope = list(upper = model2.15, lower = null2, raw = TRUE), 
        data = data2.2, direction = "both")

# score2 ~ special + free + stratio + income + spendspec

# prediction model2.12 
prediction2 = data.frame(special =c(26, 16, 11), free = c(76, 11, 1), 
                            stratio = c(27, 17, 12), income = c(10, 17, 46)) 
result2 = predict(model2.13, prediction2)
print(result2)

# prediction model2.17
prediction3 = data.frame(special =c(26, 16, 11), free = c(76, 11, 1), 
                         stratio = c(27, 17, 12), income = c(10, 17, 46)) 
result3 = predict(model2.19, prediction3)
print(result3)

# investigate outliers for score 2 model
data2.2[c(35,121,146,162),]
summary(data2.2)

####### Score 2 model without outliers/with transformations
data2.3 = data2.2[-c(35,121,146,162),]

model2.25 = lm(score2~., data2.3)
summary(model2.25)

model2.29 = lm(score2~ special + free + stratio + income, data2.3)
summary(model2.29)  # AIC preferred model for dataset without outliers

confint(model2.29)

model2.31 = lm(score2~ spendspec + I(spendspec^2) + free + I(free^2) + stratio + income, 
               data2.2)
summary(model2.31)  # transformations on model built using full dataset

# VIF
library(car)
vif(model2.29)

# standardised residuals
plot(rstandard(model2.29),pch = 16, cex = 0.7, col = "darkred", 
     main = "Standardised Residuals - Score 2", ylim = c(-5, 5))
abline(h = c(-3,0,3), col = "blue", lty = c(2,1))

plot(rstandard(model2.31),pch = 16, cex = 0.7, col = "darkred", 
     main = "Standardised Residuals - Score 2", ylim = c(-5, 5))
abline(h = c(-3,0,3), col = "blue", lty = c(2,1))

# residual plot
residualPlots(model2.29, pch = 16, cex = 0.7, col = "darkred", 
              main = "Residual Plots - Score 2")

residualPlots(model2.31, pch = 16, cex = 0.7, col = "darkred", 
              main = "Residual Plots - Score 2")

# C+R plot
crPlots(model2.29, pch = 16, cex = 0.7, col = "darkred", 
        main = "CR Plots - Score 2") 

crPlots(model2.31, pch = 16, cex = 0.7, col = "darkred", 
        main = "CR Plots - Score 2")

# identify outliers 
std_res5 = rstandard(model2.29)
which(abs(std_res5) > 3)

std_res6 = rstandard(model2.31)
which(abs(std_res6) > 3)

# check Cook's distance 
plot(cooks.distance(model2.29), pch = 16, col = "darkred", cex = 0.7, 
     ylim = c(0, 1.2), main = "Cook's distance - Score 2")
abline(h = 1, col = "blue", lty = 2)

plot(cooks.distance(model2.31), pch = 16, col = "darkred", cex = 0.7, 
     ylim = c(0, 1.2), main = "Cook's distance - Score 2")
abline(h = 1, col = "blue", lty = 2)

# test for normality of residuals
shapiro.test(model2.29$residuals) 

qqnorm(model2.29$residuals, pch = 16, cex = 0.7, col = "darkred",
       main = "Normal Q-Q Plot - Score 2")       # Q-Q plot of the residuals
qqline(model2.29$residuals, col = "blue", lwd = 2) 

shapiro.test(model2.31$residuals) 

qqnorm(model2.31$residuals, pch = 16, cex = 0.7, col = "darkred",
       main = "Normal Q-Q Plot - Score 2")       # Q-Q plot of the residuals
qqline(model2.31$residuals, col = "blue", lwd = 2)

# test for homoscedasticity 
library(lmtest)
bptest(model2.29)

bptest(model2.31)

# test for independence of residuals 
set.seed(111)
car::durbinWatsonTest(model2.29)

car::durbinWatsonTest(model2.31)

# step AIC
null3 = lm(score2~ 1, data2.3)

library(MASS) 
# Score 2
stepAIC(null3, scope = list(upper = model2.25, lower = null3, raw = TRUE), 
        data = data2.3, direction = "forward")

stepAIC(model2.25, scope = list(upper = model2.25, lower = null3, raw = TRUE), 
        data = data2.3, direction = "backward")

stepAIC(model2.25, scope = list(upper = model2.25, lower = null3, raw = TRUE), 
        data = data2.3, direction = "both")
