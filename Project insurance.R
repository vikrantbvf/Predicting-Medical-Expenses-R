#Data collection
insurance = read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)

#Data Exploration and preperation
summary(insurance$expenses)
hist(insurance$expenses)
table(insurance$region)

#Generating correlation matrix
cor(insurance[c("age", "bmi", "children", "expenses")])

#Generating scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "expenses")])

library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

#Model Training
m = lm(expenses ~ ., data = insurance)
m

#Model evaluation
summary(m)

# Model correction
insurance1 = insurance
library(MASS)
bc = boxcox(lm(expenses ~ ., data = insurance1))
lambda = bc$x[which.max(bc$y)]
insurance1$expenses2 = (insurance1$expenses^lambda -1)/lambda
hist(insurance1$expenses2)
m1 = lm(expenses2 ~ ., data = insurance1)
summary(m1)


# Model correction boc cox
insurance1 = insurance
library(MASS)
bc = boxcox(lm(expenses ~ ., data = insurance1))
lambda = bc$x[which.max(bc$y)]
insurance1$expenses2 = (insurance1$expenses^lambda -1)/lambda
hist(insurance1$expenses2)
m1 = lm(expenses2 ~ .-expenses, data = insurance1)
summary(m1)

#Predicting the old value from new prdicted value
trainCases1 = createDataPartition(insurance1$expenses2, p = 0.85, list = F)
train1 = insurance1[trainCases1,]
test1 = insurance1[-trainCases1,]
m2 = lm(log(expenses2)~.-expenses, train1)
p1 = exp(predict(m2, test1))
RSS1 = sum((p1 - test1$expenses2)^2)


# Model with gamma regression
gm1 = glm(expenses ~ ., data = insurance, family = Gamma(link = "log"))
summary(gm1)
gm2 = glm(expenses ~ ., data = insurance, family = gaussian(link = "identity"))
summary(gm2)






