setwd("C:/Users/³¯¤¸­P/Desktop/UIUC_firstsemester/is507/groupproject")


library(readr)
airbnb <- read_csv("abnb_li_process.csv", col_name = TRUE)
View(airbnb)
str(airbnb)



'Q1'   #does it has relation between room_type and price?

table(airbnb$room_type)
table(airbnb$price)

#Covnvert variable into categorical variable
airbnb$room_type <- as.factor(airbnb$room_type)
str(airbnb$room_type)

#Check for variable normality
#Using byfshapiro for G3(continuous variable) divided by studytime(categorical)
library(RVAideMemoire)
byf.shapiro(as.matrix(airbnb$price)~airbnb$room_type,data=airbnb)

#Check for variable normality
library(RVAideMemoire)
byf.shapiro(as.matrix(airbnb$price)~airbnb$room_type,data=airbnb)

#Check for equal variances
library(DescTools)
LeveneTest(airbnb$price ~airbnb$room_type)

# Not normal data, equal variance
# Kruskal-Wallis test
kruskal.test(price~room_type, data = airbnb)

#-------------------------

#Q2  manual regression: the relation between accommodates,bathrooms,bedrooms,beds and Q1, Q2, Q3 AND Q4

#first season
reg_dataset <- airbnb[, c(12:16)]
View(reg_dataset)
str(reg_dataset)


model1 <- lm(airbnb$Q1 ~ ., data = reg_dataset)
model1

library(DescTools)
VIF(model1)

library(corrplot)
corrplot(cor(reg_dataset, method ="spearman") )
corrplot(cor(reg_dataset, method = "spearman"), method = "number")

summary(model1)

plot(model1)
library(ggfortify)
autoplot(model1)




#stepwise
null = lm(airbnb$Q1 ~ 1-airbnb$Q1, data = reg_dataset)
null

full = lm(airbnb$Q1 ~ .-airbnb$Q1, data=reg_dataset)
full

train_Step = step(null, scope = list(upper=full), direction = "both")
summary(train_Step)

#second season

model2 <- lm(airbnb$Q2 ~ ., data = reg_dataset)
model2

VIF(model2)

summary(model2)

plot(model2)
autoplot(model2)

corrplot(cor(reg_dataset, method = "spearman"))
corrplot(cor(reg_dataset, method = "spearman"), method="number")

#stepwise
null = lm(airbnb$Q2 ~ 1-airbnb$Q2, data = reg_dataset)
null

full = lm(airbnb$Q2 ~ .-airbnb$Q2, data=reg_dataset)
full

train_Step = step(null, scope = list(upper=full), direction = "both")
summary(train_Step)

#third season

model3 <- lm(airbnb$Q3 ~ ., data = reg_dataset)
model3


VIF(model3)

summary(model3)

plot(model3)
library(ggfortify)
autoplot(model3)


library(corrplot)
corrplot(cor(reg_dataset, method = "spearman"))
corrplot(cor(reg_dataset, method = "spearman"), method="number")

#stepwise
null = lm(airbnb$Q3 ~ 1-airbnb$Q3, data = reg_dataset)
null

full = lm(airbnb$Q3 ~ .-airbnb$Q3, data=reg_dataset)
full

train_Step = step(null, scope = list(upper=full), direction = "both")
summary(train_Step)

#fourth season

model4 <- lm(airbnb$Q4 ~ ., data = reg_dataset)
model4

VIF(model4)

summary(model4)

plot(model4)
autoplot(model4)

corrplot(cor(reg_dataset, method = "spearman"))
corrplot(cor(reg_dataset, method = "spearman"), method="number")

#stepwise
null = lm(airbnb$Q1 ~ 1-airbnb$Q4, data = reg_dataset)
null

full = lm(airbnb$Q1 ~ .-airbnb$Q4, data=reg_dataset)
full

train_Step = step(null, scope = list(upper=full), direction = "both")
summary(train_Step)