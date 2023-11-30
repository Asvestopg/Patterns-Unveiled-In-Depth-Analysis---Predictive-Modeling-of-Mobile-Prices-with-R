library(summarytools)
library(psych)
library(ggplot2)
library(olsrr)
setwd("D:\\ΙΠΤΗΛ\\DWS\\Statistics\\Εργασία")
getwd()

# Read to data frame
cell.phones <- read.csv("Cell_Phones_labels.csv", header = T, sep = ';',
                        stringsAsFactors = T, dec = ",", na.strings = " ")

head(cell.phones)
str(cell.phones)

summary(cell.phones$mobileprice)
hist(cell.phones$mobileprice)
# Check if mobile price is normally distributed
hist(cell.phones$mobileprice, freq = F, main = "HIstogram of Mobile Prices", 
     xlab = "Mobile Price")

# Draw normal dist over histogram
lines(seq(0,350,.1), dnorm(seq(0,350,.1), mean(cell.phones$mobileprice), 
                           sd(cell.phones$mobileprice)), col = "navy", lwd = 2)


# QQ-plot 
qqnorm(cell.phones$mobileprice)
qqline(cell.phones$mobileprice, lwd = 2, col = "blue")

# Shapiro-Wilk test
shapiro.test(cell.phones$mobileprice)



names(cell.phones)[1] <-"psraid"

# Convert psraid to factor
cell.phones$psraid <- factor(cell.phones$psraid)

# Summary statistics
summary(cell.phones)

# Plot mobileprice and usr_r

plot(cell.phones$usr_r,ylim=range(0:1200))
summary(cell.phones$usr_r)

boxplot(cell.phones$mobileprice ~ cell.phones$usr_r, lty = 1, lwd = .8, cex = 1.5,
        xlab = "Residence", ylab = "Mobile Price", border = "black")

# Plot mobileprice and sex

summary(cell.phones$sex)
plot(cell.phones$sex)



boxplot(cell.phones$mobileprice ~ cell.phones$sex, lty = 1, lwd = .8, cex = 1.5,
        xlab = "Sex", ylab = "Mobile Price", border = "black")

# Plot mobileprice and empl
boxplot(cell.phones$mobileprice ~ cell.phones$empl, lty = 1, lwd = .8, cex = 1.5,
        xlab = "Employment status", ylab = "Mobile Price", border = "black")

# Plot mobileprice and education

summary(cell.phones$educ)
pie(table(cell.phones$educ))
ANOVA1_EDUC = aov(cell.phones$mobileprice ~ cell.phones$educ)

levels(cell.phones$educ)
levels(cell.phones$educ)[c(3,4)]= "LOW LEVEL"
levels(cell.phones$educ)
levels(cell.phones$educ)[c(2,6)]= "MID LEVEL"
levels(cell.phones$educ)
levels(cell.phones$educ)[c(1,5)]= "MID-HIGH LEVEL"
levels(cell.phones$educ)
levels(cell.phones$educ)[c(4)]= "HIGH LEVEL"
summary(cell.phones$educ)
plot(cell.phones$educ)

ANOVA2_EDUC = aov(cell.phones$mobileprice ~ cell.phones$educ)
anova(ANOVA1_EDUC,ANOVA2_EDUC)

boxplot(cell.phones$mobileprice ~ cell.phones$educ, lty = 1, lwd = .8, cex = 1.5,
        xlab = "Education Level", ylab = "Mobile Price", border = "black", notch = T)

summary(aov(cell.phones$mobileprice ~ cell.phones$educ))

# Plot mobileprice and income

summary(cell.phones$inc)
levels(cell.phones$inc) <-  c("10-20k", "100-150k", ">150k", "20-30k", "30-40k",
                              "40-50k", "50-75k", "75-100k", "<10k")
plot(cell.phones$inc)




boxplot(cell.phones$mobileprice ~ cell.phones$inc, lty = 1, lwd = .8, cex = 1.5,
        xlab = "Income (USD)", ylab = "Mobile Price", border = "black")

summary(aov(cell.phones$mobileprice ~ cell.phones$inc))

# Plot mobileprice and mar

summary(cell.phones$mar)
plot(cell.phones$mar)

summary(cell.phones[cell.phones$mar=="Widowed",])

boxplot(cell.phones$mobileprice ~ cell.phones$mar, lty = 1, lwd = .8, cex = 1.5,
        xlab = "Marital status", ylab = "Mobile Price", border = "black")

summary(aov(cell.phones$mobileprice ~ cell.phones$mar))

#Exetasi sisxetisewn gia meiwsi tou arithmou twn epipedwn
AOV1=aov(cell.phones$mobileprice ~ cell.phones$mar)
library(DescTools)
PostHocTest(aov(cell.phones$mobileprice ~ cell.phones$mar),method="hsd")
PostHocTest(aov(cell.phones$mobileprice ~ cell.phones$mar),method="duncan")

levels(cell.phones$mar)
levels(cell.phones$mar)[c(1,5,6)] = "Single"
levels(cell.phones$mar)
levels(cell.phones$mar)[c(2,4)]= "LivingWithPartner"
levels(cell.phones$mar)
AOV2=aov(cell.phones$mobileprice ~ cell.phones$mar)
anova(AOV1,AOV2)

# Pearson correlation of age, mobileprice
cor(cell.phones$age, cell.phones$mobileprice, use = "c")
cor.test(cell.phones$age, cell.phones$mobileprice, method = "pearson")
cor.test(cell.phones$age, cell.phones$mobileprice, method = "spearman")
cor.test(cell.phones$age, cell.phones$mobileprice, method = "kendall")

# plot age with mobileprice 
plot(cell.phones$age, cell.phones$mobileprice, pch = 4, col = "blue", main = "Age ~ Mobile Price")
abline(b = cor(cell.phones$age, cell.phones$mobileprice, use = "c"), a = 250, lwd = 3, col = "darkorange")

# plot some other features

levels(cell.phones$q17e) <-  c("NA feature", "No", "Yes")
levels(cell.phones$q22a) <-  c("Agree", "Disagree", "Neither Agree/Disagree")

par(mfrow=c(2,2))
boxplot(cell.phones$mobileprice ~ cell.phones$q14a, lty = 1, lwd = .8, cex = 1.5,
        xlab = "Emails", ylab = "Mobile Price", border = "black", notch = F)
boxplot(cell.phones$mobileprice ~ cell.phones$q14g, lty = 1, lwd = .8, cex = 1.5,
        xlab = "Games", ylab = "Mobile Price", border = "black", notch = F)
boxplot(cell.phones$mobileprice ~ cell.phones$q17e, lty = 1, lwd = .8, cex = 1.5,
        xlab = "Social Media", ylab = "Mobile Price", border = "black", notch = F)
boxplot(cell.phones$mobileprice ~ cell.phones$q22a, lty = 1, lwd = .8, cex = 1.5,
        xlab = "Feel Safer", ylab = "Mobile Price", border = "black", notch = F)


levels(cell.phones$q17g) <- c("NA feature", "No", "Yes")


par(mfrow=c(2,2))

levels(cell.phones$q24) <- c("No", "NA feature", "Yes")

boxplot(cell.phones$mobileprice ~ cell.phones$q14b, lty = 1, lwd = .8, cex = 1.5,
        xlab = "SMS", ylab = "Mobile Price", border = "black", notch = F)
boxplot(cell.phones$mobileprice ~ cell.phones$q17g, lty = 1, lwd = .8, cex = 1.5,
        xlab = "Videos", ylab = "Mobile Price", border = "black", notch = F)
boxplot(cell.phones$mobileprice ~ cell.phones$q24, lty = 1, lwd = .8, cex = 1.5,
        xlab = "Apps", ylab = "Mobile Price", border = "black", notch = F)
boxplot(cell.phones$mobileprice ~ cell.phones$q14h, lty = 1, lwd = .8, cex = 1.5,
        xlab = "Browsing", ylab = "Mobile Price", border = "black", notch = F)

summary(aov(cell.phones$mobileprice[cell.phones$q17g != "NA feature"] ~ cell.phones$q17g[cell.phones$q17g != "NA feature"]))
summary(aov(cell.phones$mobileprice ~ cell.phones$q24))
summary(aov(cell.phones$mobileprice ~ cell.phones$q14b))

#Check if any variables dont have a strong corelation with the price of the Mobilephone
summary(aov(cell.phones$mobileprice ~ cell.phones$q17d))
summary(aov(cell.phones$mobileprice ~ cell.phones$q22e))

#model 1 

# Ancova model structure using two variables , Age (numerical) & sex(categorical)
model=lm(cell.phones$mobileprice ~ cell.phones$age * cell.phones$sex )
summary.aov(model)
#Simplification of our model 
model2=lm(cell.phones$mobileprice ~ cell.phones$age + cell.phones$sex )
anova(model,model2)
#reviewing our simpler model 
summary.lm(model2)

#visualization of our model 
mobileprices_sex = split(cell.phones$mobileprice,cell.phones$sex)
age_sex = split(cell.phones$age,cell.phones$sex)
plot(cell.phones$age,cell.phones$mobileprice,type="n",ylab="Mobile Price",xlab="age")
points(age_sex[[1]],mobileprices_sex[[1]],pch=16)
points(age_sex[[1]],mobileprices_sex[[1]],pch=5,col="blue")
abline(243.20,-0.874,col="orange",lwd=5)
abline(243.20 + 7.32,-0.874 , col="green",lwd=5)

#end of model 1

#model 2 

#Trying to enhance the accuraracy of our model by implementing more variables

summary(cell.phones)
model2 = lm(log(cell.phones$mobileprice)~ cell.phones$age)
plot(model2)

summary.lm(model2)

plot(cell.phones$q17b,cell.phones$mobileprice)
plot(cell.phones$q17b)

#following the example
freq(cell.phones$educ)
freq(cell.phones$q17b)
freq(cell.phones$q14a)
freq(cell.phones$usr_r)

describe(cell.phones$mobileprice)
describe(cell.phones$age)


b =numeric(10000)
for (i in 1:10000) {b[i] = 
  mean(sample(cell.phones$mobileprice, length(cell.phones$mobileprice), replace = T))}
hist(b,main = "Confidence Interval of mobile price", xlab = "Confidence Interval")
quantile(b,c(0.025,0.975))
abline(v=quantile(b,0.025),col="red")
abline(v=quantile(b,0.975),col="purple")

#Creating another model and using Stepwise



model3 = lm(cell.phones$mobileprice~cell.phones$sex+cell.phones$q17b+
              cell.phones$q17g+cell.phones$educ+
              cell.phones$sex*cell.phones$age+cell.phones$q17e)
summary.lm(model3)
model.dim = ols_step_both_p(model3)
model.dim
summary(model.dim$model)
plot(model.dim$model)




