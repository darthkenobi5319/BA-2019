# set workspace
setwd("E:\\School\\TA\\NYU_shanghai\\Lecture2\\__MACOSX")
getwd()

# load data
kidIQ=read.csv("kidiq.csv")
str(kidIQ)
summary(kidIQ)

#Scatter plot between kidIQ and mom_hs
plot(kidIQ$kid_score, kidIQ$mom_hs, xlab="mom_hs", ylab = "kidIQ", col = "red",cex.axis=1.5,cex.lab=1.5,pch=16)

#Scatter plot between kidIQ and mom_iq
plot(kidIQ$kid_score, kidIQ$mom_iq, xlab="mom_iq", ylab = "kidIQ", col = "red",cex.axis=1.5,cex.lab=1.5,pch=16)

#Scatter plot between kidIQ and mom_work
plot(kidIQ$kid_score, kidIQ$mom_work, xlab="mom_work", ylab = "kidIQ", col = "red",cex.axis=1.5,cex.lab=1.5,pch=16)

#Scatter plot between kidIQ and mom_age
plot(kidIQ$kid_score, kidIQ$mom_age, xlab="mom_age", ylab = "kidIQ", col = "red",cex.axis=1.5,cex.lab=1.5,pch=16)


# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(kidIQ$kid_score, SplitRatio = 0.7)
Train = subset(kidIQ, spl==TRUE)
Test = subset(kidIQ, spl==FALSE)



# install package
install.packages('arm')
library('arm')


# Linear Regression 1
model1 = lm(kid_score ~ mom_hs+mom_iq+mom_work+mom_age , data=Train)
display(model1)
summary(model1)

# delete two insignificant variables: mom_work,mom_age
model2=lm(kid_score ~ mom_hs+mom_iq,data=Train)
display(model2)
summary(model2)

model3=lm(kid_score ~ mom_hs+mom_iq+mom_hs*mom_iq,data=Train)
display(model3)
summary(model3)

model34=lm(kid_score ~ mom_hs+mom_iq+mom_age+as.factor(mom_work),data=Train)
display(model34)
summary(model34)

# model4=lm(kid_score ~ as.factor(mom_work),data=kidIQ)
# display(model4)
# summary(model4)
# 
# model5=lm(kid_score ~ mom_hs+mom_iq+mom_hs*mom_iq+as.factor(mom_work),data=kidIQ)
# display(model5)
# summary(model5)