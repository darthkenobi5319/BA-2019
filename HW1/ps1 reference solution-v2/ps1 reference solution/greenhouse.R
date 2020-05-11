# Read and split the data
greenhouse = read.csv("greenhouse.csv")
str(greenhouse)
train = subset(greenhouse, Year <= 2005)
test = subset(greenhouse, Year > 2005)

# build model
ghlm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = train)
summary(ghlm)

# find correlation
cor(train)

# simplifiying the model
ghlm1 = lm(Temp ~ MEI + CO2 + CH4 + CFC.11 + TSI + Aerosols, data = train)
summary(ghlm1)

# test using the full model
PredictTest = predict(ghlm, newdata = test)
SSE = sum((test$Temp - PredictTest)^2)
SST = sum((test$Temp - mean(train$Temp))^2)
R2_1 = 1-SSE/SST
R2_1

# test using reduced model
PredictTest1 = predict(ghlm1, newdata = test)
SSE1 = sum((test$Temp - PredictTest1)^2)
R2_2 = 1-SSE1/SST
R2_2
