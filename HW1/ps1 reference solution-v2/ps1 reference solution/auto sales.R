### 3. Forecasting Auto Sales
#a
Auto = read.csv(file.choose())
Auto_train = subset(Auto, Year<2013)
Auto_test = subset(Auto, Year>=2013)
AutoLM = lm(AutoSales ~ Unemployment + Queries + CPI_energy + CPI_all, data=Auto_train)
summary(AutoLM)
# out-of-sample 1 R^2
PredictTest1 = predict(AutoLM, newdata=Auto_test)
SSE1 = sum((PredictTest1 - Auto_test$AutoSales)^2)
SST1 = sum((Auto_test$AutoSales - mean(Auto_train$AutoSales))^2)
R_Squared1 = 1-SSE1/SST1 #0.4975116

#b
AutoLM_new = lm(AutoSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month, data=Auto_train)
summary(AutoLM_new)
# out-of-sample 2 R^2
PredictTest2 = predict(AutoLM_new, newdata=Auto_test)
SSE2 = sum((PredictTest2 - Auto_test$AutoSales)^2)
SST2 = sum((Auto_test$AutoSales - mean(Auto_train$AutoSales))^2)
R_Squared2 = 1-SSE2/SST2 #0.4662344

#c
#110.69 * (3 - 1) = 110.69 * 2 = 221.38
#110.69 * (5 - 1) = 110.69 * 4 = 442.76


#d
Auto_train$MonthF=as.factor(Auto_train$Month)
Auto_test$MonthF=as.factor(Auto_test$Month)


#e
AutoLM_newF = lm(AutoSales ~ Unemployment + Queries + CPI_energy + CPI_all + MonthF, data=Auto_train)
summary(AutoLM_newF)

PredictTest3 = predict(AutoLM_newF, newdata=Auto_test)
SSE3 = sum((PredictTest3 - Auto_test$AutoSales)^2)
SST3 = sum((Auto_test$AutoSales - mean(Auto_train$AutoSales))^2)
R_Squared3 = 1-SSE3/SST3 #0.7426902

#f.1

cor(Auto_train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])
AutoLM_newF_reduced = lm(AutoSales ~ Unemployment +  CPI_energy + CPI_all + MonthF, data=Auto_train)
summary(AutoLM_newF_reduced)

 
#g.1
# out-of-sample 4 R^2
PredictTest4 = predict(AutoLM_newF_reduced, newdata=Auto_test)
SSE4 = sum((PredictTest4 - Auto_test$AutoSales)^2)
SST4 = sum((Auto_test$AutoSales - mean(Auto_train$AutoSales))^2)
R_Squared4 = 1-SSE4/SST4 # = 0.7280232

#f.2
AutoLM_newF_reduced2 = lm(AutoSales ~ Unemployment + MonthF, data=Auto_train)
summary(AutoLM_newF_reduced2)

#g.2
PredictTest5 = predict(AutoLM_newF_reduced2, newdata=Auto_test)
SSE5 = sum((PredictTest5 - Auto_test$AutoSales)^2)
SST5 = sum((Auto_test$AutoSales - mean(Auto_train$AutoSales))^2)
R_Squared5 = 1-SSE5/SST5 # = 0.8793167

