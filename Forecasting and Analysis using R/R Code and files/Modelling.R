#Modelling

#Aggregating the data based on Absenteeism time in hour per month for an year
aggre.absent.hours.months =  ddply(complete_data, c("Year", "Month.of.absence"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))


"
Adding the time stamp to the aggre.absent.hours.months, as we need to forecast monthly absenteeism and we have clubbed the data 
based on months, adding frequency = 12, start = c(2007,7) as we have data from July 2007.

"
ts_complete_data = ts(aggre.absent.hours.months$Absenteeism.time.in.hours, frequency = 12,start = c(2007, 7))
autoplot(ts_complete_data)
write.csv(aggre.absent.hours.months, "Absentese_per_month11.csv", row.names = T)

#Diving into train and test; 80:20 split
train = window(ts_complete_data, start = c(2007,7), end = c(2009,12))
test = window(ts_complete_data, start = c(2010))

#Decomposition
plot(decompose(ts_complete_data))


#Check for stationarity
#There is non - stationarity in the dataset
adf.test(ts_complete_data, k = 1)#Stationarity in the dataset  
# null hypothesis = time series is non- stationary
#Alternative = time series is stationary
#P value less than 0.05 reject the null hypothesis

"
		Augmented Dickey-Fuller Test

data:  ts_complete_data
Dickey-Fuller = -5.5202, Lag order = 1, p-value = 0.01
alternative hypothesis: stationary

"

#Test for autocorrelation
dwtest(ts_complete_data[-37] ~ ts_complete_data[-1])

"
Durbin-Watson test

data:  train[-37] ~ train[-1]
DW = 1.9788, p-value = 0.4716
alternative hypothesis: true autocorrelation is greater than 0

"
#There seems to be no autocorrelation in the series
#Null hypothesis: There is no autocorrelation


set.seed(123)

#***********************Applying time series linear model with trend*********************

#Training the model
fit =  tslm(train~ season + trend)

#Summary of the fit
summary(fit)

#AIC score of tslm
AIC(fit)

#Residuals
checkresiduals(fit)
 
#Lets forcast for next 18 months
forecast_tslm = forecast(fit, h = 18)
autoplot(forecast_tslm)

#Lets check the accuracy
accuracy(forecast_tslm, test)


#**************************Applyting auto.arima****************************

#Checkingthe ACF and PACF PLOT
tsdisplay(train)
#*Autocorrelation: The correlation coefficient between different time points(lags) in a time series
#*Partial autocorrelation: The correlation coefficient adjusted for all shorter lags in a time series
#*the acf() is used to identify the moving average(MA) part of the ARIMA model, while pacf()
#identifies the values for the autoregressive part(AR)

#Training the model
arima_fit = auto.arima(train, trace = T)

#Forecasting
arimafore = forecast(arima_fit, h =18)
autoplot(arimafore)

#Accuracy
accuracy(arimafore, test)

#Residuals
checkresiduals(arima_fit)



#*********************ETS Model**********************


#Training the model
etsmodel = ets(train)
#Model appears to be "ANN" as expected as there is no trend and seasonality

#Forecasting
ets_forecast = forecast(etsmodel, h = 18)
autoplot(ets_forecast)

#Accuracy
accuracy(ets_forecast, test)

#Residuals
checkresiduals(etsmodel)



#*******Important Note********
"Increasing the test data is leading to RMSE: 29 and MAPE: 26 i.e a high error score.
Reason: There has been a lot of variation in the recent months for example May which has absentees's hours of 62.17 and 75
in 2008 and 2009 has a absentees hours 131 in 2010, taking this in test data is not correct as model is not able to predict
this drastic variation. I've tried all the model tuning operations but it doesn't work out as the model is not trained with
such a drastic change in trend. We can also not select random train and test since since this would prevent the model from catching autocorrelation which relies on the c
correct succession of the time data.
"
