'/the act of forecasting consists in saying that (something) will happen in the future or to predict (something, such as weather) after looking at the information that is available
Forecasting is a form of prediction. In time series analysis we often want to predict something in the future on the basis of what we have observed in the past. The evaluation of the accuracy of our prediction is an important part of model evaluation. Poor accuracy means that our prediction will be scarcely helpful.'/'

##we use the library forecaast to predict values
library(forecast)

### data are assigned to a convenient vector
### this is a easy way to avoid changing the code every time
file<-read.csv("GDP_and_Major_Industrial_Sectors_of_Economy_Dataset.csv")
attach(file)

##need to remove commas in the currency for proper analysis
gdpprice<-as.character(Gross.Domestic.Product..in.Rs..Cr..at.2004.05.Prices)
gdpprice <- gsub(",","",gdpprice)#this is done to remove commas in numbers
gdpprice<-as.numeric(gdpprice)

##reducing the size of currency
gdpprice<-gdpprice/1000

# plot the series (gdp series(fig1))
plot(year,gdpprice,xlab="year",ylab="gdp in crores(1000)", type="l", col="blue")

##creating a time series for yearly basis hence frequency=1 
#starting year=1951 
gdp<-ts(gdpprice,start = c(1951,1),frequency = 1)
##gdp time series output
'/Time Series:
Start = 1951 
End = 2012 
Frequency = 1 
 [1]  286.147  294.267  312.177  325.431  333.766  352.766  348.500  374.948  383.153  410.279
[11]  423.011  431.960  453.829  488.247  470.402  475.190  513.860  527.270  561.630  589.787
[21]  595.741  593.843  620.872  628.079  684.634  693.191  744.972  785.965  745.083  798.506
[31]  843.426  868.092  936.270  973.357 1013.866 1057.612 1094.993 1206.243 1280.228 1347.889
[41] 1367.171 1440.504 1522.344 1619.694 1737.741 1876.319 1957.032 2087.828 2246.276 2342.774
[51] 2472.052 2570.690 2777.813 2971.464 3253.073 3564.364 3896.636 4158.676 4516.071 4937.006
[61] 5243.582 5503.476
/'

##The accuracy of forecasts can only be determined by considering how well a model performs on new data that were not used when fitting the model.
##The size of the test set is typically about 20% of the total sample
##So, we will split the series in a training set and a test set
##sr is the training set from year 1951 to 2001 and ser the testing set from 2002 to 2012
sr = window(gdp, start=1951, end=c(2001,1))
'/Time Series:
Start = 1951 
End = 2001 
Frequency = 1 
 [1]  286.147  294.267  312.177  325.431  333.766  352.766  348.500  374.948  383.153  410.279
[11]  423.011  431.960  453.829  488.247  470.402  475.190  513.860  527.270  561.630  589.787
[21]  595.741  593.843  620.872  628.079  684.634  693.191  744.972  785.965  745.083  798.506
[31]  843.426  868.092  936.270  973.357 1013.866 1057.612 1094.993 1206.243 1280.228 1347.889
[41] 1367.171 1440.504 1522.344 1619.694 1737.741 1876.319 1957.032 2087.828 2246.276 2342.774
[51] 2472.052
/'

ser = window(gdp, start=2002, end=c(2012,1))
'/Time Series:
Start = 2002 
End = 2012 
Frequency = 1 
 [1] 2570.690 2777.813 2971.464 3253.073 3564.364 3896.636 4158.676 4516.071 4937.006 5243.582
[11] 5503.476
/'

##plotting the training set(fig 2)
plot(sr,ylim=c(0,5700),ylab="training set")
# plot forecasting according to four methods
lines(meanf(sr,h=11)$mean, col=4)
lines(rwf(sr,h=11)$mean, col=2)
lines(rwf(sr,drift=TRUE,h=11)$mean, col=3)
lines(snaive(sr,h=11)$mean, col=5)
# the test set
lines(ser, col="yellow")
# legend
legend("topleft", lty=1, col=c(4,2,3, 5,"yellow"),
legend=c("Mean method","Naive method","Drift method", "Seasonal naïve method","test set"),bty="n")
       

 # accuracy for forecasting of sr (forecasted data) on ser (original data used as test set)
 # the best model had the lowest error (particularly the MAPE, Mean absolute percentage error)

#mean method
accuracy(meanf(sr,h=11), ser)

#                     ME      RMSE       MAE       MPE     MAPE     MASE      ACF1 Theil's U
#Training set -4.470933e-14  590.5449  480.0805 -43.94060 70.61431 10.36583 0.9153349        NA
#Test set      3.032271e+03 3182.8685 3032.2710  75.38668 75.38668 65.47238 0.7465102  10.24887


#Naive method
accuracy(rwf(sr,h=11), ser)

#                    ME       RMSE        MAE       MPE      MAPE     MASE      ACF1 Theil's U
#Training set   43.7181   61.19399   46.31374  4.177788  4.610755  1.00000 0.6213352        NA
#Test set     1472.7526 1762.09671 1472.75264 33.322558 33.322558 31.79948 0.7465102  5.206299


#drift method
accuracy(rwf(sr,drift=TRUE,h=11), ser)
#                        ME       RMSE        MAE       MPE      MAPE       MASE      ACF1
#Training set -2.842171e-14   42.81859   33.82944 -2.550543  4.580792  0.7304408 0.6213352
#Test set      1.210444e+03 1467.52844 1210.44404 27.172517 27.172517 26.1357437 0.7493857
 #            Theil's U
#Training set        NA
#Test set      4.306161

#Seasonal naïve method
accuracy(snaive(sr,h=11), ser)
#                    ME       RMSE        MAE       MPE      MAPE     MASE      ACF1 Theil's U
Training set   43.7181   61.19399   46.31374  4.177788  4.610755  1.00000 0.6213352        NA
Test set     1472.7526 1762.09671 1472.75264 33.322558 33.322558 31.79948 0.7465102  5.206299




# plot the test set(fig 3)
sr.mean <- meanf(sr,h=11)$mean
sr.naive <- rwf(sr,h=11)$mean
sr.drift <- rwf(sr,drift=TRUE,h=11)$mean
sr.seas <- snaive(sr,h=11)$mean

plot(ser,ylim=c(0,5700),ylab="test data")

# plot forecasting for 4 years according to four methods
lines(sr.mean, col=4)
lines(sr.naive, col=2)
lines(sr.drift, col=3)
lines(sr.seas, col=5)

# legend
legend("topleft", lty=1, col=c(1,4,2,3,5),
       legend=c("test data","Mean method","Naive method","Drift method", "Seasonal naive method"),bty="n")

#INFERENCE
#It is rather obvious than none of these methods produce a good forecast of the series.
#The mean method and the naive method do not detect nor the trend neither the seasonality in the series. The drift method does detect the trend but not the seasonality, while the seasonal naïve method does the reverse. The best method, on the basis of the Mean absolute percentage error (MAPE) is the drift method, which in our suggests that the trend is more important than the seasonality in this series.


#ARIMA MODEL


trainData <- sr
testData <- ser
arimaMod <- auto.arima(trainData, stepwise=FALSE, approximation=FALSE)
arimaMod.Fr <-forecast(arimaMod,h=11)

# plot of the prediction and of the test set(fig4)

plot(arimaMod.Fr)
lines(testData, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

#plot of the test set and of the mean of the forecasted data(fig5)
AR.mean <-forecast(arimaMod,h=11)$mean
plot(testData, main="forecasted mean vs testdata", ylab="", xlab="year", col="darkblue")  
lines(AR.mean, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","darkblue"),c("forecasted mean","testdata"))

accuracy(arimaMod.Fr,testData)
#                    ME     RMSE       MAE       MPE      MAPE       MASE       ACF1 Theil's U
#Training set   6.72976  26.9974  20.19148  0.546328  2.472869  0.4359718 -0.2000336        NA
#Test set     754.56707 958.4402 758.39608 16.484107 16.633056 16.3751855  0.7570209  2.756751

#(test residues of arima (fig6))
tsdisplay(residuals(arimaMod))

#(Box-Ljung test)
lb <- Box.test(residuals(arimaMod), lag = 24, type = "Ljung-Box")
lb
##(OUTPUT)
#	Box-Ljung test

#data:  residuals(arimaMod)
#X-squared = 24.754, df = 24, p-value = 0.4192



#(BOX PIERCE TEST)
bp <- Box.test(residuals(arimaMod), lag = 24, type = "Box-Pierce")
bp
##(OUTPUT)
#	Box-Pierce test

#data:  residuals(arimaMod)
#X-squared = 18.639, df = 24, p-value = 0.7709



##Residuals diagnostics in forecasting(fig7)

res.fr <- residuals(arimaMod.Fr)

par(mfrow=c(1,3))

plot(res.fr, main="Residuals from ARIMA method",
     ylab="", xlab="Years")

Acf(res.fr, main="ACF of residuals")

u <- residuals(arimaMod)

m<-mean(u)
std<-sqrt(var(u))
hist(u, breaks=20, col="gray", prob=TRUE, 
     xlab="Residuals", main="Histogram of residuals\n with Normal Curve")
curve(dnorm(x, mean=m, sd=std), 
      col="black", lwd=2, add=TRUE)


#INFERENCE
#The ARIMA model had the lowest MAPE of all forecasting methods, and it is obvious from the plot that the prediction based on the ARIMA model detects both the trend and the seasonality of the series.
#The residuals of the model are reasonably good, and the LjungBox test shows that there is no serial correlation.
#The superiority of ARIMA over the other models of forecasting depends, in part, on ARIMA including a term for differerincing.





