#loading file
DP=read.csv("F:/Study/5th sem/DATA Analytics/project/GDP_and_Major_Industrial_Sectors_of_Economy_Dataset.csv")
attach(GDP)
library(forecast)
#selecting GDP column in the Data
gdpprice<-as.character(GDP[,2])
#removing , in the digits
gdpprice<-gsub(",","",gdpprice) 
gdpprice<-as.numeric(gdpprice)
gdpprice<-gdpprice[1:60] #window size
gdp<-ts(gdpprice,start=c(1951,1),frequency = 1)#based on the window size change the year
#selecting years for plotting the graph
year<-1951:2010
#Using Auto Arima to forecast
fit<-HoltWinters(gdp,gamma=FALSE,beta=FALSE)
fit$fitted
z<-forecast(fit,3)
z
accuracy(z)
plot(year,gdp,type="s")
