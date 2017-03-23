#loading file
GDP=read.csv("F:/Study/5th sem/DATA Analytics/project/GDP_and_Major_Industrial_Sectors_of_Economy_Dataset.csv")
attach(GDP)
library(forecast)
#selecting GDP column in the Data
gdpprice<-as.character(GDP[,2])
gdpprice<-gsub(",","",gdpprice) #removing , in the digits
#converting into numeric value 
gdpprice<-as.numeric(gdpprice)
gdpprice<-gdpprice[1:60] #window size
gdp<-ts(gdpprice,start=c(1951,1),frequency = 1)#based on the window size change the year
#Using Auto Arima to forecast
fitx<-auto.arima(gdp)
fit7=forecast(fitx,h=3)
list(fit7)
accuracy(fit7)
year=1951:2010 #years for plotting the graph
plot(year,gdp,type="s")