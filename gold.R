#DO NOT FORGET TO SET THE RIGHT WORKING DIRECTORY FIRST
library(forecast)
library(smooth)
library(readxl)


#read daily data
gold_daily_data <- read_excel("Gold Historical Prices.xlsx")
df_gold_daily_data <- data.frame(gold_daily_data)

# Average per weekday
agg <- aggregate(df_gold_daily_data$Price,by=list(df_gold_daily_data$Day), FUN=mean)
agg

#read monthly data
gold <- read.csv("data.csv", header=TRUE)

#check data
summary(gold)
boxplot(gold)
View(gold)

#separate data into prices and data point to run regression
gold_avg.prices <- gold$Avg.Price

goldtimeseries <- ts(gold_avg.prices, frequency = 12, start = c(1980, 01))
#see the times series
goldtimeseries
#decompose the time series
goldtimeseriescomponents<-stl(goldtimeseries, s.window = "periodic")
plot(goldtimeseriescomponents)

#the arima command unloke  the previous does not take confidence intervals
#while runing the algorithm. for auto.arima the confidence levels go to forecast commnand
f.arima<-auto.arima(goldtimeseries, D=1)
summary(f.arima)

f.arima_fitted<-f.arima$fitted
predict_gold_arima<-forecast(f.arima, h = 6, level = c(95,99))
hist(residuals(predict_gold_arima))
predict_gold_arima
plot(predict_gold_arima, type = "l", col = "black")
#lines do plot over plot but first there must be a plot
lines(f.arima$fitted, lty=2, col="red")
#legend at position (x,y)=(1988,80000)
legend(x = 1988, y = 80000, legend = c("data", "fitted"),
       col = c("black", "red"), lty = 1:2, cex = 0.8)