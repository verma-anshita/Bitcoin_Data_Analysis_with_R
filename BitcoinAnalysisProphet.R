# Installing prophet library
#install.packages('prophet')
# Using prophet library for analysis on BitCoin data
library(prophet)
library(tidyverse)

bitcoin<- read.csv("Bitcoindata.csv")

head(bitcoin)

#Renaming Date and Close Column Names

colnames(bitcoin)[colnames(bitcoin) == "Date"] ="ds"
colnames(bitcoin)[colnames(bitcoin) == "Close"] ="y"

head(bitcoin)
summary(bitcoin)

#converting date column to date format
#bitcoin$ds<-as.Date(bitcoin$ds)
bitcoin$ds<- strptime(bitcoin$ds, format = "%m/%d/%Y %H:%M")
head(bitcoin)
summary(bitcoin)
sum(is.na(bitcoin))

#taking small subset of data
bitcoin2 <- bitcoin[c(1:6000),c(1:8)]
#Checking NA in Date Column
sum(is.na(bitcoin2))


# Fit the model using Prophet Function
Model1<- prophet(bitcoin2, yearly.seasonality = TRUE, weekly.seasonality = TRUE)
Future1 <- make_future_dataframe(Model1, periods = 365)
tail(Future1)

Forecast1 <- predict(Model1, Future1)

#Plotting the Model Estimates
dyplot.prophet(Model1, Forecast1)
prophet_plot_components(Model1, Forecast1)