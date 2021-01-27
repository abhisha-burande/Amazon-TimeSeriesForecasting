#******************************************************************************************************
library(forecast)
library(zoo)
library(ggplot2)
library(dplyr)


# Create data frame
Amazon.data <- read.csv("Amazon_Profit_2005_2020.csv")
head(Amazon.data)
tail(Amazon.data) 

# Selecting required columns 
selected.var <- c(1,2,3) 
Amazon_final.data <- Amazon.data[, selected.var]

# See the first 6 records of the file.
head(Amazon_final.data)
tail(Amazon_final.data)


#----------------------------------------------------------------------------------------------------
#*******************************DATA EXPLORATION OF AMAZON REVENUE*************************************
#---------------------------------------------------------------------------------------------------
# Create Time Series DataSet For Amazon 
Amazon.ts <- ts(Amazon_final.data$Revenue, 
                start = c(2005, 1), end = c(2020, 3), freq = 4)

# Use Acf() Function To Identify Amazon Time Series Components
autocor_Amazon <- Acf(Amazon.ts, lag.max = 12, main = "Autocorrelation for Amazon Revenue") 
Lag <- round(autocor_Amazon$lag, 0)
ACF <- round(autocor_Amazon$acf, 3)
data.frame(Lag, ACF)

# Use plot() Function To plot Quaterly Amazon Time Series Data
plot(Amazon.ts, 
     xlab = "TimeLine", ylab = "Revenue",main = "Amazon Revenue", col = "blue", lwd = 3)
lines(ma(Amazon.ts,9),col="red",lwd=3)
# Use stl() Function to plot Times Series Components of the Original Data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
Amazon.stl <- stl(Amazon.ts, s.window = "periodic")
autoplot(Amazon.stl, main = "Amazon Time Series Component")

#*************** TEST PREDICTABILITY OF AMAZON .
# Create differenced Amazon Revenue data using (lag-1).
diff.Amazon.ts <- diff(Amazon.ts, lag = 1)
diff.Amazon.ts

# Use Acf() function to identify autocorrealtion for differenced 
# Amazon Revenue, and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(diff.Amazon.ts, lag.max = 12, 
    main = "Autocorrelation for Differenced Amazon Revenue Data")
plot(diff.Amazon.ts, 
     xlab = "TimeLine", ylab = "Revenue",main = "Predictability Test Amazon Revenue", col = "blue", lwd = 3)
lines(ma(diff.Amazon.ts,9),col="red",lwd=3)
#---------------------------------------------------------------------------------------------------------
#*************************************DATA PARTITIONING FOR AMAZON***************************************************
#---------------------------------------------------------------------------------------------------------
# creating data partition for AMAZON with the validation partition of 16 Quarters and
# rest training partition

nValid.az <- 16  
nTrain.az <- length(Amazon.ts) - nValid.az
train.ts.az <- window(Amazon.ts, start = c(2005, 1), end = c(2005, nTrain.az))
train.ts.az
valid.ts.az <- window(Amazon.ts, start = c(2005, nTrain.az + 1), 
                      end = c(2005, nTrain.az + nValid.az))
valid.ts.az


#-----------------------------------------------------------------------------------------------------------------------
#*********************************Execution of Forecasting Models*******************************************************
#-----------------------------------------------------------------------------------------------------------------------
#_____________________MODEL1 - 2 level Model (Regression + MA Trailing for Residuals)______________________________________________________
#***************************************************************************************************************************
# Regression Model with Quadratic Trend and Seasonality
reg.trend.seas <- tslm(train.ts.az ~ trend + I(trend^2) + season)
summary(reg.trend.seas)
# Creating regression forecast for the 16 Quarters of the validation period.
reg.trend.seas.pred <- forecast(reg.trend.seas, h = 16, level = 0)
reg.trend.seas.pred
# Identifying and displaying residuals for time series based on the regression
reg.trend.seas.res <- reg.trend.seas$residuals 
reg.trend.seas.res
# Applying trailing MA with 4 Quarters in the window to residuals.
ma.trailing.res_4 <- rollmean(reg.trend.seas.res, k = 4 , align = "right") 
ma.trailing.res_4
# Creating forecast for residuals for the 16 Quarters of the validation period
ma.trailing.res_4.pred <- forecast(ma.trailing.res_4, h = 16, level = 0) 
ma.trailing.res_4.pred
# combining regression forecast and trailing MA forecast for residuals(window=4)
ts.forecast.4 <- reg.trend.seas.pred$mean + ma.trailing.res_4.pred$mean 
ts.forecast.4
# Creating a table with regression forecast, trailing MA for residuals and total forecast for 
# 4 Quarters into the future
total.reg.ma.pred <- data.frame(reg.trend.seas.pred$mean, ma.trailing.res_4.pred$mean, 
                                ts.forecast.4)
total.reg.ma.pred 

# Plot training data and regression model.
plot(train.ts.az, 
     xlab = "Time", ylab = "Revenue",ylim=c(0,110000), bty = "l",
     xaxt = "n", xlim = c(2005, 2022.75), lwd =2,
     main = "Training series and Regression with Trend and Seasonality") 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(reg.trend.seas$fitted, col = "brown", lwd = 2)
lines(reg.trend.seas.pred$mean, col = "brown", lty =5, lwd = 2)
lines(valid.ts.az)
legend(2005,95000, legend = c("Training series", "Regression",
                              "Regression Forecast for 16 Periods into Validation"), 
       col = c("black", "brown" , "brown"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
lines(c(2019.50 - 3, 2019.50 - 3), c(0, 120000))
lines(c(2020.75, 2020.75), c(0, 120000))
text(2010, 110000, "Training")
text(2018.50, 110000, "Validation")
text(2021.75, 110000, "Future")
arrows(2020 - 3.5,100000, 2005, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3.5, 100000, 2020.5, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2022 - 1.25, 100000, 2022.75, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot regression residuals data and trailing MA based on residuals.
plot(reg.trend.seas.res, 
     xlab = "Time", ylab = "Revenue", bty = "l", ylim=c(-4000,10000),
     xaxt = "n", xlim = c(2005, 2022.75),lwd =2,
     main = "Regression Residuals and Trailing MA for Residuals, k =4") 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(ma.trailing.res_4, col = "blue", lwd = 2, lty = 1)
lines(ma.trailing.res_4.pred$mean, col = "blue", lwd = 2, lty = 5)
legend(2005,9000, legend = c("Regresssion Residuals", "Trailing MA for Residuals, k=4",
                            "Trailing MA Forecast for validation"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Use accuracy() function to identify common accuracy measures for regression model and two level forecast.
round(accuracy(reg.trend.seas.pred, valid.ts.az), 3) # RMSE=19192.227, MAPE=22.838
round(accuracy(ts.forecast.4, valid.ts.az), 3)# RMSE=13127.41, MAPE=14.622

#-----------------------------------------------------------------------------------------------------------------------
#____________________________MODEL 2 - Holt Winter's Model for Amazon ______________________________________________________
#***************************************************************************************************************************
hw.ZZZ.train.az <- ets(train.ts.az, model = "ZZZ")  #Model Received is AAA
hw.ZZZ.train.az 

hw.ZZZ.train.pred.az <- forecast(hw.ZZZ.train.az, h = nValid.az, level = 0)
hw.ZZZ.train.pred.az

# Plot hw predictions for training data, optimal smoothing parameters.
plot(hw.ZZZ.train.pred.az, 
     xlab = "Time", ylab = "Amazon Revenue ($Billion)",ylim=c(0,120000), bty = "l",
     xaxt = "n", xlim = c(2005, 2022.75), 
     main = "Holt-Winter's Model with Automated Selection of Model Options", flty = 2) 
axis(1, at = seq(2005, 2022.75, 1), labels = format(seq(2005, 2022, 1)))
lines(hw.ZZZ.train.pred.az$fitted, col = "blue", lwd = 3)
lines(valid.ts.az)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019.50 - 3, 2019.50 - 3), c(0, 120000))
lines(c(2020.75, 2020.75), c(0, 120000))
text(2010, 120000, "Training")
text(2018.5, 120000, "Validation")
text(2021.75, 120000, "Future")
arrows(2020 - 3.5, 110000, 2005, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3, 110000, 2020, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 110000, 2022.75, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
legend(2005,85000, legend = c("Amazon Revenue time series", "Holt-Winter's Model for Training Data",
                             "Holt-Winter's Model for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

round(accuracy(hw.ZZZ.train.pred.az, valid.ts.az), 3) #RSME=12763.096 MAPE=13.166
#____________________________MODEL 3 - Regression ______________________________________________________
#***************************************************************************************************************************
#Model 1: Regression model with linear trend
train.az.lin <- tslm(train.ts.az ~ trend)
summary(train.az.lin)

#Forecasting for Validation period:
train.az.lin.pred <- forecast(train.az.lin, h = nValid.az, level = 0)
train.az.lin.pred

# Plot ts data, linear trend and predictions for validation period.
plot(train.az.lin.pred, 
     xlab = "Time", ylab =  "Amazon Revenue ($Billion)", bty = "l",ylim=c(0, 120000),
     xlim = c(2005, 2022.75), main = "Linear Trend for Training and Validation data", flty = 2) 
#axis(1, at = seq(2005, 2022.75, 1), labels = format(seq(2005, 2022, 1)))
lines(train.az.lin.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts.az, col = "black", lty = 1)
legend(2005,85000, legend = c("AMAZON Revenue time series", "Linear Regression for Training Data",
                              "Linear forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019.5 - 3, 2019.5 - 3), c(0, 120000))
lines(c(2020.75, 2020.75), c(0, 120000))
text(2010, 120000, "Training")
text(2018.50, 120000, "Validation")
text(2021.75, 120000, "Future")
arrows(2020 - 3.5,110000, 2005, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3, 110000, 2020, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 110000, 2022.75,110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#---------------------------------------------------------------------------------------------------
#Model 2: Regression Model with Exponential Trend:
train.az.expo <-tslm(train.ts.az ~ trend, lambda = 0)
summary(train.az.expo)

#Forecasting for Validation period:
train.az.expo.pred <- forecast(train.az.expo, h = nValid.az, level = 0)
train.az.expo.pred

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.az.expo.pred, 
     xlab = "Time", ylab = "Amazon Revenue ($Billion)", bty = "l",ylim=c(1000, 120000),
     xlim = c(2005, 2022), main = "Exponential Trend for Training and Validation Data", flty = 2) 
#axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(train.az.expo.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts.az, col = "black", lty = 1)
lines(train.ts.az,col="black",lty=1)
legend(2005,85000, legend = c("AMAZON Revenue time series", "Exponential Trend for Training Data",
                              "Exponential forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019.5 - 3, 2019.5 - 3), c(0, 120000))
lines(c(2020.75, 2020.75), c(0, 120000))
text(2010, 120000, "Training")
text(2018.50, 120000, "Validation")
text(2021.75, 120000, "Future")
arrows(2020 - 3.5, 110000, 2005, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3, 110000, 2020, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 110000, 2022.75,110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#---------------------------------------------------------------------------------------------------
#Model 3: Regression mode with quadratic trend
train.az.quad <- tslm(train.ts.az~ trend + I(trend^2))
summary(train.az.quad)

#Forecasting for Validation period:
train.az.quad.pred <- forecast(train.az.quad, h = nValid.az, level = 0)
train.az.quad.pred

# Plot ts data, quadratic trend and predictions for validation period.
plot(train.az.quad.pred, 
     xlab = "Time", ylab = "Amazon Revenue ($Billion)", bty = "l",ylim=c(1000, 120000),
     xlim = c(2005, 2022), main = "Quadratic Trend for Training and Validation Data", flty = 2) 
#axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(train.az.quad.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts.az, col = "black", lty = 1)
legend(2005,85000, legend = c("AMAZON Revenue time series", "Quadratic Trend for Training Data",
                              "Quadratic Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019.5 - 3, 2019.5 - 3), c(0, 120000))
lines(c(2020.75, 2020.75), c(0, 120000))
text(2010, 120000, "Training")
text(2018.50, 120000, "Validation")
text(2021.75, 120000, "Future")
arrows(2020 - 3.5, 110000, 2005, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3, 110000, 2020, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 110000, 2022.75,110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#---------------------------------------------------------------------------------------------------

#Model 4:Regression model with seasonality
train.az.season <- tslm(train.ts.az ~ season) 
summary(train.az.season)

#Forecasting for Validation period:
train.az.season.pred <- forecast(train.az.season, h = nValid.az, level = 0)
train.az.season.pred

# Plot ts data, linear trend and predictions for validation period.
plot(train.az.season.pred, 
     xlab = "Time", ylab = "Amazon Revenue ($Billion)", bty = "l",ylim=c(1000, 120000),
     xlim = c(2005, 2022), main = "Model with Seasonality for Training and Validation Data", flty = 2) 
#axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(train.az.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts.az, col = "black", lty = 1)
legend(2005,85000, legend = c("AMAZON Revenue time series", "Seasonality Model Training Data",
                              "Seasonality forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019.5 - 3, 2019.5 - 3), c(0, 120000))
lines(c(2020.75, 2020.75), c(0, 120000))
text(2010, 120000, "Training")
text(2018.50, 120000, "Validation")
text(2021.75, 120000, "Future")
arrows(2020 - 3.5, 110000, 2005, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3, 110000, 2020, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 110000, 2022.75,110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#---------------------------------------------------------------------------------------------------
#Model 5: Regression model with quadratic trend and seasonality.
train.az.trend.season <- tslm(train.ts.az ~ trend + I(trend^2) + season)
summary(train.az.trend.season)

train.az.trend.season.pred <- forecast(train.az.trend.season, h = nValid.az, level = 0)
train.az.trend.season.pred

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.az.trend.season.pred, 
     xlab = "Time", ylab = "Amazon Revenue ($Billion))", bty = "l",ylim=c(0, 120000),
     xlim = c(2005, 2022.75), main = "Model with Quadratic Trend and Quarterly Seasonality", flty = 2) 
#axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(train.az.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts.az, col = "black", lty = 1)
legend(2005,85000, legend = c("AMAZON Revenue time series", "Trend and Seasonality Model Training Data",
                              "Trend and Seasonality for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019.5 - 3, 2019.5 - 3), c(0, 120000))
lines(c(2020.75, 2020.75), c(0, 120000))
text(2010, 120000, "Training")
text(2018.50, 120000, "Validation")
text(2021.75, 120000, "Future")
arrows(2020 - 3.5, 110000, 2005, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3, 110000, 2020, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 110000, 2022.75,110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot residuals of predictions with trend and seasonality.
plot(train.az.trend.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", bty = "l",
     xlim = c(2005, 2022), main = "Residuals for Trend and Seasonality Model", 
     col = "brown", lwd = 2,ylim=c(-600,50000)) 
#axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(valid.ts.az - train.az.trend.season.pred$mean, col = "brown", lty = 1, lwd=2)
lines(c(2019.5 - 3, 2019.5 - 3), c(0, 100000))
lines(c(2020.75, 2020.75), c(0, 100000))
text(2010, 50000, "Training")
text(2018.50, 50000, "Validation")
text(2021.75, 50000, "Future")
arrows(2020 - 3.5, 45000, 2005, 45000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3, 45000, 2020, 45000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 45000, 2022.75,45000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#-------------------------------------------------------------------------------------------------------
#Comparing Accuracy for Above 5 MLR models For AMAZON QUARTERLY Revenue Validation Data

#Accuracy for Model 1: Regression model with linear trend
round(accuracy(train.az.lin.pred, valid.ts.az), 3)  #RMSE: 32450.33 , MAPE: 43.003

#Accuracy for Model2:  Regression Model with Exponential Trend:
round(accuracy(train.az.expo.pred, valid.ts.az), 3) #RMSE: 11414.85 , MAPE: 16.565 (best 2) 

#Accuracy for Model 3:Regression mode with quadratic trend
round(accuracy(train.az.quad.pred, valid.ts.az), 3)  #RMSE: 20293.347 , MAPE:24.104

#Accuracy for Model 4:Regression model with seasonality
round(accuracy(train.az.season.pred, valid.ts.az), 3)   #RMSE:52755.052 , MAPE: 78.341 

#Accuracy for Model 5: Regression model with quadratic trend and seasonality.  
round(accuracy(train.az.trend.season.pred, valid.ts.az),3) #RMSE:19192.227 #MAPE: 22.838   ( BEST)

#----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------
#MODEL 4 - Autocorrelation and Autoregressive model for Amazon ______________________________________________________
#***************************************************************************************************************************
## FIT REGRESSION MODEL WITH Exponential TREND.
train.expo.trend.season  <- tslm(train.ts.az ~ trend , lambda = 0)
summary(train.expo.trend.season )

# Apply forecast() function to make predictions for ts with 
# trend and seasonal model in validation set.  
train.expo.trend.season.pred <- forecast(train.expo.trend.season , h = nValid.az, level = 0)
train.expo.trend.season.pred

# plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.expo.trend.season.pred, 
     xlab = "Time", ylab = "Amazon Sales (in millions $) ",ylim=c(0,120000), bty = "l",
     xaxt = "n", xlim = c(2005, 2022.75), 
     main = "Regression with Exponential Trend", lwd = 2, flty = 5) 
axis(1, at = seq(2005, 2022.75, 1), labels = format(seq(2005, 2022, 1)))
lines(train.expo.trend.season.pred$fitted, col = "red", lwd = 2)
lines(valid.ts.az, col = "black", lwd = 2, lty = 1)
lines(train.ts.az, col = "black", lwd = 2, lty = 1)
legend(2005,85000, legend = c("Amazon Time Series", "Regression for Training Data",
                              "Forecast for Validation Data"), 
       col = c("black", "red" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019.50 - 3, 2019.50 - 3), c(0, 120000))
lines(c(2020.75, 2020.75), c(0, 120000))
text(2010, 120000, "Training")
text(2018.5, 120000, "Validation")
text(2021.75, 120000, "Future")
arrows(2020 - 3.5, 110000, 2005, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3, 110000, 2020, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 110000, 2022.75, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot residuals of the predictions with trend and seasonality.
plot(train.expo.trend.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals",bty = "l",
     xaxt = "n", xlim = c(2005, 2022.75),
     main = "Regresssion Residuals for Training and Validation Data", 
     col = "brown", lwd = 2) 
axis(1, at = seq(2005, 2022.75, 1), labels = format(seq(2005, 2022, 1)))
lines(valid.ts.az - train.expo.trend.season.pred$mean, col = "brown", lwd = 2, lty = 1)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019.50 - 3, 2019.50 - 3), c(-3000, 100000))
lines(c(2020.75, 2020.75), c(-3000, 100000))
text(2010, 3000, "Training")
text(2018.5, 3000, "Validation")
text(2021.75, 3000, "Future")
arrows(2020 - 3.5, 2500, 2005, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3, 2500, 2020, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 2500, 2022.75, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for the model residuals 
Acf(train.expo.trend.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Amazon Training Residuals")

## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
res.ar1 <- Arima(train.expo.trend.season.pred$residuals, order = c(5,0,0))
summary(res.ar1)

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid.az, level = 0)
res.ar1.pred

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- data.frame(train.ts.az, train.expo.trend.season$fitted, 
                       train.expo.trend.season$residuals, res.ar1$fitted, res.ar1$residuals)
names(train.df) <- c("Amazon Revenue", "Regression", "Residuals",
                     "AR.Model", "AR.Model.Residuals")
train.df

# Plot residuals of the predictions for training data before AR(1).
plot(train.expo.trend.season.pred$residuals, 
     xlab = "Time", ylab = "Residuals", bty = "l",
     xaxt = "n", xlim = c(2005, 2022), 
     main = "Regresssion Residuals for Training Data before AR(1)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2005, 2022.75, 1), labels = format(seq(2005, 2022, 1)))

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019.50 - 3, 2019.50 - 3), c(-3000, 40000))
lines(c(2020.75, 2020.75), c(-3000, 40000))
text(2010, 3000, "Training")
text(2018.5, 3000, "Validation")
text(2021.75, 3000, "Future")
arrows(2020 - 3.5, 2500, 2005, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3, 2500, 2020, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 2500, 2022.75, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Plot residuals of the residuals for training data after AR(5).
plot(res.ar1$residuals, 
     xlab = "Time", ylab = "Residuals",bty = "l",
     xaxt = "n", xlim = c(2005, 2022), 
     main = "Residuals of Residuals for Training Data after AR(5)", 
     col = "brown", lwd = 3) 
axis(1, at = seq(2005, 2022.75, 1), labels = format(seq(2005, 2022, 1)))

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019.50 - 3, 2019.50 - 3), c(-3000, 3000))
lines(c(2020.75, 2020.75), c(-3000, 3000))
text(2010, 3000, "Training")
text(2018.5, 3000, "Validation")
text(2021.75, 3000, "Future")
arrows(2020 - 3.5, 2500, 2005, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3, 2500, 2020, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 2500, 2022.75, 2500, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrrelation for different lags 
Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Amazon Training Residuals of Residuals")


# Create two-level modeling results, regression + AR(1) for validation period.
valid.two.level.pred <- train.expo.trend.season.pred$mean + res.ar1.pred$mean
# Plot two-level modeling results, regression + AR(1) for validation period.
plot(valid.two.level.pred, 
     xlab = "Time", ylab = "Revenue (in milions$)",ylim=c(0,120000),bty = "l",
     xaxt = "n", xlim = c(2005, 2022.75), 
     main = "Two level(Exponential trend + AR(5)) Model", lwd = 2, lty = 5, col = "blue") 
axis(1, at = seq(2005, 2022.75, 1), labels = format(seq(2005, 2022, 1)))
lines(train.expo.trend.season.pred$fitted , col = "blue", lwd = 2)
lines(valid.ts.az, col = "black", lwd = 2, lty = 1)
lines(train.ts.az, col = "black", lwd = 2, lty = 1)
legend(2005,85000, legend = c("Amazon Time Series", "Two level Model for Training Period",
                              "Two level Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019.50 - 3, 2019.50 - 3), c(0, 120000))
lines(c(2020.75, 2020.75), c(0, 120000))
text(2010, 120000, "Training")
text(2018.5, 120000, "Validation")
text(2021.75, 120000, "Future")
arrows(2020 - 3.5, 110000, 2005, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3, 110000, 2020,110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 110000, 2022.75, 110000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
# Create data table with historical validation data, regression forecast
# for validation period, AR(1) for validation, and two level model results. 
valid.df <- data.frame(valid.ts.az, train.expo.trend.season.pred$mean, 
                       res.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Amazon Revenue", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

# Use accuracy() function to identify common accuracy measures for validation period forecast:
# (1) two-level model (quadratic trend and seasonal model + AR(1) model for residuals)
round(accuracy(valid.two.level.pred, valid.ts.az), 3) #RMSE=11414.8 MAPE 16.565

#____________________________MODEL 5 - ARIMA model for Amazon ______________________________________________________
#***************************************************************************************************************************
## FIT AUTO ARIMA MODEL.

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima.az <- auto.arima(train.ts.az)
summary(train.auto.arima.az)

#Use Acf() to create autocorrelation chart
Acf(train.auto.arima.az$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")  #We only get random noise as the model incorporates all the components - level,trend, seasonality.


# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.az.pred <- forecast(train.auto.arima.az, h = nValid.az, level = c(80,95))
train.auto.arima.az.pred

# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.auto.arima.az.pred, 
     xlab = "Time", ylab = "Amazon Revenue ($Billion)", bty = "l",
     xaxt = "n", xlim = c(2005, 2022.75), ylim = c(0,110000),
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(2005, 2022.75, 1), labels = format(seq(2005, 2022, 1)))
lines(train.auto.arima.az.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts.az, col = "black", lwd = 2, lty = 1)
legend(2004,85000, legend = c("Amazon Time Series", "Auto ARIMA Model for Training Period",
                              "Auto ARIMA Forecast for Validation Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019.5 - 3, 2019.5 - 3), c(0, 100000))
lines(c(2020.75, 2020.75), c(0, 100000))
text(2010, 110000, "Training")
text(2018.5, 110000, "Validation")
text(2021.75, 110000, "Future")
arrows(2020 - 3.5, 100000, 2005, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020 - 3, 100000, 2020, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 100000, 2022.75, 100000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Accuracy on the validation dataset
round(accuracy(train.auto.arima.az.pred, valid.ts.az), 3)# RMSE = 7471.543, MAPE = 7.813

##########################################################################################
#------ACCURACY FOR VALIDATION DATA---------------------------------------------------------
#########################################################################################
#Model1- Two level (Regression + MA Trailing for Residuals)
round(accuracy(reg.trend.seas.pred, valid.ts.az), 3) # RMSE=19192.227, MAPE=22.838

#Model 2-Holt's Winter
round(accuracy(hw.ZZZ.train.pred.az, valid.ts.az), 3) #RSME=12763.096 MAPE=13.166

#Model 3- Regression
#Regression model with linear trend
round(accuracy(train.az.lin.pred, valid.ts.az), 3)  #RMSE: 32450.33 , MAPE: 43.003

#Regression Model with Exponential Trend:
round(accuracy(train.az.expo.pred, valid.ts.az), 3) #RMSE: 11414.85 , MAPE: 16.565 (best 2) 

#Regression mode with quadratic trend
round(accuracy(train.az.quad.pred, valid.ts.az), 3)  #RMSE: 20293.347 , MAPE:24.104

#Regression model with seasonality
round(accuracy(train.az.season.pred, valid.ts.az), 3)   #RMSE:52755.052 , MAPE: 78.341 

#Regression model with quadratic trend and seasonality.  
round(accuracy(train.az.trend.season.pred, valid.ts.az),3) #RMSE:19192.227 #MAPE: 22.838


#Model 4- Two level Model (expo trend + AR(5)) Model)
round(accuracy(valid.two.level.pred, valid.ts.az), 3) #RMSE=19196.08 MAPE 22.854

#Model 5- ARIMA
round(accuracy(train.auto.arima.az.pred, valid.ts.az), 3)# RMSE = 7471.543, MAPE = 7.813


########################################################################################
# Two Best Model- ARIMA and Holt's Winter
# Fitting on Entire Data Set

## FORECAST AUTO ARIMA MODEL FOR ENTIRE DATA SET. 
#FIRST BEST MODEL: AUTO ARIMA
# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(Amazon.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 4 periods. 
auto.arima.pred <- forecast(auto.arima, h = 4, level = c(85,95))
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")


# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.auto.arima.az.pred, 
     xlab = "Time", ylab = "Amazon Revenue ($Billion)", bty = "l",
     xaxt = "n", xlim = c(2005, 2022.75), ylim = c(0,180000),
     main = "Auto ARIMA Model", lwd = 2, flty = 5) 
axis(1, at = seq(2005, 2022.75, 1), labels = format(seq(2005, 2022, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2004,150000, legend = c("Amazon Time Series", "Auto ARIMA Model Forecast",
                               "Auto ARIMA Forecast for 4 quarters"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
#lines(c(2019.5 - 3, 2019.5 - 3), c(0, 180000))
lines(c(2020.75, 2020.75), c(0, 180000))
text(2012, 180000, "Training")
text(2022, 180000, "Future")
arrows(2020 , 170000, 2005, 170000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 170000, 2022.75, 170000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.
# Use accuracy() function to identify common accuracy measures for:
#Auto ARIMA Model
round(accuracy(auto.arima.pred$fitted, Amazon.ts), 3)

#SECOND BEST MODEL:HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 4 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full  data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(Amazon.ts, model = "ZZZ")
HW.ZZZ # Model appears to be (M, A, M), with alpha =0.6077,Beta=0.2715,gamma =0.3923.

# Use forecast() function to make predictions using this HW model for
# 4 period into the future.
length <- length(Amazon.ts)
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 4 , level =c(85,95))
HW.ZZZ.pred
HW.ZZZ.pred$fitted

# plot HW predictions for original data, optimal smoothing parameters.
plot(HW.ZZZ.pred, 
     xlab = "Time", ylab = "Amazon Revenue ($Billion)", ylim = c(0,180000), bty = "l",
     xaxt = "n", xlim = c(2005, 2022.75), lwd = 2,
     main = "Holt-Winter's Model and Forecast for Future Periods", 
     flty = 5) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
#lines(c(2019.5 - 3, 2019.5 - 3), c(0, 180000))
lines(c(2020.50, 2020.50), c(0, 180000))
text(2012, 180000, "Training")
text(2022, 180000, "Future")
arrows(2020 , 170000, 2005, 170000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 170000, 2022.75, 170000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

round(accuracy(HW.ZZZ.pred$fitted, Amazon.ts), 3)
#############################################################################
#Model 6: Regression model with Quadratic Trend, seasonality and External Variables

#Creating time series for 1 external variables
#For Amazon Revenue, we will be using 1 external variables-- US GDP
# FIRST FIND CORRELATION OF EXTERNAL VARIABLE
Corr_1 <- cor.test(Amazon_final.data$Revenue,Amazon_final.data$US.GDP, method = "pearson")
Corr_1 # Correlation 0.922

# Create time series data
gdp.ts <- ts(Amazon_final.data$US.GDP, 
             start = c(2005, 1), end = c(2020, 3), freq = 4)

head(gdp.ts)
tail(gdp.ts)

 

##Using Regression Model with quadratic trend and seasonality for Entire Amazon Dataset.
#az.trend.season.external <- tslm(Amazon.ts ~ trend + I(trend^2) + season
#                                 + gdp.ts)
az.expo.trend.external <- tslm(Amazon.ts ~ trend + gdp.ts, lambda = 0)
summary(az.expo.trend.external)
#Significant Variables achieved are: Trend,Trend^2, Season4-Qtr4 and US GDP.
#R-squared = 0.9788
#Adj R Square = 0.9781

# To forecast 4 quarters of 2020-2021, develop the values of the variables for those periods.
#newdata <- data.frame(trend = c(65:68), 
#                      season2 = c(0,1,0,0), season3 = c(0,0,1,0), season4 = c(0,0,0,1), 
#                      gdp.ts = c( 22093.09,22302.84,22514.73,22728.77)) 
newdata <- data.frame(trend = c(65:68),gdp.ts = c( 22093.09,22302.84,22514.73,22728.77))                       
newdata

##Forecasting Amazon Revenue. 
az.expo.trend.external.pred <- forecast(az.expo.trend.external, newdata = newdata, level = 0)
az.expo.trend.external.pred

plot(az.expo.trend.external.pred, 
     xlab = "Time", ylab = "Amazon Revenue ($Billion)", bty = "l",ylim=c(0, 150000),
     xlim = c(2005, 2023), main = "Model with Exponential Trend with External variables", flty = 2) 
#axis(1, at = seq(2005, 2023, 1), labels = format(seq(2005, 2023, 1)))
lines(az.expo.trend.external.pred$fitted, col = "blue", lwd = 2)
legend(2005,100000, legend = c("Amazon Revenue time series", "Trend with External variables for Training Data",
                             "Trend with External variables for Future Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2020.50, 2020.50), c(0, 150000))
text(2012, 150000, "Training")
text(2022, 150000, "Future")
arrows(2020 , 140000, 2005, 140000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2020.75, 140000, 2022.75, 140000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Accuracy for Entire Data
round(accuracy(az.expo.trend.external.pred$fitted,Amazon.ts),3) #RMSE:4459.365 , MAPE:12.787  

#  MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.
round(accuracy(auto.arima.pred$fitted, Amazon.ts), 3) #MAPE-3.624, RMSE-1538.811
round(accuracy(HW.ZZZ.pred$fitted, Amazon.ts), 3) #MAPE-4.203, RMSE-1921.9 
round(accuracy((naive(Amazon.ts))$fitted, Amazon.ts), 3) #MAPE-18.5 RMSE-6201.657
round(accuracy((snaive(Amazon.ts))$fitted, Amazon.ts), 3) # MAPE-22.173 RMSE-8211.3

