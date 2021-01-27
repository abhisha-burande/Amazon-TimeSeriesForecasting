library(forecast)
library(zoo)
library(ggplot2)
library(dplyr)



# Create data frame
GDP.data <- read.csv("GDP.csv")
head(GDP.data)
tail(GDP.data) 

# Selecting required columns 
selected.var <- c(1,2) 
GDP_final.data <- GDP.data[, selected.var]

# See the first 6 records of the file.
head(GDP_final.data)
tail(GDP_final.data)


#----------------------------------------------------------------------------------------------------
#*******************************DATA EXPLORATION OF AMAZON REVENUE*************************************
#---------------------------------------------------------------------------------------------------
# Create Time Series DataSet For Amazon 
GDP.ts <- ts(GDP_final.data$US.GDP, 
                start = c(2005, 1), end = c(2020, 3), freq = 4)

plot(GDP.ts, 
     xlab = "TimeLine", ylab = "GDP",main = "Amazon GDP", col = "blue", lwd = 3)
lines(ma(GDP.ts,9),col="red",lwd=3)
# Use stl() Function to plot Times Series Components of the Original Data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
trend.season <- tslm(GDP.ts ~ trend + I(trend^2))

# See summary of quadratic trend and seasonality equation and associated parameters.
summary(trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in 12 future periods.
trend.season.pred <- forecast(trend.season, h = 4, level = 0)
trend.season.pred
