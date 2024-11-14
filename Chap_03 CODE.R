setwd("C:/Users/Harsh Deep/OneDrive/Desktop/BUA 650/Case Study 3")

# Importing the dataset
data <- read.csv("Chapter_03.csv", header = TRUE, sep = ",")
data.class(data)

# Convert the second column of data to a time series object
data_ts <- ts(data[, 2], start = c(1992, 1), frequency = 12)

# Data exploration
start(data_ts)              # Starting point of the time series
end(data_ts)                # End point of the time series
frequency(data_ts)          # Frequency of observations (monthly)
summary(data_ts)            # Descriptive summary
sum(is.na(data_ts))         # Check for missing values
head(data_ts)               # checking first few entries
tail(data_ts)               # checking last entries


# Plotting the original time series data
plot(data_ts, 
     main = "Years vs Sales (Time Series Plot)", 
     xlab = "Years", 
     ylab = "Sales (in millions)", 
     col = "blue")


adf_test <- adf.test(data_ts, alternative = "stationary")
print(adf_test)

# Load the astsa library for ACF/PACF analysis
library(astsa)

# Plot ACF and PACF of the original data
acf2(data_ts, max.lag = 24, main = "ACF & PACF of Original Sales Data")


# Seasonally differencing the retail sales data
datadiff12 <- diff(data_ts, 12)

# Plotting seasonally differenced retail sales
plot.ts(datadiff12, 
        main = "Seasonally Differenced Sales Data", 
        ylab = "Differenced Sales", 
        xlab = "Year", 
        col = "red")

# Differencing to remove trend and seasonality
diff1and12 <- diff(datadiff12, 1)

# Plotting trend and seasonally differenced retail sales
plot(diff1and12, 
     main = "Trend & Seasonally Differenced Sales Data", 
     xlab = "Year", 
     ylab = "Sales Difference", 
     col = "blue")

# Plot ACF and PACF of trend and seasonally differenced data
acf2(diff1and12, max.lag = 36, main = "ACF & PACF of Differenced Sales Data")

# Load the forecast library for ARIMA modeling and forecasting
library(forecast)

# Building seasonal ARIMA(2,1,1)(2,1,2)[12] model
model1 <- arima(data_ts, order = c(2, 1, 1), 
                seasonal = list(order = c(2, 1, 2), period = 12))
summary(model1)

# Residual diagnostics for model1
Acf(residuals(model1), main = "ACF of Residuals for ARIMA(2,1,1)(2,1,2)[12]")
Box.test(residuals(model1), lag = 24, fitdf = 1, type = "Ljung")

# Rebuilding model with different non-seasonal terms: ARIMA(6,1,1)(2,1,2)[12]
model2 <- arima(data_ts, order = c(6, 1, 1), 
                seasonal = list(order = c(2, 1, 2), period = 12))
summary(model2)

# Residual diagnostics for model2
Acf(residuals(model2), main = "ACF of Residuals for ARIMA(6,1,1)(2,1,2)[12]")
Box.test(residuals(model2), lag = 24, fitdf = 1, type = "Ljung")

# Compare AIC values to select the better model
aic_model1 <- AIC(model1)
aic_model2 <- AIC(model2)
cat("AIC for Model 1 (SARIMA(2,1,1)(2,1,2)[12]):", aic_model1, "\n")
cat("AIC for Model 2 (SARIMA(6,1,1)(2,1,2)[12]):", aic_model2, "\n")

print(aic_model1)
print(aic_model2)

better_model <- if (aic_model1 < aic_model2) model1 else model2
cat(if (aic_model1 < aic_model2) "Model 1" else "Model 2", 
    "is better based on AIC.\n")

# Forecasting the next 30 months
Pred <- forecast(model2, h = 30)
print(Pred)

# Plotting the forecast
plot(Pred, 
     main = "30-Month Forecast of Retail Sales", 
     xlab = "Year", 
     ylab = "Sales (in millions)", 
     col = "black")

# Use auto.arima
auto_model <- auto.arima(data_ts, start.p = better_model$arma[1], start.q = better_model$arma[2],
                         start.P = better_model$arma[5], start.Q = better_model$arma[6],
                         seasonal = TRUE)
summary(auto_model)

# Plotting auto forecast
auto_forecast <- forecast(auto_model, h = 30)
print(auto_forecast)
plot(auto_forecast, ylab="Sales (million in dollars)", xlab="Year", main="Auto ARIMA Forecast Model 2", fcol="red")


# comparison plots
plot(Pred, ylab="Sales (million in dollars)", xlab="Year", 
     main="coparison of forecast: Manual ARIMA vs Auto ARIMA", 
     col=c("black", "blue"), lwd=2, xlim=c(2018, max(time(auto_forecast$mean))))
lines(auto_forecast$mean, col="red", lwd=2)  # Plot Automatic ARIMA in red

# Adding legend
legend("bottomright", legend=c("Manual SARIMA Model 2", "Automatic ARIMA"), 
       col=c("blue", "red"), lty=1, lwd=2)


# Load ChatGPT SARIMA forecast
chatgpt_forecast <- read.csv("chatGPT_forecast.csv")

# Convert the date column if needed
chatgpt_forecast$Period <- as.Date(chatgpt_forecast$Period)
chatgpt_forecast_ts <- ts(chatgpt_forecast$Point.Forecast, start=c(2017, 10), frequency=12)

# Comparison plots
plot(Pred, ylab="Sales (million in dollars)", xlab="Year", 
     main="comparison of forecast: Manual ARIMA vs. Auto ARIMA vs. ChatGPT ARIMA", 
     col=c("black", "blue"), lwd=2, xlim=c(2018, max(time(auto_forecast$mean))))
lines(auto_forecast$mean, col="red", lwd=2)  # Plot Automatic ARIMA in red
lines(chatgpt_forecast_ts, col="green", lwd=2, lty=2)  # Plot ChatGPT SARIMA in green with dashed lines
legend("bottomright", legend=c("Manual ARIMA Model 2", "Auto ARIMA", "ChatGPT ARIMA"), 
       col=c("blue", "red", "green"), lty=c(1, 1, 2), lwd=2)

#End of code
                                                                                                                                     