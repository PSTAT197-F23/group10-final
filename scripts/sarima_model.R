# Install and load necessary packages
install.packages("forecast")
install.packages("xts")
library(forecast)
library(lubridate)

# Load data
load("data/data_clean.RData")
df <- data_clean

str(df)

# Convert 'date' to Date class and set it as the time index
ts_data <- ts(df$Close, frequency = 1, start = c(year(df$Date[1]), month(df$Date[1])))

# Number of days for the rolling window
window_size <- 6

# Initialize an empty dataframe to store forecasts
forecasts <- data.frame(Date = as.Date(character()), Actual = numeric(), ARIMA = numeric(), SARIMA = numeric())

# Perform rolling window forecast
for (i in (window_size + 1):length(ts_data)) {
  # Extract the current window
  current_window <- ts_data[(i - window_size):(i - 1)]
  
  # ARIMA Model (Non-Seasonal)
  arima_model <- auto.arima(current_window)
  
  # SARIMA Model (Seasonal)
  sarima_model <- auto.arima(current_window, seasonal = TRUE)
  
  # Forecast the next day
  arima_forecast <- forecast(arima_model, h = 1)
  sarima_forecast <- forecast(sarima_model, h = 1)
  
  # Store the results in the forecasts dataframe
  forecasts <- rbind(forecasts, data.frame(Date = time(ts_data)[i], 
                                           Actual = ts_data[i],
                                           ARIMA = arima_forecast$mean[1],  # Extract the first forecast value
                                           SARIMA = sarima_forecast$mean[1]))  # Extract the first forecast value
  }

# Visualize forecasts for ARIMA
plot(ts_data, type = "l", col = "blue", lwd = 2, main = "ARIMA Model Forecast", xlab = "Date", ylab = "Closing Prices")
lines(forecasts$Date, forecasts$Actual, col = "black", lwd = 2, lty = 2, type = "b", pch = 16)
lines(forecasts$Date, forecasts$ARIMA, col = "red", lwd = 2, type = "b", pch = 16)
legend("topright", legend = c("Actual", "ARIMA Forecast"), col = c("black", "red"), lwd = 2, pch = 16)

#Visualize forecasts for SARIMA
plot(ts_data, type = "l", col = "blue", lwd = 2, main = "SARIMA Model Forecast", xlab = "Date", ylab = "Closing Prices")
lines(forecasts$Date, forecasts$Actual, col = "black", lwd = 2, lty = 2, type = "b", pch = 16)
lines(forecasts$Date, forecasts$SARIMA, col = "green", lwd = 2, type = "b", pch = 16)
legend("topright", legend = c("Actual", "SARIMA Forecast"), col = c("black","green"), lwd = 2, pch = 16)






####### OLD MODELS


# # Create lag features
# lags <- 5:10
# for (lag in lags) {
#   for (col in c('Open', 'High', 'Low', 'Close', 'Volume')) {
#     df[paste0(col, '_lag_', lag)] <- lag(df[, col], lag)
#   }
# }

# # Train-Test Split
# train_size <- floor(nrow(df) * 0.8)
# train <- window(df, end = train_size)
# test <- window(df, start = train_size + 1)
# 
# # Build and Train the SARIMA Model
# order <- c(1, 1, 1)  # Replace with appropriate values based on model tuning
# seasonal_order <- c(1, 1, 1, 12)  # Replace with appropriate values based on model tuning
# 
# sarima_model <- Arima(train$Close, order = order, seasonal = seasonal_order)
# 
# # Make Predictions
# sarima_forecast <- forecast(sarima_model, h = nrow(test))
# predicted_mean <- sarima_forecast$mean
# 
# # Evaluate the Model
# rmse <- sqrt(mean((test$Close - predicted_mean)^2))
# cat('Root Mean Squared Error (RMSE):', rmse, '\n')
# 
# # Visualize Results
# plot(train$Close, main = 'S&P 500 Stock Price Prediction with SARIMA')
# lines(test$Close, col = 'blue', lty = 2)
# lines(predicted_mean, col = 'red', lty = 2)
# legend('topright', legend = c('Train', 'Test', 'SARIMA Predictions'), col = c('black', 'blue', 'red'), lty = c(1, 2, 2))

