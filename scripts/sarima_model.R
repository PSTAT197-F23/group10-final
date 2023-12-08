# Install and load necessary packages
install.packages("forecast")
install.packages("xts")
library(xts)
library(forecast)

# Load data
load("data/data_clean.RData")
# Replace 'your_data.csv' with the actual file path or URL of your data
df <- data_clean
df$Date <- as.Date(df$Date)
df <- xts(df[, c('Open', 'High', 'Low', 'Close', 'Volume')], order.by = df$Date)

# Explore data
plot(df$Close, main = 'S&P 500 Stock Prices Over Time')

# Data Preprocessing
# Handle missing values
df <- na.omit(df)

# Create lag features
lags <- 5:10
for (lag in lags) {
  for (col in c('Open', 'High', 'Low', 'Close', 'Volume')) {
    df[paste0(col, '_lag_', lag)] <- lag(df[, col], lag)
  }
}

# Train-Test Split
train_size <- floor(nrow(df) * 0.8)
train <- window(df, end = train_size)
test <- window(df, start = train_size + 1)

# Build and Train the SARIMA Model
order <- c(1, 1, 1)  # Replace with appropriate values based on model tuning
seasonal_order <- c(1, 1, 1, 12)  # Replace with appropriate values based on model tuning

sarima_model <- Arima(train$Close, order = order, seasonal = seasonal_order)

# Make Predictions
sarima_forecast <- forecast(sarima_model, h = nrow(test))
predicted_mean <- sarima_forecast$mean

# Evaluate the Model
rmse <- sqrt(mean((test$Close - predicted_mean)^2))
cat('Root Mean Squared Error (RMSE):', rmse, '\n')

# Visualize Results
plot(train$Close, main = 'S&P 500 Stock Price Prediction with SARIMA')
lines(test$Close, col = 'blue', lty = 2)
lines(predicted_mean, col = 'red', lty = 2)
legend('topright', legend = c('Train', 'Test', 'SARIMA Predictions'), col = c('black', 'blue', 'red'), lty = c(1, 2, 2))

