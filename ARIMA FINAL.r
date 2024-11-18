# Step 1: Set CRAN Mirror (only once if needed)
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Step 2: Install Packages (only if not already installed)
if (!require(forecast)) install.packages("forecast")
if (!require(tseries)) install.packages("tseries")
if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")

# Load necessary libraries
library(forecast)
library(tseries)
library(dplyr)
library(ggplot2)

# Step 1: Load your dataset
csv_path <- "/Users/siddhant/Desktop/Case SUMMIT/VAR AND ARIMA CODES/ESS data.csv"  # Update this with your actual file path
economic_data <- read.csv(csv_path)

# Step 2: Function to clean percentage signs and convert to decimals
clean_percentage <- function(x) {
  cleaned <- as.numeric(gsub("%", "", trimws(x)))
  return(cleaned / 100)  # Convert to decimal form
}

# Step 3: Apply the cleaning function to relevant columns
# Check if columns exist before trying to clean them
if ("Year" %in% colnames(economic_data)) {
  economic_data$Year <- as.numeric(economic_data$Year)
} else {
  stop("Year column is missing.")
}

if ("Inflation" %in% colnames(economic_data)) {
  economic_data$Inflation <- clean_percentage(economic_data$Inflation)
} else {
  stop("Inflation column is missing.")
}

if ("GDP_Growth" %in% colnames(economic_data)) {
  economic_data$GDP_Growth <- clean_percentage(economic_data$GDP_Growth)
} else {
  stop("GDP_Growth column is missing.")
}

if ("Unemployment" %in% colnames(economic_data)) {
  economic_data$Unemployment <- clean_percentage(economic_data$Unemployment)
} else {
  stop("Unemployment column is missing.")
}

# Step 4: Debugging - Check the structure and content of cleaned data
print("Data after cleaning:")
print(str(economic_data))
print(head(economic_data))

# Check for NA values after cleaning
print("Checking for NA values:")
print(colSums(is.na(economic_data)))

# Step 5: Remove rows with any NAs
economic_data <- na.omit(economic_data)

# Confirm that there are no NAs left
if (any(is.na(economic_data))) {
  stop("Data still contains NA values after cleaning.")
}

# Step 6: Ensure that the 'Year' column is not empty
if (length(economic_data$Year) == 0) {
  stop("Year column is empty after cleaning.")
}

# Step 7: Prepare data for ARIMA model
# Use the Inflation column as an example for ARIMA
if (nrow(economic_data) > 0) {
  inflation_ts <- ts(economic_data$Inflation, start = min(economic_data$Year), frequency = 1)
} else {
  stop("No data available for ARIMA model.")
}

# Step 8: Check for stationarity and apply differencing if necessary
adf_test <- adf.test(inflation_ts)
print(adf_test)

# Apply differencing if the series is not stationary
if (adf_test$p.value > 0.05) {
  inflation_diff <- diff(inflation_ts, differences = 1)
} else {
  inflation_diff <- inflation_ts
}

# Step 9: Fit ARIMA Model
arima_model <- auto.arima(inflation_diff)
summary(arima_model)

# Step 10: Forecast Future Values Using ARIMA
forecast_period <- 5  # Forecasting next 5 periods (e.g., years)
inflation_forecast <- forecast(arima_model, h = forecast_period)
print(inflation_forecast)

# Step 11: Plot the Forecast
plot(inflation_forecast, main = "Inflation Rate Forecast", xlab = "Year", ylab = "Inflation Rate (%)")

# Step 12: Perform Residual Analysis
checkresiduals(arima_model)

# Step 13: Save Forecasted Results to CSV (Optional)
write.csv(inflation_forecast, "/Users/siddhant/Desktop/inflation_forecast.csv", row.names = FALSE)

# Step: Convert the forecast to a data frame
forecast_df <- data.frame(
  Year = seq(max(economic_data$Year) + 1, by = 1, length.out = length(inflation_forecast$mean)),
  Inflation_Forecast = inflation_forecast$mean
)

# Combine historical data and forecasted data for plotting
historical_df <- data.frame(
  Year = economic_data$Year,
  Inflation = economic_data$Inflation
)

combined_df <- rbind(
  data.frame(Year = historical_df$Year, Inflation = historical_df$Inflation, Type = "Historical"),
  data.frame(Year = forecast_df$Year, Inflation = forecast_df$Inflation_Forecast, Type = "Forecast")
)

# Step: Plot the data using ggplot2
library(ggplot2)

ggplot(combined_df, aes(x = Year, y = Inflation, color = Type)) +
  geom_line(size = 1.2) +
  labs(
    title = "Inflation Forecast Using ARIMA",
    x = "Year",
    y = "Inflation Rate (%)"
  ) +
  scale_color_manual(values = c("Historical" = "blue", "Forecast" = "red")) +
  theme_minimal()

# Optional: Save the plot as an image file
ggsave("inflation_forecast_plot.png", width = 8, height = 6)

summary(arima_model)


