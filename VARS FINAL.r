#Set CRAN Mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

#Install Required Packages (only run this section once)
if (!require(vars)) install.packages("vars")
if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")

# Load necessary libraries
library(vars)
library(dplyr)
library(ggplot2)

# Step 1: Load your dataset
csv_path <- "/Users/siddhant/Desktop/Case SUMMIT/VAR AND ARIMA CODES/ESS data.csv"  # Update this with your actual file path
economic_data <- read.csv(csv_path)

# Step 2: Function to clean percentage signs and convert to decimals
clean_percentage <- function(x) {
  # Remove any leading or trailing spaces, then remove percentage signs and convert to numeric
  cleaned <- as.numeric(gsub("%", "", trimws(x)))
  return(cleaned / 100)  # Convert to decimal form
}

# Step 3: Apply cleaning function to relevant columns
economic_data$Year <- as.numeric(economic_data$Year)
economic_data$Inflation <- clean_percentage(economic_data$Inflation)
economic_data$GDP_Growth <- clean_percentage(economic_data$GDP_Growth)
economic_data$Unemployment <- clean_percentage(economic_data$Unemployment)

# Step 4: Debugging - Check the structure of your cleaned data
print("Data after cleaning:")
print(str(economic_data))
print(head(economic_data))

# Check for NA values
print("Checking for NA values:")
print(colSums(is.na(economic_data)))

# Remove rows with any NAs
economic_data <- na.omit(economic_data)

# Step 5: Ensure that there are no NAs left
if (any(is.na(economic_data))) {
  stop("Data still contains NA values after cleaning.")
}

# Step 6: Prepare data for the VAR model
var_data <- economic_data %>% select(Inflation, GDP_Growth, Unemployment)

# Check if var_data has any rows after cleaning
if (nrow(var_data) == 0) {
  stop("No valid data points available after cleaning.")
}

# Convert to time series object
var_ts <- ts(var_data, frequency = 1, start = min(economic_data$Year))

# Step 7: Determine optimal lag length for VAR model
lag_selection <- VARselect(var_ts, lag.max = 5, type = "const")
print("Lag selection:")
print(lag_selection$selection)

# Fit the VAR model
optimal_lag <- lag_selection$selection["AIC(n)"]
var_model <- VAR(var_ts, p = optimal_lag, type = "const")
summary(var_model)

# Step 8: Forecast using the VAR model
forecast_horizon <- 5
var_forecast <- predict(var_model, n.ahead = forecast_horizon)

# Convert forecasted values to a data frame for plotting
forecast_df <- data.frame(
  Year = seq(max(economic_data$Year) + 1, by = 1, length.out = forecast_horizon),
  Inflation = var_forecast$fcst$Inflation[, 1],
  GDP_Growth = var_forecast$fcst$GDP_Growth[, 1],
  Unemployment = var_forecast$fcst$Unemployment[, 1]
)

# Step 9: Plot the forecast
ggplot(forecast_df, aes(x = Year)) +
  geom_line(aes(y = Inflation, color = "Inflation")) +
  geom_line(aes(y = GDP_Growth, color = "GDP Growth")) +
  geom_line(aes(y = Unemployment, color = "Unemployment")) +
  labs(title = "VAR Model Forecast", x = "Year", y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("Inflation" = "red", "GDP Growth" = "blue", "Unemployment" = "green"))

# Save the plot (optional)
ggsave("var_forecast_plot.png", width = 8, height = 6)

