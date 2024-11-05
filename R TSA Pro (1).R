## step 0: data cleaning, step 1: time plots and decomposition of time series, step 2: stationarity test, step 3: transformation to make stationary, step 4: correlation analysis, 
## step 5: grangers causality test, step 6: Testing using VAR Model, step 7: TVAR model, step 8:Impulse response functions 
## step: ARIMA model and forecasting, step: checking long run relationship using  cointegration


# Installing necessary packages 
install.packages("vars")  
install.packages("tidyr")
install.packages("tsDyn")
install.packages("ggplot2")
install.packages("zoo")
install.packages("forecast")
install.packages("strucchange")


### load the required package
library(readxl)
library(dynlm)
library(ARDL)
library(urca)
library(tseries)
library(zoo)
library(vars)
library(ggplot2)
library(tidyr)
library(tsDyn)
library(forecast)
library(strucchange)

### Loading data in R

data <- read_excel("C:/Users/TarandeepKaur/Downloads/cpi Group data.xlsx", sheet=2)
View(data)

save(dl_CrudeOil, dl_CrudeOil_dubai, file= "newrdata.rda")

getwd()

load(newrdata.rda)

###### Checking for Null values

### Renaming * as NA
data$`Inflation (%)`[data$`Inflation (%)` == "*"] <- NA

# Count total number of NA values in the dataset
total_na <- sum(is.na(data))
total_na

# Convert the column to numeric 
data$`Inflation (%)` <- as.numeric(data$`Inflation (%)`)

## Removing the values for year 2013 as inflation data is not available 
data_clean <- data[data$Year != 2013, ]
View(data_clean)

# Removing other 2 NA values using linear interpolation
data_clean$`Inflation (%)` <- zoo::na.approx(data_clean$`Inflation (%)`)

is.na(data_clean)

## Making the series as time series and ploting

timeseries_oil<- ts(data_clean$`Crude oil, Dubai`, frequency = 12,  start = c(2014, 1), end = c(2024, 8))

plot.ts(timeseries_oil)

# Plot with customized color and title
plot.ts(timeseries_oil, col = "blue", 
        main = "Monthly Crude Oil Prices (Dubai Fateh)", 
        xlab = "Year", ylab = "Price in USD", 
        lwd = 2)

timeseries_inf <- ts(data_clean$`Inflation (%)`, frequency = 12, start = c(2014,1), end = c(2024, 8))

plot.ts(timeseries_inf, col = "pink", 
        main = "Monthly CPI Inflation (India)", 
        xlab = "Year", ylab = "Percentage", 
        lwd = 2)

# Assuming your ts object is named timeseries_oil
df_oil <- data.frame(
  Time = time(timeseries_oil), 
  Value = as.numeric(timeseries_oil)
)

ggplot(df_oil, aes(x = Time, y = Value)) +
  geom_line(color = "blue") +
  labs(title = "Crude Oil Prices Over Time",
       x = "Time",
       y = "Crude Oil Price") +
  theme_minimal()

# Assuming your ts object is named timeseries_inf
df_inf <- data.frame(
  Time = time(timeseries_inf), 
  Value = as.numeric(timeseries_inf)
)


ggplot(df_inf, aes(x = Time, y = Value)) +
  geom_line(color = "blue") +
  labs(title = "Inflation In India Over Time",
       x = "Time",
       y = "Monthly CPI Inflation (%)") +
  theme_minimal()


#### Checking for stationarity

adf.test(data_clean$`Inflation (%)`) #non stationary
adf.test(data_clean$Index) #leave it
adf.test(data_clean$`Crude oil, Dubai`) #stationary
length(data_clean$`Crude oil, Dubai`)

### Applying log differencing transformation 

L_inf <- log(data_clean$`Inflation (%)`)
dL_inf <- diff(L_inf)

adf.test(dL_inf)# Now stationary


l_CrudeOil <- log(data_clean$`Crude oil, average`)
dl_CrudeOil <- diff(l_CrudeOil)

adf.test(dl_CrudeOil)


l_CrudeOil_dubai<- log(data_clean$`Crude oil, Dubai`)
dl_CrudeOil_dubai<- diff(l_CrudeOil_dubai)

adf.test(dl_CrudeOil_dubai)


#final series dL_inf, dl_CrudeOil_dubai, dl_CrudeOil

ts_inf<- ts(dL_inf, frequency = 12, start = c(2014,1), end = c(2024,8))
ts_oil<- ts(dl_CrudeOil, frequency=12, start = c(2014,1), end = c(2024,8))
ts_oil_dubai<- ts(dl_CrudeOil_dubai, frequency=12, start = c(2014,1), end = c(2024,8))

decompose_inf <- decompose(ts_inf)
decompose_oil<- decompose(ts_oil)
decompose_oil_dubai<- decompose(ts_oil_dubai)

plot(decompose_inf)
# Plot with enhanced title and colors for components
plot(decompose_inf, 
     col = "orange", lwd = 1.5)


plot(decompose_oil)
plot(decompose_oil_dubai, col = "orange", lwd = 1.5)


par(mfrow = c(1, 2))

# Extracting the components from the decompose object
decomposed_data <- data.frame(
  time = time(decompose_inf$seasonal),
  observed = decompose_inf$x,
  trend = decompose_inf$trend,
  seasonal = decompose_inf$seasonal,
  random = decompose_inf$random
)


# Gather the data for easier plotting
decomposed_long <- gather(decomposed_data, key = "component", value = "value", -time)

# Plotting the decomposed components
ggplot(decomposed_long, aes(x = time, y = value, color = component)) +
  geom_line(size = 0.7) +
  facet_wrap(~component, scales = "free_y", ncol = 1) +
  labs(title = "Decomposition of Inflation Series", x = "Time", y = "Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("observed" = "black", "trend" = "blue", 
                                "seasonal" = "red", "random" = "green"))


# Assuming oil_prices and inflation are your stationary time series

# Calculate the correlation
correlation <- cor(dL_inf, dl_CrudeOil_dubai) ## 0.11
correlation_orig <- cor(timeseries_inf, timeseries_oil) ## 0.38

length(dL_inf)  # Length of inflation data
length(dl_CrudeOil_dubai)  # Length of crude oil data


correlation2<- cor(dL_inf, dl_CrudeOil)
cat("Correlation between Crude Oil Prices and Inflation: ", correlation, "\n")
cat("Correlation between Average Crude Oil Prices and Inflation: ", correlation2, "\n")


# Plot the time series to visualize the relationship
df <- data.frame(
  Date = seq(as.Date("2014-01-01"), by = "month", length.out = length(dL_inf)),
  Oil_Prices = dl_CrudeOil_dubai,
  Inflation = dL_inf
)
View(df)

# Plot the series
ggplot(df, aes(x = Date)) +
  geom_line(aes(y = Oil_Prices, color = "Crude Oil Prices")) +
  geom_line(aes(y = Inflation, color = "Inflation")) +
  labs(title = "Crude Oil Prices and Inflation Over Time",
       x = "Time",
       y = "Log Differenced Values") +
  scale_color_manual("", 
                     breaks = c("Crude Oil Prices", "Inflation"),
                     values = c("blue", "red")) +
  theme_minimal()


# Assuming your stationary series are 'oil_prices' and 'inflation'
# Combine the series into a data frame or time series object
DataFrame <- cbind(dl_CrudeOil_dubai, dL_inf)

# Determine the optimal lag length for the VAR model
lag_selection <- VARselect(DataFrame, lag.max = 10, type = "const")
print(lag_selection$selection)  # Check AIC, BIC, HQIC to select the best lag

# Fit the VAR model using the selected lag length (use the lag with the lowest criterion value)
var_model <- VAR(DataFrame, p = 2, type = "const")

# Perform the Granger Causality Test
granger_test_oil_to_inflation <- causality(var_model, cause = "dl_CrudeOil_dubai")
granger_test_inflation_to_oil <- causality(var_model, cause = "dL_inf")

# Display the results
cat("Granger Causality Test: Does Oil Prices Granger-cause Inflation?\n")
print(granger_test_oil_to_inflation$Granger)

cat("\nGranger Causality Test: Does Inflation Granger-cause Oil Prices?\n")
print(granger_test_inflation_to_oil$Granger)

### Grangers Causality Test tells no causality. This test checks for linear causal effects.

### Checking causality through non-linear methods.
## Threshold VAR

# Assuming 'data' contains your stationary time series (e.g., oil_prices and inflation)
# Set a threshold on oil_prices or inflation for example

# Apply a TVAR model
tvar_model <- TVAR(DataFrame, lag=2, nthresh=1, thDelay=1)

# Summary of the model
summary(tvar_model)


# Calculate IRFs
irf_tvar <- irf(tvar_model, impulse="dl_CrudeOil_dubai", response="dL_inf", n.ahead=30, ortho=FALSE, boot=TRUE, runs=100)

# This computes the IRFs up to 10 steps ahead.
# 'boot=TRUE' enables bootstrapping to generate confidence intervals, and 'runs=100' specifies the number of bootstrap samples.

# Plot the IRFs
plot(irf_tvar, main="Impulse Response of Inflation to Crude Oil Shocks")

# Customize plot with labels and colors
plot(irf_tvar, main="Impulse Response of Inflation to Crude Oil Price Shocks",
     col=c("blue", "red"), ylab="Response of Inflation", xlab="Time Horizons (Months)", lty=1)


# Perform variance decomposition
# Here, 'n.ahead' refers to the number of periods ahead for which we want the variance decomposition
var_decomp <- fevd(var_model, n.ahead = 10)

# View the results for variance decomposition
print(var_decomp)

# You can also look at specific variables' decompositions
# For example, for the first variable:
print(var_decomp$dl_CrudeOil_dubai)

# For the second variable:
print(var_decomp$dL_inf)

# To plot the variance decomposition for visual understanding
plot(var_decomp)



##Chow test is for linear models only 
# Chow test at time point (e.g., 2019, assuming the time series is monthly)
break_point <- 2020

# Example linear model using data from one regime of the TVAR
lin_model <- lm(dL_inf ~ dl_CrudeOil_dubai)

# Run Chow test at a suspected breakpoint
chow_test <- sctest(lin_model, point = break_point)
summary(chow_test)

stat_value <- chow_test$statistic
p_value <- chow_test$p.value

if (p_value < 0.05) {
  print(paste("Structural break detected with test statistic:", stat_value, "and p-value:", p_value))
} else {
  print(paste("No significant structural break with test statistic:", stat_value, "and p-value:", p_value))
}

# Automatically detect breakpoints
bp_model <- breakpoints(lin_model)

# View summary
summary(bp_model)

# Plot breakpoints
plot(bp_model)

# You can add the breakpoints to your time series plot
plot.ts(your_time_series, main="Detected Breakpoints in Time Series")
lines(bp_model)

# Automatically detect breakpoints in the relationship between inflation and crude oil prices
bp_model <- breakpoints(dL_inf ~ dl_CrudeOil_dubai)

# View the summary to check the breakpoints
summary(bp_model)

#### Low BIC is preferred, high BIC> complex model
#### Low RSS is preferred, but a very low RSS can mean a complex model

# Plot the breakpoints
plot(bp_model)

# Add breakpoints to a time series plot for visual reference
plot.ts(timeseries_inf, main="Detected Breakpoints in Time Series")
abline(bp_model)


###### ARIMA MODEL

arima_model <- auto.arima(timeseries_oil)
summary(arima_model)

forecast_values <- forecast(arima_model, h = 12)   # Forecast for next 12 months
plot(forecast_values, main = "Oil price forecast using ARIMA model", xlab = "Year", ylab = "Oil Prices (USD)", col = "blue")


arima_model2 <- auto.arima(timeseries_inf)
summary(arima_model2)

forecast_values2 <- forecast(arima_model2, h = 12)   # Forecast for next 12 months
plot(forecast_values2, main= "Inflation Forecast using ARIMA model", xlab = "Year", ylab = "Inflation (%)", col = "blue")


install.packages("urca")   # If not already installed
library(urca)


coint_data <- cbind(timeseries_inf, timeseries_oil)

# Johansen Test for Cointegration
johansen_test <- ca.jo(coint_data, type = "trace", ecdet = "const", K = 2)

# Summary of the test
summary(johansen_test)


#### Evidence is found for atleast one cointegrating relationship
###### LONG RUN EQUILIBRIUM RELATIONSHIP EXISTS
###### THE EQUILIBRIUM RELATIONSHIP GETS DISTURBED/DEVIATED IN SHORT RUN.and inflation adjusts more significantly than crude oil prices to restore balance (LARGER NEGATIVE LOADING)

##The cointegration equation can be approximated as:
###  Inflation = 0.0088 × Crude Oil +constant


### ERROR CORRECTION MODEL

# Get the cointegration equation
coint_eqn <- cajorls(johansen_test, r = 1)$rlm

# Extract the residuals (Error Correction Term)
ECT <- residuals(coint_eqn)

diff_inf <- diff(timeseries_inf)  # Differenced inflation series
diff_oil <- diff(timeseries_oil)

# Create the ECM
ECM_model <- lm(diff_inf ~ diff_oil + trimmed_ECT)

length(timeseries_inf)
length(timeseries_oil)
length(ECT[-1])

# Trim the ECT to match the length of differenced variables
trimmed_ECT <- ECT[(length(ECT) - length(diff_inf) + 1):length(ECT)]
length(trimmed_ECT)

# Summary of the ECM
summary(ECM_model)

#### no variable is statistically significant, no short run relationship. 