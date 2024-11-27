# Gross Ddomestic Product (GDP) Y = Personal
# Consumption Expenditures (PCEC) C + Gross
# Private Domestic Investment (GDPI) I +
# Gov. Consumption Expenditures and
# Gross Investment (GCE) G + Net Exports of
# Goods and Services (NETEXP) NX

library(quantmod)
library(ggplot2)
library(vars)
library(forecast)
# specially designed background for ggplot(s)
theme_set(
  theme_minimal() +
    theme(panel.grid.major = element_line(color = "gray88", size = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          line = element_blank())
)

# Growth models
getSymbols(c("UNRATE", "CPIAUCSL"), src = "FRED")

# merging unemployment and cpi

unemp_cpi <- na.omit(merge(UNRATE, 100 * Delt(CPIAUCSL, k = 12)))
                     
colnames(unemp_cpi) <- c("UNEMP", "CPI")
head(unemp_cpi)

plot(as.numeric(unemp_cpi[,1]), as.numeric(unemp_cpi[,2]), xlab = "Unemployment Rate", ylab = "CPI YoY")

sixties <- unemp_cpi[grep("^196", index(unemp_cpi)),]
head(sixties)
# graph 1
ggplot(sixties, aes(UNEMP, CPI)) + geom_point() + xlab("Unemployment") + ylab("CPI YoY")

# graph 2
ggplot(unemp_cpi, aes(UNEMP, CPI)) + geom_point() + xlab("Unemployment") + ylab("CPI YoY")

# as far as the 70's the ST relationship with unemployment and inflation broke down the Phillips curve hypothesis.
# as we can see in graph 2 vs graph 1.
# later the model was extended to include expectations.
# workers and firms bargain over wages as they are concerned w/ the real value of the wage.
# contracts about their expectations about future inflation are set.
# eg. Inflation Expectation Augmented Phillips Curve.
# assuming real wages are constant: actual price inflation is the same as wage inflation.

# experimenting if the augmented PC fits the date assuming inflation expectations

unemp_cpi_annual <- unemp_cpi[grep("-12-01$", index(unemp_cpi)), ]
head(unemp_cpi_annual)

library(knitr)
mdl <- lm(diff(unemp_cpi_annual[,2]) ~ unemp_cpi_annual[,1])
head(mdl)
summary(mdl)               

# turning the output into a table
kable(summary(mdl)$unemp_cpi_annual[,1], format = "html")

ggplot(data = unemp_cpi_annual, aes(x = unemp_cpi_annual[,1], y = diff(unemp_cpi_annual[,2]))) +
  geom_point() +                  # Add scatter plot points
  geom_smooth(method = "lm") +    # Add linear regression line
  labs(x = "Unemployment Rate", y = "Change in Inflation Rate YOY", title = "")

# Okun's law

getSymbols(c("GDPC1", "UNRATE"), src = "FRED") # US Real GDP per Capita
gdp_unrate <- merge(GDPC1, UNRATE)
gdp_unrate <- na.omit(gdp_unrate)
gdp_unrate[,1] <- 100 * Delt(gdp_unrate[, 1], k = 4)
gdp_unrate[, 2] <- diff(gdp_unrate[, 2], lag = 4)

gdp_unrate <- na.omit(gdp_unrate)
# regression

mdl2 <- lm(gdp_unrate[, 1] ~ gdp_unrate[, 2])
summary(mdl2)
ggplot(data = gdp_unrate, aes(x = gdp_unrate[,1], y = diff(gdp_unrate[,2]))) +
  geom_point() +                  # Add scatter plot points
  geom_smooth(method = "lm") +    # Add linear regression line
  labs(x = "Change in Unemployment Rate", y = "Change in Real GDP Growth", title = "")
summary(mdl2)

# 0.41/1.6 = 0.25. When output exceeds the natural rate of output by 4%, inflation increases by 1% holding inflationary expectations constant.

# Vector Autoregression (VAR)

# We will describe the behavior of a set of TS data through VAR Models.

# Sims (1982) and the author's explaining the fucking theory. 

# Stock and Watson VAR (4) simulating the response function w/ the CPI



#US

getSymbols(c("GDPCTPI", "UNRATE", "FEDFUNDS"), src = "FRED")



## Long term forecasts HERE

macro_datalt <- merge(GDPCTPI, UNRATE, FEDFUNDS)
macro_datalt <- na.omit(macro_datalt)
macro_datalt[, 1] <- 400 * Delt(macro_datalt[, 1], type = "log")
macro_datalt <- na.omit(macro_datalt)
macro_datalt <- macro_datalt["1960-01-01" <= index(macro_datalt) &
                               index(macro_datalt) < "2023-06-30", ]
# head(macro_data) tail(macro_data)
# VARselect(macro_data)
# impulse response function
# shocks through the system
#variable 1
#impacts variables 2 and 3 at time t, variable 2 impacts only variable 3 at time t, variable 3 does not
#impact variable 1 and variable 2 at time t##
mdl_lt <- VAR(macro_datalt, p = 4)
summary(mdl_lt)

# Inflation equation
# Given the Endogenous Variables: GDPCTPI, UNRATE, FEDFUNDS
# The Lagged values of GDPCTPI, UNRATE, and FEDFUNDS are used to predict GDPCTPI. 
#It shows a positive and statistical significant coefficients for GDPCTPI.I1, , GDPCTPI.l3 indicate a positive relationship with the current GDPCTPI.
# The model captures a lagged effect indicating the persistance of GDPCTPI (inflation)

#UNRATE Equation:
#Lagged values of GDPCTPI, UNRATE, and FEDFUNDS are used to predict UNRATE.
#UNRATE.l1 has a strong positive relationship with the current UNRATE, as indicated by a high coefficient and statistical significance.
#Lagged values of GDPCTPI and FEDFUNDS also contribute to predicting UNRATE.

#FEDFUNDS Equation:
#Lagged values of GDPCTPI, UNRATE, and FEDFUNDS are used to predict FEDFUNDS (interest rate).
#FEDFUNDS.l1 has a strong positive relationship with the current FEDFUNDS, and it is statistically significant.
#Lagged values of UNRATE and GDPCTPI also contribute to predicting FEDFUNDS.

# Overall, the model (VAR) suggests a good fit to the data, capturing the relationships among the variables. 

# Inflation Shocks

cpi_irflt <- irf(mdl_lt, impulse = "GDPCTPI", ortho = F,
               n.ahead = 24, ci = 0.66, runs = 300)
par(mfrow = c(1, 3))
plot(cpi_irflt$irf$GDPCTPI[, 1], typ = "l", ylab = "Inflation Shock to Inflation")
lines(cpi_irflt$Lower$GDPCTPI[, 1], lty = 2)
lines(cpi_irflt$Upper$GDPCTPI[, 1], lty = 2)
abline(h = 0)

plot(cpi_irflt$irf$GDPCTPI[, 2], typ = "l", ylab = "Inflation Shock to Unemployment",
     ylim = c(-0.1, 0.3))
lines(cpi_irflt$Lower$GDPCTPI[, 2], lty = 2)
lines(cpi_irflt$Upper$GDPCTPI[, 2], lty = 2)
abline(h = 0)

plot(cpi_irflt$irf$GDPCTPI[, 3], typ = "l", ylab = "Inflatiton Shock to Federal Funds Rate",
     ylim = c(0, 0.75))
lines(cpi_irflt$Lower$GDPCTPI[, 3], lty = 2)
lines(cpi_irflt$Upper$GDPCTPI[, 3], lty = 2)
abline(h = 0)

## 
# one unit shock to inflation slowly decreases over 24-quarters
# Inflation proceeds to fall while unemployment increases
# the increase in inflation corresponds in the increase of funds rate

##
# Unemployment shocks

cpi_irfunem <- irf(mdl_lt, impulse = "UNRATE", ortho = F,
                 n.ahead = 24, ci = 0.66, runs = 300)
par(mfrow = c(1, 3))
plot(cpi_irfunem$irf$UNRATE[, 1], typ = "l", ylab = "Unemployment Shock to Inflation",
     ylim = c(-0.5,0.5))
lines(cpi_irfunem$Lower$UNRATE[, 1], lty = 2)
lines(cpi_irfunem$Upper$UNRATE[, 1], lty = 2)
abline(h = 0)

plot(cpi_irfunem$irf$UNRATE[, 2], typ = "l", ylab = "Unemployment Shock to Unemployment")
lines(cpi_irfunem$Lower$UNRATE[, 2], lty = 2)
lines(cpi_irfunem$Upper$UNRATE[, 2], lty = 2)
abline(h = 0)

plot(cpi_irfunem$irf$UNRATE[, 3], typ = "l", ylab = "Unemployment Shock to Federal Funds Rate",
     ylim = c(-1, 1))
lines(cpi_irfunem$Lower$UNRATE[, 3], lty = 2)
lines(cpi_irfunem$Upper$UNRATE[, 3], lty = 2)
abline(h = 0)

# IR Shocks

cpi_ir <- irf(mdl_lt, impulse = "FEDFUNDS", ortho = F,
              n.ahead = 24, ci = 0.66, runs = 300)
par(mfrow = c(1, 3))
plot(cpi_ir$irf$FEDFUNDS[, 1], typ = "l", ylab = "Federal Funds Rate Shock to Inflation",
     ylim = c(-0.5,0.5))
lines(cpi_ir$Lower$FEDFUNDS[, 1], lty = 2)
lines(cpi_ir$Upper$FEDFUNDS[, 1], lty = 2)
abline(h = 0)

plot(cpi_ir$irf$FEDFUNDS[, 2], typ = "l", ylab = "Federal Funds Rate Shock to Unemployment",
     ylim = c(-0.2, 0.2))
lines(cpi_ir$Lower$FEDFUNDS[, 2], lty = 2)
lines(cpi_ir$Upper$FEDFUNDS[, 2], lty = 2)
abline(h = 0)

plot(cpi_ir$irf$FEDFUNDS[, 3], typ = "l", ylab = "Federal Funds Rate Shock to Federal Funds Rate",
     ylim = c(-1, 1))
lines(cpi_ir$Lower$FEDFUNDS[, 3], lty = 2)
lines(cpi_ir$Upper$FEDFUNDS[, 3], lty = 2)
abline(h = 0)

# An increase inflation corresponds to
# an increase in unemployment
# moreover, a reduction in inflation
# consistent w the simple IS-LM framework

# forecasts using VAR

macro_data <- merge(GDPCTPI, UNRATE, FEDFUNDS)
macro_data <- na.omit(macro_data)
macro_data[, 1] <- 400 * Delt(macro_data[, 1],
                              type = "log")
macro_data <- na.omit(macro_data)
mdl2 <- VAR(macro_data, p = 4)
foreacst_mdl <- predict(mdl2, n.ahead = 12)
plot(foreacst_mdl)
foreacst_mdl
str(foreacst_mdl)
str(mdl2)

# Our model forecasts that ovr the next three years
# inflation, unemployment, and fund rates are expected to increase

# forecast accuracy tests

actual_values <- macro_data
summary(macro_data)

# inflation
forecast_values <- foreacst_mdl$fcst$GDPCTPI[, 1]  # Adjust column index as needed
mae <- mean(abs(actual_values - forecast_values)) # mean absolute error
mae
# the forecast for the MAE is equal to 2.46 which means that is a good forecast
#  given the average magnitude of errors between predicted and actual values.
mse <- mean((actual_values - forecast_values)^2) # mean squared error
rmse <- sqrt(mse) # root mean squared
# the large mse and rmse shows that the model isn't optimized very well.
mape <- mean(abs((actual_values - forecast_values) / actual_values)) * 100 # mean absolute percentage error
# the large MAPE shows an extremely large value showing the forecasts aren't very reliable
# Unemployment
forecast_values2 <- foreacst_mdl$fcst$UNRATE[, 2]  # Adjust column index as needed

mae2 <- mean(abs(actual_values - forecast_values2))
mse2 <- mean((actual_values - forecast_values2)^2)
rmse2 <- sqrt(mse2)
mape2 <- mean(abs((actual_values - forecast_values2) / actual_values)) * 100
# same patterns only the mse passes the test
# Fed Funds
forecast_values3 <- foreacst_mdl$fcst$FEDFUNDS[, 3]  # Adjust column index as needed

mae3 <- mean(abs(actual_values - forecast_values3))
mse3 <- mean((actual_values - forecast_values3)^2)
rmse3 <- sqrt(mse3)
mape3 <- mean(abs((actual_values - forecast_values3) / actual_values)) * 100
# it fails in every category


# forecast interval coverage inflation
interval_coverage <- mean((actual_values >= foreacst_mdl$fcst$GDPCTPI[, 2]) &
                            (actual_values <= foreacst_mdl$fcst$GDPCTPI[, 3]))

# forecast interval coverage unemployement
interval_coverage2 <- mean((actual_values >= foreacst_mdl$fcst$UNRATE[, 2]) &
                            (actual_values <= foreacst_mdl$fcst$UNRATE[, 3]))

# forecast interval coverage Fed Funds
interval_coverage3 <- mean((actual_values >= foreacst_mdl$fcst$FEDFUNDS[, 2]) &
                             (actual_values <= foreacst_mdl$fcst$FEDFUNDS[, 3]))

# the interval coverage shows that interest rates forecasts the best (0.84).
# Next, inflation (0.74) and lastly, unemployment (0.60)

# Augmented Dickey Fuller Test

#inflation
plot(macro_data[, 1], type = "l", main = "Inflation Time Series")
library(urca)
adf_test_result <- ur.df(macro_data[, 1], type = "drift", lags = 4)
summary(adf_test_result)
# there is a stationarity on zlag 1, diff lag 1, and diff lag 2
# rejecting the null hypothesis of a unit root

# unemployment
plot(macro_data[, 2], type = "l", main = "Unemployment Time Series")
adf_test_result2 <- ur.df(macro_data[, 2], type = "drift", lags = 4)
summary(adf_test_result2)
# There is a stationarity o z lag 1 and z diff lag 1

# Fed funds
plot(macro_data[, 3], type = "l", main = "Fed Funds Time Series")
adf_test_result3 <- ur.df(macro_data[, 3], type = "drift", lags = 4)
summary(adf_test_result3)
# stationarity in z lag 1, diff lag 1, and diff lag 3

# first differences
# difference of the series
#inflation
diff_data <- diff(macro_data[, 1])
plot(diff_data, type = "l", main = "Differenced Inflation Time Series")
# clearly stationary

# unemployment
diff_data2 <- diff(macro_data[, 2])
plot(diff_data2, type = "l", main = "Differenced Unemployment Time Series")
# big spike

# fed funds
diff_data3 <- diff(macro_data[, 3])
plot(diff_data3, type = "l", main = "Differenced Fed Funds Time Series")
# overall stationary with a big dip

# Autocorrelation and Partial Autocorrelation Functions

# inflation
par(mfrow = c(2, 1))
acf(macro_data[, 1], lag.max = 20, main = "ACF")
pacf(macro_data[, 1], lag.max = 20, main = "PACF")

# unem
par(mfrow = c(2, 1))
acf(macro_data[, 2], lag.max = 20, main = "ACF")
pacf(macro_data[, 2], lag.max = 20, main = "PACF")

# fed funds
acf(macro_data[, 3], lag.max = 20, main = "ACF")
pacf(macro_data[, 3], lag.max = 20, main = "PACF")

# creating a matrix for for the ACF and PACF

par(mfrow = c(2, 3))

# Loop through each variable
for (i in 1:3) {
  # ACF
  acf(macro_data[, i], lag.max = 20, main = paste("ACF - Variable", i))
  
  # PACF
  pacf(macro_data[, i], lag.max = 20, main = paste("PACF - Variable", i))
}

# SARIMA & Diebold and Mariano test (forecast accuracy)

# Assuming macro_datalt is a time series object
ts_macro_datalt <- ts(macro_datalt[, 1], frequency = 4)  # Assuming quarterly data

# Fit SARIMA model using auto.arima
sarima_model <- auto.arima(ts_macro_datalt)

# Generate SARIMA forecasts
forecast_sarima <- forecast(sarima_model, h = 12)  # Forecasting 12 periods ahead
summary(forecast_sarima)
# Fit VAR model
var_model <- VAR(macro_datalt)
forecast_var <- predict(var_model, n.ahead = 12)
str(forecast_var)
# Extract actual values
actual_values <- ts_macro_datalt[(length(ts_macro_datalt) - 11):length(ts_macro_datalt)]

# Extract forecast errors
forecast_errors_sarima <- actual_values - as.vector(forecast_sarima$mean)


# Extract forecast values for each variable from forecast_var$fcst
forecast_values_var <- lapply(forecast_var$fcst, function(x) as.vector(x[, "fcst"]))

# Combine the forecast values into a matrix
forecast_values_var <- do.call(cbind, forecast_values_var)

# Extract the first column (you can change the index as needed)
forecast_errors_var <- actual_values - forecast_values_var[, 1]
forecast_errors_var2 <- actual_values - forecast_values_var[, 2]
forecast_errors_var3 <- actual_values - forecast_values_var[, 3]

# Perform Diebold-Mariano Test
dm_test_result <- dm.test(forecast_errors_sarima, forecast_errors_var, alternative = "two.sided")
dm_test_result2 <- dm.test(forecast_errors_sarima, forecast_errors_var2, alternative = "two.sided")
dm_test_result3 <- dm.test(forecast_errors_sarima, forecast_errors_var3, alternative = "two.sided")
# Display the test results
print(dm_test_result) # inflation
print(dm_test_result2) # unemployment
print(dm_test_result3) # Fed Funds
#inflation
#DM:This is the test statistic for the Diebold-Mariano test. 
#It measures the difference in forecast accuracy between the two models. 
#A negative value suggests that the first model (presumably SARIMA) performed worse than the second model.

#p-value = 0.08581: This is the p-value associated with the test statistic. 
#The p-value is used to assess the statistical significance of the DM test. 
#In this case, the p-value is 0.08581, which is greater than the typical significance level of 0.05. 
#Therefore, you might fail to reject the null hypothesis that there is no difference in forecast accuracy between the two models.

#based on the p-value being greater than 0.05, 
#you might not have enough evidence to conclude that there 
#is a statistically significant difference in forecast accuracy between the two models 
#at the 1-period forecast horizon.