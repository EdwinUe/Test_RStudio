install.packages("readxl")
install.packages('xts')
install.packages("ggfortify")
install.packages("PerformanceAnalytics")

library(xts)
library(forecast)
library(tseries)
library(ggfortify)
library(gridExtra)
library(PerformanceAnalytics)



#------------------------------------------------------------------------------
# Step 1: Read, Format, Convert Data
#------------------------------------------------------------------------------


# Read Data
DF.TS <- read.csv(file.choose())
head(DF.TS)


# Format Data
closing_price <- gsub(".","",DF.TS$Zuletzt, fixed = TRUE)
closing_price <- gsub(",",".",closing_price, fixed = TRUE)
DF.TS <- data.frame(DF.TS$Datum, closing_price)
head(DF.TS)


# Convert Time Series into XTS with ascending order:
date_format <- as.character(DF.TS$DF.TS.Datum)
date_format <- paste(substring(date_format,7,10),
                     substring(date_format,4,5),
                     substring(date_format,1,2),sep = '-')
date_format <- as.Date(date_format)
DF.TS <- data.frame(date_format, closing_price)
DF.TS.xts <- xts(DF.TS, order.by = date_format)
DF.TS.xts$date_format <- NULL
DF.TS.xts  <- apply.weekly(DF.TS.xts$closing_price, mean)
head(DF.TS.xts)


#------------------------------------------------------------------------------
# Step 2: Setting the time intervals 
#------------------------------------------------------------------------------


# Enter of the time intervals (training-/test set):
training_set <- "2020-05-15/2022-05-13"
test_set <- "2022-05-20/2022-06-22"

# Definition and plot of the sets:
DF.TS.training <- DF.TS.xts[training_set, 'closing_price']
DF.TS.test <- DF.TS.xts[test_set, 'closing_price']

plot.zoo(cbind(DF.TS.training,
               DF.TS.test),
         plot.type = "single", 
         col = c("black", "gray"), 
         main = 'Time Series Development', 
         ylab = '', xlab = '')
legend('topleft', 
       legend = c(training_set,
                  test_set), 
       col = c("black", "gray"),
       lty = 1, cex = 0.65)


#------------------------------------------------------------------------------
# Step 3: Box-Cox-Transformation
#------------------------------------------------------------------------------


# Creation Lambda and transformed time series by Box-Cox:
lambda <- BoxCox.lambda(DF.TS.training)
lambda
DF.TS.training.BC <- BoxCox(DF.TS.training,lambda)


# Creation QQ-plot
op <- par(pty = "s", mfrow = c(1, 2))
qqnorm(DF.TS.training$closing_price,main = 'Q-Q Plot for Normality')
qqline(DF.TS.training$closing_price, lwd = 2, lty = 2)
qqnorm(DF.TS.training.BC$closing_price, main = 'BC Q-Q Plot for Normality',col = 'steelblue')
qqline(DF.TS.training.BC$closing_price, , col = 'red', lwd = 2, lty = 2)
par(op)


# Plot test set and Box-Cox time series
plot.zoo(cbind(DF.TS.training,
               DF.TS.training.BC),
         col = c("black", "gray"), 
         main = 'Original vs. Box-Cox Transformed Time Series', 
         ylab = '', xlab = '')


# Choose between DF.TS.training and DF.TS.training.BC:
DF.TS.select <- DF.TS.training.BC


#------------------------------------------------------------------------------
# Step 4: Execution KPSS-Test and differentiation
#------------------------------------------------------------------------------


# Stationarity test
kpss.test(DF.TS.select)


# If p-value smaller than 0.05 -> differentiation:
DF.TS.diff <- diff(DF.TS.select)
kpss.test(DF.TS.diff)


# If necessary repeat differentiation
DF.TS.diff2 <- diff(DF.TS.diff)
kpss.test(DF.TS.diff2)
DF.TS.diff <- DF.TS.diff2


#Plot of differentiated time series:
plot.zoo(DF.TS.diff,
         main = 'Differentiated Times Series',
         ylab = '', xlab = '')


#------------------------------------------------------------------------------
# Step 5: ACF and PACF
#------------------------------------------------------------------------------


# Plot ACF & PACF:
acf_plot <- autoplot(Acf(DF.TS.diff, 
                         plot = F, 
                         lag.max = 35)) + ggtitle('ACF')
pacf_plot <- autoplot(Acf(DF.TS.diff, 
                          plot = F, 
                          lag.max = 35, 
                          type = 'partial')) + ggtitle('PACF')
grid.arrange(acf_plot, pacf_plot, ncol = 2)


#------------------------------------------------------------------------------
# Step 6: Setup of the parameters:
#------------------------------------------------------------------------------


# Choose parameters for the ARIMA-Model:
p_value <- 0
d_value <- 2
q_value <- 2

fit.test <- Arima(DF.TS.training, order = c(p_value,d_value,q_value))
summary(fit.test)


# Execution Ljung-Box-Test
LjungBoxTest <- Box.test(residuals(fit.test), 
                         lag = 10, 
                         fitdf = 6, 
                         type = "Ljung")
LjungBoxTest



# Robustness check:
fit.00 <- Arima(DF.TS.training, order = c(0,d_value,0))
fit.01 <- Arima(DF.TS.training, order = c(0,d_value,1))
fit.02 <- Arima(DF.TS.training, order = c(0,d_value,2))
fit.03 <- Arima(DF.TS.training, order = c(0,d_value,3))
fit.04 <- Arima(DF.TS.training, order = c(0,d_value,4))
fit.05 <- Arima(DF.TS.training, order = c(0,d_value,5))
fit.10 <- Arima(DF.TS.training, order = c(1,d_value,0))
fit.11 <- Arima(DF.TS.training, order = c(1,d_value,1))
fit.12 <- Arima(DF.TS.training, order = c(1,d_value,2))
fit.13 <- Arima(DF.TS.training, order = c(1,d_value,3))
fit.14 <- Arima(DF.TS.training, order = c(1,d_value,4))
fit.15 <- Arima(DF.TS.training, order = c(1,d_value,5))
fit.20 <- Arima(DF.TS.training, order = c(2,d_value,0))
fit.21 <- Arima(DF.TS.training, order = c(2,d_value,1))
fit.22 <- Arima(DF.TS.training, order = c(2,d_value,2))
fit.23 <- Arima(DF.TS.training, order = c(2,d_value,3))
fit.24 <- Arima(DF.TS.training, order = c(2,d_value,4))
fit.25 <- Arima(DF.TS.training, order = c(2,d_value,5))
fit.30 <- Arima(DF.TS.training, order = c(3,d_value,0))
fit.31 <- Arima(DF.TS.training, order = c(3,d_value,1))
fit.32 <- Arima(DF.TS.training, order = c(3,d_value,2))
fit.33 <- Arima(DF.TS.training, order = c(3,d_value,3))
fit.34 <- Arima(DF.TS.training, order = c(3,d_value,4))
fit.35 <- Arima(DF.TS.training, order = c(3,d_value,5))
fit.40 <- Arima(DF.TS.training, order = c(4,d_value,0))
fit.41 <- Arima(DF.TS.training, order = c(4,d_value,1))
fit.42 <- Arima(DF.TS.training, order = c(4,d_value,2))
fit.43 <- Arima(DF.TS.training, order = c(4,d_value,3))
fit.44 <- Arima(DF.TS.training, order = c(4,d_value,4))
fit.45 <- Arima(DF.TS.training, order = c(4,d_value,5))
fit.50 <- Arima(DF.TS.training, order = c(5,d_value,0))
fit.51 <- Arima(DF.TS.training, order = c(5,d_value,1))
fit.52 <- Arima(DF.TS.training, order = c(5,d_value,2))
fit.53 <- Arima(DF.TS.training, order = c(5,d_value,3))
fit.54 <- Arima(DF.TS.training, order = c(5,d_value,4))
fit.55 <- Arima(DF.TS.training, order = c(5,d_value,5))

values <- c(fit.00$aicc, fit.01$aicc, fit.02$aicc, fit.03$aicc, fit.04$aicc, 
            fit.05$aicc, fit.10$aicc, fit.11$aicc, fit.12$aicc, fit.13$aicc, 
            fit.14$aicc, fit.15$aicc, fit.20$aicc, fit.21$aicc, fit.22$aicc, 
            fit.23$aicc, fit.24$aicc, fit.25$aicc, fit.30$aicc, fit.31$aicc, 
            fit.32$aicc, fit.33$aicc, fit.34$aicc, fit.35$aicc, fit.40$aicc, 
            fit.41$aicc, fit.42$aicc, fit.43$aicc, fit.44$aicc, fit.45$aicc,
            fit.50$aicc, fit.51$aicc, fit.52$aicc, fit.53$aicc, fit.54$aicc, 
            fit.55$aicc)
models <- c('ARMA (0,0)', 'ARMA(0,1)', 'ARMA(0,2)', 'ARMA(0,3)', 'ARMA(0,4)', 
            'ARMA(0,5)', 'ARMA(1,0)', 'ARMA(1,1)', 'ARMA(1,2)',
            'ARMA(1,3)', 'ARMA(1,4)', 'ARMA(1,5)', 'ARMA(2,0)', 'ARMA(2,1)', 
            'ARMA(2,2)', 'ARMA(2,3)', 'ARMA(2,4)', 'ARMA(2,1,5)',
            'ARMA(3,0)', 'ARMA(3,1,1)', 'ARMA(3,2)', 'ARMA(3,3)', 'ARMA(3,4)', 
            'ARMA(3,5)', 'ARMA(4,0)', 'ARMA(4,1)', 'ARMA(4,2)',
            'ARIMA(4,3)', 'ARMA(4,4)', 'ARMA(4,5)', 'ARMA(5,0)', 'ARMA(5,1)', 
            'ARMA(5,2)', 'ARMA(5,3)', 'ARMA(5,4)', 'ARMA(5,5)')
robustness <- data.frame(Modell=models, Aicc=values, stringsAsFactors = FALSE)





#------------------------------------------------------------------------------
# Auto-ARIMA-Function for forecast plot
#------------------------------------------------------------------------------

# Creation Auto-ARIMA model
fit.auto <- auto.arima(DF.TS.training, approximation = FALSE ,
                       trace = TRUE, ic = 'aicc', 
                       allowdrift = TRUE, allowmean = FALSE)
summary(fit.auto)


# Plot Auto-ARIMA-Forecast:
autari.forecast <- forecast(fit.auto, h = length(DF.TS.test))
plot(autari.forecast)


# Plot test set
lines(ts(coredata(DF.TS.test),
         start = start(autari.forecast$mean)[1],
         frequency = 1), col = 'red')


#------------------------------------------------------------------------------
# Calculation of results
#------------------------------------------------------------------------------


# Percentage devation: Forecast vs. Real observations:
results <- data.frame(DF.TS.test, autari.forecast$mean, autari.forecast$lower, autari.forecast$upper)
dev_mean <- round((results$autari.forecast.mean-results$closing_price)/results$autari.forecast.mean*100,2)
devlo_95 <- round((results$X95.-results$closing_price)/results$X95.*100,2)
devlo_80 <- round((results$X80.-results$closing_price)/results$X80.*100,2)
devhi_80 <- round((results$X80..1-results$closing_price)/results$X80..1*100,2)
devhi_95 <- round((results$X95..1-results$closing_price)/results$X95..1*100,2)


# Plot percentage devation
perc_dev <- data.frame(x = 1:length(DF.TS.test), dev_mean, devlo_80, devlo_95, devhi_80, devhi_95)
head(perc_dev)

plot(perc_dev$x, perc_dev$dev_mean, 
     type = "l", 
     col = "magenta", 
     ylim = c(- 150, 100), 
     main = "Forecasts vs. Real Observations", 
     ylab = "Percentage Deviation", 
     xlab = "Forecasted Observations")

lines(perc_dev$x, perc_dev$devhi_80, type = "l", col = "orange")
lines(perc_dev$x, perc_dev$devhi_95, type = "l", col = "dark red")
lines(perc_dev$x, perc_dev$devlo_95, type = "l", col = "dark red")
lines(perc_dev$x, perc_dev$devlo_80, type = "l", col = "orange")
legend(1, 95, 
       legend=c("High 95%", "High 80%", "Mean", "Low 80%", "Low 95%"),
       col=c("dark red", "orange", "magenta", "orange", "dark red"), lty=1:1, cex=0.8)

# Volatility of the test set
df_ts_returns <- CalculateReturns(DF.TS.training)
plot.zoo(df_ts_returns,
         main = 'Returns',
         ylab = 'Returns in %', xlab = 'Time Period Training-Set')
chart.RollingPerformance(R = df_ts_returns,
                         width = 22,
                         FUN = "sd.annualized",
                         main = "Annual 30-Day Volatility",
                         ylim = c(0, 1)
)