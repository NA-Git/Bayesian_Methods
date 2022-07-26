################## Dataset Construction ################
### This data was created from the Amazon Kind & Nature's Path
### It was selected down to either just kind or just NP,
### Then the date column was renamed to 'ds' and the sales column to 'y'
# install.packages('prophet')
# install.packages('timeDate')
# install.packages('dplyr')
# install.packages('lubridate')

library('prophet')
library('dplyr')
library('lubridate')
library('ggplot2')

start = Sys.time()

getwd()
setwd('/home/matt/Insync/matthew@mdnorris.com/Google Drive/To_Do/IN_DS/Prophet')
getwd()

# this file was preformatted to work with Prophet
df <- read.csv('np_prophet.csv')

# to run prophet, there needs to be only two columns: ds, which is 
# formatted in YYYY-MM-DD format, and y, which is usually sales
# for the model to be working correctly, date needs to be sorted in
# descending order, then converted to POSIXct format. setting the
# starting date, or origin, is absolutely necessary
# all other columns can be dropped, nor need to be pivoted

# I ordered the file in advance, but the conversion to POSIXct is crucial
df <- df[order(df$ds),] # orders the df$ds column in ascending order
df$ds <- as.POSIXct(df$ds,"%Y-%m-%d", tz = "UTC", origin="2018-06-03")

df <- subset(df, ds > '2020-02-28' & ds < '2022-05-22')

df_train <- subset(df, ds > '2020-03-01' & ds < '2022-02-27')

df_test <- subset(df, ds > '2022-02-28' & ds < '2022-05-22')

prophet_holidays <- read.csv("prophet_holidays.csv")
prophet_holidays <- prophet_holidays[prophet_holidays$country == 'US', ]

# I've chosen to define most parameters in the Prophet
# functions that is used in most of the remaining program
# the most important arguments to set are growth, seasonality,
# all function definitions are available 
# here: https://cran.r-project.org/web/packages/prophet/prophet.pdf

m <- prophet(
    df = df_train,
    growth = "linear",
    # changepoints = NULL,
    # n.changepoints = 25,
    changepoint.range = 0.8,
    yearly.seasonality = TRUE, 
    weekly.seasonality = TRUE, 
    daily.seasonality = FALSE,
    holidays = prophet_holidays,
    seasonality.mode = "additive", 
    seasonality.prior.scale = 10, 
    holidays.prior.scale = 10, 
    changepoint.prior.scale = .5,
    mcmc.samples = 600, 
    interval.width = .25, 
    uncertainty.samples = 150,
    fit = TRUE
)

# # The following process creates a dataframe for a 
# # forecast to be added to
# # note that both number of periods and length of
# # intervals can be see and should be consistent throughout

future <- make_future_dataframe(m, periods = 12, freq = 'week')
forecast <- predict(m, future)

fcst <- predict(m, future)
plot(m, fcst, main = "Fig 1", xlab = 'Time',
     ylab = 'Revenue')

# plots trend, holidays, seasonality, if those are included
prophet_plot_components(m, forecast)

# this plots the third argument from FCST, in this case 'trend'
plot_forecast_component(m, fcst, 'trend', uncertainty = TRUE, 
                        plot_cap = FALSE)

# a ggplot that provides the forecast, actual value,
# and a slider to see the forecast and actual values
dyplot.prophet(m, fcst, uncertainty = TRUE)

# creates lines to overlay significant changes in the forecast plot
# plot(m, fcst) + add_changepoints_to_plot(m)

# the following functions cross-validate
# and provide the RMSE stat, among others
cv <-cross_validation(m, 7, 'weeks', period = 6, initial = NULL, 
                      cutoffs = NULL)

# this plots a performance metric, in this case RMSE, over the forecast horizon
plot_cross_validation_metric(cv, metric = 'rmse', rolling_window = .1)
plot_cross_validation_metric(cv, metric = 'mape', rolling_window = .1)
plot_cross_validation_metric(cv, metric = 'smape', rolling_window = .1)

# calculates a variet of statistics to estimate model quality, including 
# mse, rmse, mae, mape, mdape, smape, coverage
pred_sample = predictive_samples(m, df_train)

# calculates the RMSE, MAPE, and SMAPE for comparison to other models
rmse = performance_metrics(cv, metrics = 'rmse',
                           rolling_window = 0.1)
mape = performance_metrics(cv, metrics = 'mape',
                           rolling_window = 0.1)
smape = performance_metrics(cv, metrics = 'smape',
                           rolling_window = 0.1)

print(mean(rmse$rmse))
print(mean(mape$mape))
print(mean(smape$smape))

fcst_12 <- subset(fcst, ds > '2022-02-28' & ds < '2022-05-22')
fcst_12 <- fcst_12[,c('ds', 'yhat')]
names(fcst_12)[2] <- "y"

actual <- rbind(df_train, df_test)
predicted <- rbind(df_train, fcst_12)

# plotting of actual data against forecast for 12 weeks
xdata <- actual$ds
y1 <- actual$y
y2 <- predicted$y

# First curve is plotted
plot(xdata, y1, type="o", col="blue", pch="+", lty=2)

# Add second curve to the same plot by calling points() and lines()
points(xdata, y2, col="green", pch="x")
lines(xdata, y2, col="green",lty=2)

# Calculation of RMSE for the last twelve weeks of the forecast
test = df_test$y
fcst_y = fcst_12$y
sqrt(mean((test - fcst_y)^2))

# RSS from the 12 week prediction period
sum((fcst_y - test)^2)

total = 0 
for (i in 1:12) { 
  temp = (fcst_y[i] - test[i])^2 
  total = total + temp }
### KIND and Nature's Path tests

# 1st test RMSE: 237838.3 & MAPE: .1755 & SMAPE: .1529
# increasing prior value of seasonality
# 2nd test RMSE: 270038.5 & MAPE: .1779 & SMAPE: .1452
# change additive to multiplicative
# 3rd test RMSE: 1302971 & MAPE: 0.7202772 & SMAPE: 0.3971218
# changed back to additive, lowered seasonality
# 4th test RMSE: 261580.7 & MAPE: 0.1738695 & SMAPE: 0.1519308
# increased MCMC samples
# 5th test RMSE: 257949.5 & MAPE: 0.1703171 & SMAPE: 0.1490988
# lowered MCMC samples to 400
# 6th test RMSE: 284216.8 & MAPE: 0.1836734 & SMAPE: 0.1569333
# lowered interval size
# 7th test RMSE: 262790.4 & MAPE: 0.1743669 & SMAPE: 0.1521979
# increased interval width
# 8th test RMSE: 274505.8 & MAPE: 0.180157 & SMAPE: 0.1555094
# .35 interval width and 200 fewer samples
# 9th test RMSE: 261817 & MAPE: 0.1743573 & SMAPE: 0.1522331
# increase samples, decrease interval, and increase seasonality prior

# LAST RUN KIND
# 10th RUN - RMSE: 257864.9 & MAPE: 0.1713011 & SMAPE: 0.1502048

# LAST RUN NATURE"S PATH
# 11th RUN - RMSE: 57629.27 & MAPE: 0.167076 & SMAPE: 0.1349863

end = Sys.time()
duration = end - start
print(duration)

###???################# SUMMARY ####################
#### Summary: training model over two years provided decent fit
#### but it is also possible an outlier holiday may have distorted
#### at least one year's worth of modeling of
#### the RSME, MAPE, and SMAPE. The did appear to have, with tweaking,
#### continuously improved, as well as the unexpected spike
#### right before the the prediction period ended did not help
#### anything
#### ??? Do you think the approximate spike around the end of the
#### train period may have skewed the prediction