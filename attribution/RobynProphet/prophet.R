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

# this file was preformatted to work with Prophet
df <- read.csv('G:/My Drive/IN/Data/Prophet/np_prophet.csv')

# to run prophet, there needs to be only two columns: ds, which is 
# formatted in YYYY-MM-DD format, and y, which is usually sales
# for the model to be working correctly, date needs to be sorted in
# descending order, then converted to POSIXct format. setting the
# starting date, or origin, is absolutely necessary
# all other columns can be dropped, nor need to be pivoted

# I ordered the file in advance, but the conversion to POSIXct is crucial
df <- df[order(df$ds),] # orders the df$ds column in ascending order
df$ds <- as.POSIXct(df$ds,"%Y-%m-%d", tz = "UTC", origin="2018-06-03")

## I split up the files for testing, training, and holidays
df <- subset(df, ds > '2020-02-28' & ds < '2022-05-22')

df_train <- subset(df, ds > '2020-03-01' & ds < '2022-02-27')

df_test <- subset(df, ds > '2022-02-28' & ds < '2022-05-22')

prophet_holidays <- read.csv("G:/My Drive/IN/DATA/Robyn/dt_prophet_holidays.csv")
prophet_holidays <- prophet_holidays[prophet_holidays$country == 'US', ]

# I've chosen to define most parameters in the Prophet
# functions that is used in most of the remaining program
# the most important arguments to set are growth, seasonality,
# all function definitions are available 
# here: https://cran.r-project.org/web/packages/prophet/prophet.pdf

m <- prophet(
    df_train,
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
plot(m, fcst)

# plots trend, holidays, seasonality, if those are included
prophet_plot_components(m, forecast)

# this plots the third argument from FCST, in this case 'trend'
plot_forecast_component(m, fcst, 'trend', uncertainty = TRUE, 
                        plot_cap = FALSE)

# a ggplot that provides the forecast, actual value,
# and a slider to see the forecast and actual values
dyplot.prophet(m, fcst, uncertainty = TRUE)

# creates lines to overlay significant changes in the forecast plot
plot(m, fcst) + add_changepoints_to_plot(m)

# the following functions cross-validate
# and provide the RMSE stat, among others
cv <-cross_validation(m, 7, 'weeks', period = 12, initial = NULL, 
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

#### Creates two 12 week windows from the predicted and actual values
#### to be used to plot against each other and create more metrics
fcst_12 <- subset(fcst, ds > '2022-02-28' & ds < '2022-05-22')
fcst_12 <- fcst_12[,c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]
names(fcst_12)[2] <- "y"
fcst_12$y_obs <- df_test$y

# plot of y_hat, y_hat_upper, y_hat_lower, and observed against each other
matplot(fcst_12$ds, fcst_12, type = c("l"), lty = 1, lwd = 2,
        pch = 1, col = 2:5, xlab = 'Time', ylab = 'Predicted Revenue', 
        axes = FALSE,)
leg.txt <- c("Observed", "Predicted", "Pred. Lower Bound", "Pred. Upper Bound")
legend('top', leg.txt, bg = "gray90", col=2:5, pch=1)
axis(1, fcst_12$ds, format(fcst_12$ds, "%b %d"), cex.axis = .7)
box()

###### Calculations of RMSE and two for RSS for the past 12 weeks ####
test = df_test$y
fcst_y = fcst_12$y

# # Backup RSS from the 12 week prediction period
# total = 0
# for (i in 1:12) {
#   temp = (fcst_y[i] - test[i])^2
#   total = total + temp
# }
# # print(total)

## This can recreate the chart if need be
# Chart <- data.frame(matrix(ncol = 5, nrow = 2))
# colnames(Chart) <- c('RSS', 'RSME', 'RSME_12', 'MAPE', 'SMAPE')

# this section records the statistics for the chart

# RSS - a measure that subtracts the predicted value from the model
# from the observed and squares their difference, then sums all of the
# total differences

# RSME - is an excellent measure that minimizes deviations or residuals, but
# is sensitive to outliers

# RSME_12 - We performed the RMSE calculation for the last twelve weeks
# of the predictions against the actuals

# MAPE - an intuitive diagnostic that is often used for ML, typically
# used for forecasting accuracy. There are some issues with its interpretation,
# namely it's unusual behavior nearer zero

# SMAPE - a relative of MAPE, it is based on on percentages away in error.
# it does not deal well with some asymmetric data

RSS <- sum((fcst_y - test)^2)
RSME <- round(mean(rmse$rmse), 4)
RSME_12 <- round(sqrt(mean((test - fcst_y)^2)), 4)
MAPE <- round(mean(mape$mape), 4)
SMAPE <- round(mean(smape$smape), 4)

# by adding the starts to this chart, printing it,
# and commenting it out, it would be useful for testing
Chart[1,1:5] <- c(RSS, RSME, RSME_12, MAPE, SMAPE)
print(Chart)

# Nature's Path Model #1 mk 1
# RSS     RSME  RSME_12   MAPE  SMAPE
# 1 55321963933 192098.5 67898.19 0.2832 0.2646

# Nature's Path Model #1 mk2
# RSS     RSME  RSME_12   MAPE  SMAPE
# 1 57887436328 58473.42 69454.68 0.1122 0.0999

#################### SUMMARY ####################
#### Summary: training model over two years may have improved
#### fit, it seems like holiday is throwing at least one year's
#### worth of modeling is off, the RSME, MAPE, and SMAPE have
#### continuously improved, as well as the unexpected spike
#### right before the the prediction period ended did not help
#### anything