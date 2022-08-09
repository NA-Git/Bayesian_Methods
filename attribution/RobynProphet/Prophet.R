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
df <- read.csv('G:/My Drive/IN/Data/Prophet/kind_prophet.csv')

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

prophet_holidays <- read.csv("G:/My Drive/IN/Data/Prophet/prophet_holidays.csv")
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
    mcmc.samples = 800, 
    interval.width = .20, 
    uncertainty.samples = 200,
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
df.cv <- performance_metrics(cv)
head(df.cv)
# this plots a performance metric, in this case RMSE, over the forecast horizon
plot_cross_validation_metric(cv, metric = 'rmse', rolling_window = .1)
plot_cross_validation_metric(cv, metric = 'mape', rolling_window = .1)
plot_cross_validation_metric(cv, metric = 'smape', rolling_window = .1)

# calculates a variet of statistics to estimate model quality, including 
# mse, rmse, mae, mape, mdape, smape, coverage
pred_sample = predictive_samples(m, df_train)
twelve_wk_RMSE = sqrt(mean((pred_sample$trend-pred_sample$yhat)^2))
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
print('RMSE for the last two weeks:')
print(twelve_wk_RMSE)
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
