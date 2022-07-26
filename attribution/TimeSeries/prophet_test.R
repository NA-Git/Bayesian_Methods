# install.packages('prophet')
# install.packages('timeDate')
# install.packages('dplyr')
# install.packages('lubridate')

library('prophet')
library('dplyr')
library('lubridate')

# this file was preformated to work with Prophet
df <- read.csv('G:/My Drive/To_Do/IN_DS/Prophet/amazon_prophet.csv')

# to run prophet, there needs to be only two columns: ds, which is 
# formatted in YYYY-MM-DD format, and y, which is usually sales
# for the model to be working correctly, date needs to be sorted in
# descending order, then converted to POSIXct format. setting the
# starting date, or origin, is absolutely necessary

# I ordered the file in advance, but the conversion to POSIXct is crucial
df <- df[order(df$ds),] # orders the date or the df$ds column in ascending order
df$ds <- as.POSIXct(df$ds,"%Y-%m-%d", tz = "UTC", origin="2018-06-03")

# I've chosen to define most parameters in the Prophe
# functions that is used in most of the remaining program
# the most important arguments to set are growth, seasonality,
# zll function definitions are available 
# here: https://cran.r-project.org/web/packages/prophet/prophet.pdf


m <- prophet(
    df,
    growth = "linear",
    changepoints = NULL,
    n.changepoints = 25,
    changepoint.range = 0.8,
    yearly.seasonality = "auto",
    weekly.seasonality = "TRUE",
    daily.seasonality = "auto",
    holidays = NULL,
    seasonality.mode = "additive",
    seasonality.prior.scale = 10,
    holidays.prior.scale = 10,
    changepoint.prior.scale = 0.05,
    mcmc.samples = 5,
    interval.width = 0.8,
    uncertainty.samples = 500,
    fit = TRUE
)

# this bit disables the standard weekly seasonality
# and redefines it with parameters that are more suitable
# for weekly data
m <- prophet(weekly.seasonality=FALSE)
m <- add_seasonality(m, name='weekly', 
                     period=7, fourier.order=5)
m <- fit.prophet(m, df)


# The following process creates a dataframe for a 
# forecast to be added to
# note that both number of periods and length of
# intervals can be see and should be consistent throughout

future <- make_future_dataframe(m, periods = 52, freq = 'week')
forecast <- predict(m, future)

# even though forecast and fcst seem to be the same
# the data structure is different between the two
# and is suitable for different functions
fcst <- predict(m, future)
plot(m, fcst)

# plots trend, holidays, seasonality, if those are included
prophet_plot_components(m, forecast)

# this plots the third argument from fcst, in this case 'trend'
plot_forecast_component(m, fcst, 'trend', uncertainty = TRUE, 
                        plot_cap = FALSE)

# a ggplot that provides the forecast, actual value,
# and a slider to see the forecast and actual values
dyplot.prophet(m, fcst, uncertainty = TRUE)

# creates lines to overlay significant changes in the forecast plot
plot(m, fcst) + add_changepoints_to_plot(m)

# the following functions cross-validate
# and provide the RMSE stat
cv <-cross_validation(m, 7, 'weeks', period = 98, 
                      initial = NULL, cutoffs = NULL)

# this plots a performance metric, in this case rmse, over the forecast horizon
plot_cross_validation_metric(cv, metric = 'rmse', rolling_window = .1)
pred_sample = predictive_samples(m, df)

# calculates the rmse for comparison to other models
rmse = performance_metrics(cv, metrics = NULL,
                           rolling_window = 0.1)
print(mean(rmse$rmse))

