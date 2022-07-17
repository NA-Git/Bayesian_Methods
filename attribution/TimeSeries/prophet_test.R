install.packages('prophet')
install.packages('timeDate')
install.packages('dplyr')
install.packages('lubridate')

library('prophet')
library('dplyr')
library('lubridate')

df <- read.csv('C:/Users/norri/Desktop/Prophet/amazon_prophet.csv')

# to run prophet, there needs to be only two columns: ds, which is 
# formatted in YYYY-MM-DD format, and y, which is usually sales
# for the model to be working correctly, date needs to be sorted in
# descending order, then converted to POSIXct format. setting the
# starting date, or origin, is absolutely necessary

df <- df[order(df$ds),] # orders the date or the df$ds column in ascending order
df$ds <- as.POSIXct(df$ds,"%Y-%m-%d", tz = "UTC", origin="2018-06-03")

# I've chosen to define most parameters in the prophet
# function that is used in most of the remaining program
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

# this bit disable the standard weekly seasonality
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

# history <- data.frame(ds = seq(as.Date('2018-06-03'), as.Date('2022-06-05'), by = 'd'),
#                       y = sin(1:366/200) + rnorm(366)/10)

prophet_plot_components(m, forecast)

plot_forecast_component(m, fcst, 'trend', uncertainty = TRUE, 
                        plot_cap = FALSE)

dyplot.prophet(m, fcst, uncertainty = TRUE)

plot(m, fcst) + add_changepoints_to_plot(m)

cv <-cross_validation(m, 7, 'weeks', period = 98, 
                      initial = NULL, cutoffs = NULL)

plot_cross_validation_metric(cv, metric = 'rmse', rolling_window = .1)
pred_sample = predictive_samples(m, df)

rmse = performance_metrics(cv, metrics = NULL,
                           rolling_window = 0.1)
print(mean(rmse$rmse))

