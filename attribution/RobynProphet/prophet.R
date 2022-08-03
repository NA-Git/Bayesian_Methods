# install.packages('prophet')
# install.packages('timeDate')
# install.packages('dplyr')
# install.packages('lubridate')

library('prophet')
library('timeDate')
library('dplyr')
library('lubridate')

df <- read.csv('C:/Users/norri/Desktop/Prophet/amazon_forecast.csv')

df <- df[order(df$ds),]
df$ds <- as.POSIXct(df$ds,"%Y-%m-%d", tz = "UTC", origin="2018-06-03")

#examine change points later, also prior arguments
# add holiday feature

# m <- prophet(
#     df = df,
#     growth = "linear",
#     changepoints = TRUE,
#     n.changepoints = 25,
#     changepoint.range = 0.8,
#     yearly.seasonality = "auto",
#     weekly.seasonality = "auto",
#     daily.seasonality = "auto",
#     # holidays = TRUE,
#     seasonality.mode = "multiplicative",
#     seasonality.prior.scale = 26,
#     holidays.prior.scale = 26,
#     changepoint.prior.scale = 0.05,
#     mcmc.samples = 10,
#     interval.width = 0.8,
#     uncertainty.samples = 500,
#     fit = TRUE
# )

m <- prophet(df)

future <- make_future_dataframe(m, periods = 13, freq = 'week')
forecast <- predict(m, future)

fcst <- predict(m, future)
plot(m, fcst)

history <- data.frame(ds = seq(as.Date('2018-06-03'), as.Date('2022-06-05'), by = 'd'),
                      y = sin(1:366/200) + rnorm(366)/10)
# plot(
#   m_2,
#   fcst,
#   uncertainty = TRUE,
#   plot_cap = TRUE,
#   xlabel = "ds",
#   ylabel = "y"
# )

prophet_plot_components(m, forecast)

plot_forecast_component(m, 
                        fcst, 'trend', uncertainty = TRUE, 
                        plot_cap = FALSE)

dyplot.prophet(m, fcst, uncertainty = TRUE)

# get inpretation
plot(m, fcst) + add_changepoints_to_plot(m)

# check
cv <-cross_validation(
  m,
  7,
  'weeks',
  period = 52,
  initial = NULL,
  cutoffs = NULL
)

performance_metrics(cv, metrics = NULL, rolling_window = 0.1)

post_samples <- predictive_samples(m, df)

regressor_coefficients(m)
