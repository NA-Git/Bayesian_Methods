CREATE OR REPLACE MODEL bq_dev.forecast 
OPTIONS(model_type='ARIMA_PLUS',
    time_series_data_col='sales',
    time_series_timestamp_col='week',
    data_frequency='WEEKLY',
    holiday_region='US' ) AS
SELECT
  avg(revenue) as sales,
  week
FROM
   `tyson-ml-07-28-22.bq_dev.kind_forecast_train` AS kind_data
   group by week
ORDER BY
  week

SELECT 
 *
 FROM ML.FORECAST(MODEL bq_dev.forecast, STRUCT(16 AS horizon, 0.8 AS confidence_level))