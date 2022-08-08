CREATE OR REPLACE MODEL bq_dev.forecast 
OPTIONS(model_type='ARIMA_PLUS',
    time_series_data_col='sales',
    time_series_timestamp_col='week',
    data_frequency='WEEKLY',
    holiday_region='US' ) AS
SELECT
  avg(sales) as sales,
  week
FROM
   projectmercuryv1.bq_dev_db.kind_ml 
   group by week
ORDER BY
  week

SELECT 
 *
 FROM ML.FORECAST(MODEL bq_dev.forecast, STRUCT(12 AS horizon, 0.8 AS confidence_level))
