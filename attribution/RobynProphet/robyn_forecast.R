################################################################
#### Step 0: Setup environment
# It's best to install and load in this order, it has yet to fail
install.packages("reticulate") # Install reticulate first if you haven't alreadyt
remotes::install_github("facebookexperimental/Robyn/R")
install.packages(c('nloptr', 'lares', 'dplyr', 'h2o', 'reticulate', 'tidyr'))
library(Robyn)
library(npreg)
library(dplyr)
library(nloptr)
library(lares)
library(tidyr)
library(reticulate)
library(h2o)
h2o.init()
conda_create("r-reticulate")
conda_install("r-reticulate", "nevergrad", pip = TRUE)
use_condaenv("r-reticulate")
################################################################
#### Step 1: Load data
getwd()
setwd('G:/My Drive/IN/Data/Robyn')
### Force multicore when using RStudio
Sys.setenv(R_FUTURE_FORK_ENABLE = TRUE)
options(future.fork.enable = TRUE)
df <- read.csv('robyn_cortex_sparse.csv', fileEncoding = 'UTF-8-BOM')
## Check holidays from Prophet and select from your country
data("dt_prophet_holidays")
head(dt_prophet_holidays)

## Set robyn_object. It must have extension .RDS. The object name can
# be different than Robyn, but ideally should not be moved
robyn_object <- "'G:/My Drive/IN/Data/Robyn/MyRobyn.RDS"
################################################################
### Step 1.5: Data Planning
# # DATE
# names(df)[names(df) == "Date"] <- "DATE"
# df$DATE <- as.POSIXlt(df$DATE, "%Y-%m-%d", tz = "UTC", origin = "2019-09-30")

################################################################
#### Step 2a: For first time user: Model specification in 4 steps
#### 2a-1: First, specify input variables

InputCollect <- robyn_inputs(
  dt_input = df
  , dt_holidays = dt_prophet_holidays
  , date_var = "DATE" # date format must be "2020-01-01"
  , dep_var = "revenue" # there should be only one dependent variable
  , dep_var_type = "revenue" # "revenue" (ROI) or "conversion" (CPA)
  , prophet_vars = c("trend", "season", "holiday") # "trend","season",
  # "weekday" & "holiday"
  , prophet_country = "US" # input one country of dt_prophet_holidays
  , context_vars = ("cag_V") # e.g. competitors, discount, unemployment etc
  , paid_media_spends = c("Influencer_S", "Social_Media_S", "Radio_S") # mandator input
  , paid_media_vars = c("Influencer_I", "Social_Media_I", "Radio_I") # mandatory.
  # paid_media_vars must have same order as paid_media_spends. Use media exposure
  # metrics like
  # impressions, GRP etc. If not applicable, use spend instead.
  # , organic_vars = c("beef", 'chicken') # marketing activity without media spend
  # ,factor_vars = ("incidents") # specify which variables in context_vars or
  # organic_vars are factorial
  # prophet pulls in your date range from the date variable, but the window start
  # and window end require you to put in dates that are in between and smaller
  # than your total dates
  , window_start = "2019-10-21"
  , window_end = "2021-04-19"
  , adstock = "weibull_pdf" # geometric, weibull_cdf or weibull_pdf.
)
print(InputCollect)


#### 2a-2: Second, define and add hyperparameters
#### finished 2a-2
## -------------------------------- NOTE v3.6.0 CHANGE !!! -------------------------- ##
## hyperparameter names needs to be base on paid_media_spends names. Run:
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
plot_adstock(plot = TRUE)
plot_saturation(plot = TRUE)
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
## to see correct hyperparameter names. Check GitHub homepage for background of change.
## Also calibration_input are required to be spend names.
## ----------------------------------------------------------------------------------- ##
## Guide to setup & understand hyperparameters

## 1. IMPORTANT: set plot = TRUE to see helper plots of hyperparameter's
# effect in transformation

## Write function here to pass through all the info
## 2. Get correct hyperparameter names:
# All variables in paid_media_spends and organic_vars require hyperparameter and will be
# transformed by adstock & saturation.

######### Run hyper_names() as above to get correct media hyperparameter names. All names in
######### hyperparameters must equal names from hyper_names(), case sensitive.
######### Run ?hyper_names to check parameter definition.

## 3. Hyperparameter interpretation & recommendation:

###??? Geometric adstock: Theta is the only parameter and means fixed decay rate. Assuming TV
###??? spend on day 1 is 100€ and theta = 0.7, then day 2 has 100*0.7=70€ worth of effect
###??? carried-over from day 1, day 3 has 70*0.7=49€ from day 2 etc. Rule-of-thumb for common
###? media genre: TV c(0.3, 0.8), OOH/Print/Radio c(0.1, 0.4), digital c(0, 0.3)

###??? Weibull CDF adstock: The Cumulative Distribution Function of Weibull has two parameters
# , shape & scale, and has flexible decay rate, compared to Geometric adstock with fixed
# decay rate. The shape parameter controls the shape of the decay curve. Recommended
# bound is c(0.0001, 2). The larger the shape, the more S-shape. The smaller, the more
# L-shape. Scale controls the inflexion point of the decay curve. We recommend very
# conservative bounce of c(0, 0.1), because scale increases the adstock half-life greatly.

###??? Weibull PDF adstock: The Probability Density Function of the Weibull also has two
# parameters, shape & scale, and also has flexible decay rate as Weibull CDF. The
# difference is that Weibull PDF offers lagged effect. When shape > 2, the curve peaks
# after x = 0 and has NULL slope at x = 0, enabling lagged effect and sharper increase and
# decrease of adstock, while the scale parameter indicates the limit of the relative
# position of the peak at x axis; when 1 < shape < 2, the curve peaks after x = 0 and has
# infinite positive slope at x = 0, enabling lagged effect and slower increase and decrease
# of adstock, while scale has the same effect as above; when shape = 1, the curve peaks at
# x = 0 and reduces to exponential decay, while scale controls the inflexion point; when
# 0 < shape < 1, the curve peaks at x = 0 and has increasing decay, while scale controls
# the inflexion point. When all possible shapes are relevant, we recommend c(0.0001, 10)
# as bounds for shape; when only strong lagged effect is of interest, we recommend
# c(2.0001, 10) as bound for shape. In all cases, we recommend conservative bound of
# c(0, 0.1) for scale. Due to the great flexibility of Weibull PDF, meaning more freedom
# in hyperparameter spaces for Nevergrad to explore, it also requires larger iterations
# to converge.

###??? Hill function for saturation: Hill function is a two-parametric function in Robyn with
# alpha and gamma. Alpha controls the shape of the curve between exponential and s-shape.
# Recommended bound is c(0.5, 3). The larger the alpha, the more S-shape. The smaller, the
# more C-shape. Gamma controls the inflexion point. Recommended bounce is c(0.3, 1). The
# larger the gamma, the later the inflection point in the response curve.

## 4. Set individual hyperparameter bounds. They either contain two values e.g. c(0, 0.5),
# or only one value, in which case you'd "fix" that hyperparameter.

hyperparameters <- list(
    Influencer_S_alphas = c(0.5, 3)
  , Influencer_S_gammas = c(0.3, 1)
  , Influencer_S_scales = c(0, 0.1)
  , Influencer_S_shapes = c(2.0001, 10)
  , Radio_S_alphas = c(0.5, 3)
  , Radio_S_gammas = c(0.3, 1)
  , Radio_S_scales = c(0, 0.1)
  , Radio_S_shapes = c(2.0001, 10)
  , Social_Media_S_alphas = c(0.5, 3)
  , Social_Media_S_gammas = c(0.3, 1)
  , Social_Media_S_scales = c(0, 0.1)
  , Social_Media_S_shapes = c(2.0001, 10)
)

#### 2a-3: Third, add hyperparameters into robyn_inputs()

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)

#### 2a-4: Fourth (optional), model calibration / add experimental input

## Guide for calibration source

# 1. We strongly recommend to use experimental and causal results that are considered
# ground truth to calibrate MMM. Usual experiment types are people-based (e.g. Facebook
# conversion lift) and geo-based (e.g. Facebook GeoLift).
# 2. Currently, Robyn only accepts point-estimate as calibration input. For example, if
# 10k$ spend is tested against a hold-out for channel A, then input the incremental
# return as point-estimate as the example below.
# 3. The point-estimate has to always match the spend in the variable. For example, if
# channel A usually has $100K weekly spend and the experimental holdout is 70%, input
# the point-estimate for the $30K, not the $70K.

## -------------------------------- NOTE v3.6.4 CHANGE !!! ---------------------------------- ##
## Calibration channels need to be paid_media_spends or organic_vars name.
## ------------------------------------------------------------------------------------------ ##
calibration_input <- data.frame(
  # channel name must in paid_media_vars
  channel = c("Influencer_S", "Social_Media_S", "Radio_S"),
  # liftStartDate must be within input data range
  liftStartDate = as.Date(c("2019-12-08", "2019-12-08", "2019-12-08")),
  # liftEndDate must be within input data range
  liftEndDate = as.Date(c("2021-04-17", "2021-04-17", "2021-04-17")),
  # Provided value must be tested on same campaign level in model and same metric as dep_var_type
  liftAbs = c(400000, 300000, 200000),
  # Spend within experiment: should match within a 10% error your spend on date range for each channel from dt_input
  spend = c(357200000, 23410000, 146400000),
  # Confidence: if frequentist experiment, you may use 1 - pvalue
  confidence = c(0.85, 0.8, 0.99),
  # KPI measured: must match your dep_var
  metric = c("revenue", "revenue", "revenue")
)
InputCollect <- robyn_inputs(InputCollect = InputCollect, calibration_input = calibration_input)


################################################################
#### Step 2b: For known model specification, setup in one single step

## Specify hyperparameters as in 2a-2 and optionally calibration as in 2a-4 and provide them directly in robyn_inputs()
###? Note that any hyperparameters changed above need to be used here

# InputCollect <- robyn_inputs(
#   dt_input = df
#   , dt_holidays = dt_prophet_holidays
#   , date_var = "DATE"
#   , dep_var = "revenue"
#   , dep_var_type = "revenue"
#   , prophet_vars = c("trend", "season", "holiday")
#   , prophet_country = "US"
#   , context_vars = c("competitor_sales_B", "events")
#   , paid_media_spends = c("disp_S", "event_S", "email_S", "sm_S", "influence_S")
#   , paid_media_vars = c("disp_S", "event_S", "email_S", "app_S", "banner_S")
#   , organic_vars = c("circular_S")
#   , factor_vars = c("events")
#   , window_start = "2019-12-08"
#   , window_end = "2022-04-17"
#   , adstock = "weibull_pdf"
#   , hyperparameters = hyperparameters # as in 2a-2 above
#   , calibration_input = calibration_input # as in 2a-4 above
# )

################################################################
#### Step 3: Build initial model

## Run all trials and iterations. Use ?robyn_run to check parameter definition
OutputModels <- robyn_run(
  InputCollect = InputCollect # feed in all model specification
  , cores = NULL # default ??? Test tese functions
  #, add_penalty_factor = FALSE # Untested feature. Use with caution.
  , iterations = 2500 # recommended for the dummy dataset
  , trials = 15 # recommended for the dummy dataset
  , outputs = FALSE # outputs = FALSE disables direct model output
)
print(OutputModels)

## Check MOO (multi-objective optimization) convergence plots
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot
# check convergence rules ?robyn_converge

## Calculate Pareto optimality, cluster and export results and plots. See ?robyn_outputs
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels
  , pareto_fronts = 1
  , calibration_constraint =.1 #& default at 0.1
  , csv_out = "pareto" # "pareto" or "all"
  , clusters = TRUE # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  , plot_pareto = TRUE # Set to FALSE to deactivate plotting and saving model one-pagers
  , plot_folder = robyn_object # path for plots export
)
print(OutputCollect)

# # Run & output in one go
# OutputCollect <- robyn_run(
#   InputCollect = InputCollect
#   , cores = NULL
#   , iterations = 300
#   , trials = 3
#   , add_penalty_factor = TRUE # Test this functionality
#   , outputs = TRUE
#   , pareto_fronts = 3
#   , csv_out = "pareto"
#   , clusters = TRUE
#   , plot_pareto = TRUE
#   , plot_folder = robyn_object
# )
convergence <- robyn_converge(OutputModels)
convergence$moo_distrb_plot
convergence$moo_cloud_plot
print(OutputCollect)

##??? 4 csv files are exported into the folder for further usage. Check schema here:
## https://github.com/facebookexperimental/Robyn/blob/main/demo/schema.R
# pareto_hyperparameters.csv, hyperparameters per Pareto output model
# pareto_aggregated.csv, aggregated decomposition per independent variable of all Pareto output
# pareto_media_transform_matrix.csv, all media transformation vectors
# pareto_alldecomp_matrix.csv, all decomposition vectors of independent variables

################################################################
#### Step 4: Select and save the initial model
## Compare all model one-pagers and select one that mostly reflects your business reality
print(OutputCollect)
select_model <- "3_108_2" # select one from above
ExportedModel <- robyn_save(
  robyn_object = robyn_object # model object location and name
  , select_model = select_model # selected model ID
  , InputCollect = InputCollect
  , OutputCollect = OutputCollect
)
print(ExportedModel)
plot(ExportedModel)

################################################################
#### Step 5: Get budget allocation based on the selected model above

## Budget allocation result requires further validation. Please use this recommendation with caution.
## Don't interpret budget allocation result if selected model above doesn't meet business expectation.
###??? Look at any and all suggestions to see if the recomendations make sense;
# On occasions, they suggest dropping spend on the holidays and even no ad spend
# Check media summary for selected model
print(ExportedModel)

# Run ?robyn_allocator to check parameter definition
# Run the "max_historical_response" scenario: "What's the revenue lift potential with the
# same historical spend level and what is the spend mix?"
AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect
  , OutputCollect = OutputCollect
  , select_model = select_model
  , scenario = "max_historical_response"
  , channel_constr_low = 0.7
  , channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5)
  , export = TRUE
  , date_min = "2019-12-08"
  , date_max = "2022-04-17"
)
print(AllocatorCollect1)
plot(AllocatorCollect1)

# Run the "max_response_expected_spend" scenario: "What's the maximum response for a given
# total spend based on historical saturation and what is the spend mix?" "optmSpendShareUnit"
# is the optimum spend share.
AllocatorCollect2 <- robyn_allocator(
  InputCollect = InputCollect
  , OutputCollect = OutputCollect
  , select_model = select_model
  , scenario = "max_response_expected_spend"
  , channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7)
  , channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5)
  , expected_spend = 1000000 # Total spend to be simulated
  , expected_spend_days = 7 # Duration of expected_spend in days
  , export = TRUE
)
print(AllocatorCollect2)
AllocatorCollect2$dt_optimOut
plot(AllocatorCollect2)

## A csv is exported into the folder for further usage. Check schema here:
## https://github.com/facebookexperimental/Robyn/blob/main/demo/schema.R

## QA optimal response
# Pick any media variable: InputCollect$all_media
select_media <- "disp_S"
# For paid_media_spends set metric_value as your optimal spend
metric_value <- AllocatorCollect1$dt_optimOut[channels == select_media, optmSpendUnit]
# # For paid_media_vars and organic_vars, manually pick a value
# metric_value <- 10000

if (TRUE) {
  optimal_response_allocator <- AllocatorCollect1$dt_optimOut[
    channels == select_media, optmResponseUnit]
  optimal_response <- robyn_response(
    robyn_object = robyn_object,
    select_build = 0,
    media_metric = select_media,
    metric_value = metric_value)
  plot(optimal_response$plot)
  if (length(optimal_response_allocator) > 0) {
    cat("QA if results from robyn_allocator and robyn_response agree: ")
    cat(round(optimal_response_allocator) == round(optimal_response$response), "( ")
    cat(optimal_response$response, "==", optimal_response_allocator, ")\n")
  }
}

################################################################
#### Step 6: Model refresh based on selected model and saved Robyn.RDS object - Alpha

## NOTE: must run robyn_save to select and save an initial model first, before refreshing below
## The robyn_refresh() function is suitable for updating within "reasonable periods"
## Two situations are considered better to rebuild model:
## 1, most data is new. If initial model has 100 weeks and 80 weeks new data is added in refresh,
## it might be better to rebuild the model
## 2, new variables are added

# Run ?robyn_refresh to check parameter definition
Robyn <- robyn_refresh(
  robyn_object = robyn_object
  , dt_input = df
  , dt_holidays = dt_prophet_holidays
  , refresh_steps = 4
  , refresh_mode = "manual"
  , refresh_iters = 1000 # 1k is estimation. Use refresh_mode = "manual" to try out.
  , refresh_trials = 3
  , plot_pareto = TRUE
  , clusters = FALSE
)

## Besides plots: there're 4 csv output saved in the folder for further usage
# report_hyperparameters.csv, hyperparameters of all selected model for reporting
# report_aggregated.csv, aggregated decomposition per independent variable
# report_media_transform_matrix.csv, all media transformation vectors
# report_alldecomp_matrix.csv,all decomposition vectors of independent variables

# Export this refreshed model you wish to export
last_refresh_num <- sum(grepl('listRefresh', names(Robyn))) + 1 # Pick any refresh.
# Here's the final refresh using the model recommended by least combined normalized nrmse and decomp.rssd
ExportedRefreshModel <- robyn_save(
  robyn_object = robyn_object
  , select_model = Robyn[[last_refresh_num]]$OutputCollect$selectID
  , InputCollect = Robyn[[last_refresh_num]]$InputCollect
  , OutputCollect = Robyn[[last_refresh_num]]$OutputCollect
)

################################################################
#### Step 7: Get budget allocation recommendation based on selected refresh runs

# Run ?robyn_allocator to check parameter definition
AllocatorCollect <- robyn_allocator(
  robyn_object = robyn_object
  #, select_build = 1 # Use third refresh model
  , scenario = "max_response_expected_spend"
  , channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7)
  , channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5)
  , expected_spend = 2000000 # Total spend to be simulated
  , expected_spend_days = 14 # Duration of expected_spend in days
)
print(AllocatorCollect)
plot(AllocatorCollect)

################################################################
#### Step 8: get marginal returns

###??? Can take hours to run

## Example of how to get marginal ROI of next 1000$ from the 80k spend level for search channel

# Run ?robyn_response to check parameter definition

## -------------------------------- NOTE v3.6.0 CHANGE !!! ---------------------------------- ##
## The robyn_response() function can now output response for both spends and exposures (imps,
## GRP, circular_S sendings etc.) as well as plotting individual saturation curves. New
## argument names "media_metric" and "metric_value" instead of "paid_media_var" and "spend"
## are now used to accommodate this change. Also the returned output is a list now and
## contains also the plot.
## ------------------------------------------------------------------------------------------ ##

# Get response for 80k from result saved in robyn_object
Spend1 <- 60000
Response1 <- robyn_response(
  robyn_object = robyn_object
  #, select_build = 1 # 2 means the second refresh model. 0 means the initial model
  , media_metric = "influence_S"
  , metric_value = Spend1)
Response1$response / Spend1 # ROI for search 80k
Response1$plot

# Get response for 81k
Spend2 <- Spend1 + 1000
Response2 <- robyn_response(
  robyn_object = robyn_object
  #, select_build = 1
  , media_metric = "influence_S"
  , metric_value = Spend2)
Response2$response / Spend2 # ROI for search 81k
Response2$plot

# Marginal ROI of next 1000$ from 80k spend level for search
(Response2$response - Response1$response) / (Spend2 - Spend1)

## Example of getting paid media exposure response curves
imps <- 50000000
response_imps <- robyn_response(
  robyn_object = robyn_object
  #, select_build = 1
  , media_metric = "app_S"
  , metric_value = imps)
response_imps$response / imps * 1000
response_imps$plot

## Example of getting organic media exposure response curves
sendings <- 30000
response_sending <- robyn_response(
  robyn_object = robyn_object
  #, select_build = 1
  , media_metric = "circular_S"
  , metric_value = sendings)
response_sending$response / sendings * 1000
response_sending$plot

################################################################
#### Optional: get old model results

# Get old hyperparameters and select model
# dt_hyper_fixed <- data.table::fread("~/Desktop/2022-03-31 12.32 rf4/pareto_hyperparameters.csv")
# select_model <- "1_25_9"
# dt_hyper_fixed <- dt_hyper_fixed[solID == select_model]

# OutputCollectFixed <- robyn_run(
#   # InputCollect must be provided by robyn_inputs with same dataset and parameters as before
#   InputCollect = InputCollect
#   , plot_folder = robyn_object
#   , dt_hyper_fixed = dt_hyper_fixed)
# 
# # Save Robyn object for further refresh
# robyn_save(robyn_object = robyn_object
#            , select_model = select_model
#            , InputCollect = InputCollect
#            , OutputCollect = OutputCollectFixed)
