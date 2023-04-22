## STEP 0: Setup Environment

#It's best to install and load in this order; it has yet to fail. The following two commands remove any previously installed H2O #packages for R. Then we download packages H20 depends on.

pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

#Downloading and initializing H20 for R.

install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-zygmund/4/R")
install.packages("reticulate") # Install reticulate first if you haven't already
remotes::install_github("facebookexperimental/Robyn/R")
install.packages(c('nloptr', 'lares', 'dplyr', 'h2o', 'reticulate', 'tidyr'))

#Calling the necessary libraries

library(Robyn)
library(npreg)
library(dplyr)
library(nloptr)
library(lares)
library(tidyr)
library(stringr)
library(reticulate)
library(h2o)

#Finally, initiate H20 and Nevergrad

h2o.init()
conda_create("r-reticulate")
conda_install("r-reticulate", "nevergrad", pip = TRUE)
use_condaenv("r-reticulate")

## STEP 1: Data Creation

#Ideal requirements:\
# Two years' worth of data\
# Consistent information on spend and impressions (FB prefers impressions over clicks)\
# Several tactics with the associated information necessary.

#Procedure: with a dataset such as the one described above, eliminate all but date, revenue, tactic category, tactic spend, and #impressions or something similar, then create a pivot table that uses weekly dates as rows and has as columns a tactic category's #spend and another for impressions. Revenue should also remain.
#Robyn suggests a 10:1 ratio between observations and variables, so you'll likely have more tactics than you need and have to cull #them down.
#Robyn does not like nulls, nor too many zeroes, and if imputing missing values, you need to be very deliberate on how you do it. #Multicollinearity is a concern, but less so with ridge regression, so you have a bit of wiggle room there. Just expect to have #many of your models fail to converge before you start finding some that do.

## STEP 2: Load Data

#This section is important not only to load the data, but it also tells the program where to place the Robyn object.

setwd('G:/My Drive/')
Sys.setenv(R_FUTURE_FORK_ENABLE = TRUE) # Force multicore when using RStudio

create_files = TRUE
options(future.fork.enable = TRUE)
df <- read.csv('eo_context_4.csv', fileEncoding = 'UTF-8-BOM')

data("dt_prophet_holidays")

# This adds Prime Days into the holiday dataframe
dt_prophet_holidays[nrow(dt_prophet_holidays) + 1,] <- list(as.Date('2018-07-16'), 'Prime Day', 'US', 2018)
dt_prophet_holidays[nrow(dt_prophet_holidays) + 1,] <- list(as.Date('2019-07-15'), 'Prime Day', 'US', 2019)
dt_prophet_holidays[nrow(dt_prophet_holidays) + 1,] <- list(as.Date('2020-10-13'), 'Prime Day', 'US', 2020)
dt_prophet_holidays[nrow(dt_prophet_holidays) + 1,] <- list(as.Date('2021-06-21'), 'Prime Day', 'US', 2021)
dt_prophet_holidays[nrow(dt_prophet_holidays) + 1,] <- list(as.Date('2022-07-12'), 'Prime Day', 'US', 2022)

#I've found it's best to set the full path to make sure this file is in the right spot and can be overwritten.

# robyn_object <- "G:/My Drive/MyRobyn.RDS"
robyn_directory <- ('G:/My Drive')

## STEP 2a: Four steps of model specification
### 2a-1: First, specify the input variables.
#All sign control are now automatically provided: "positive" for media & organic variables and "default" for all others. Users can #still customize signs if necessary. Documentation is available in ?robyn_inputs.

InputCollect <- robyn_inputs(
  dt_input = df,
  dt_holidays = dt_prophet_holidays,
  date_var = "DATE", # date format must be "2020-01-01, and must be in all caps"
  dep_var = "revenue", # there should be only one dependent variable
  dep_var_type = "revenue", # "revenue" (ROI) or "conversion" (CPA)
  prophet_vars = c("trend", "season", "holiday"), # "trend","season", "weekday" & "holiday"
  prophet_country = "US", # input one country. dt_prophet_holidays includes 59 countries by default
  # context_vars = c('deaths'), # e.g. competitors, discount, unemployment etc
  paid_media_spends = c("display_S", "b_branded_S", "b_category_S",
                        "p_auto_S", "p_brand_S", "p_category_S", "p_competitive_S"), # mandatory input
  paid_media_vars = c("display_S", "b_branded_S", "b_category_S",
                        "p_auto_S", "p_brand_S", "p_category_S", "p_competitive_S"), # mandatory.
  # paid_media_vars must have same order as paid_media_spends. Use media exposure metrics like
  # impressions, GRP etc. If not applicable, use spend instead.
  # organic_vars = "newsletter", # marketing activity without media spend
  # factor_vars = c("lockdown"), # force variables in context_vars or organic_vars to be categorical
  window_start = "2019-09-15",
  window_end = "2022-12-25",
  adstock = "weibull_pdf" # geometric, weibull_cdf or weibull_pdf.
)
print(InputCollect)


### 2a-2: Define and add hyperparameters
#Robyn's hyperparameters have four components: Adstock parameters (theta or shape/scale). Saturation parameters (alpha/gamma). #Regularisation parameter (lambda). No need to specify manually. Time series validation parameter (train_size).

names <- hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
limit <- hyper_limits()

print(length(hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)))
alphas <- c(.1, 9.9)
gammas <- c(.1, 1)
shapes <- c(0, 19.9)
scales <- c(0, 1)

hyperparameters <-list(
  display_S_alphas = c(.1, 9.9)
  , display_S_gammas = c(.1, 1)
  , display_S_shapes = c(0, 19.9)
  , display_S_scales = c(0, 1)
  
  , b_branded_S_alphas = c(.1, 9.9)
  , b_branded_S_gammas = c(.1, 1)
  , b_branded_S_shapes = c(0, 19.9)
  , b_branded_S_scales = c(0, 1)
  
  , b_category_S_alphas = c(.1, 9.9)
  , b_category_S_gammas = c(.1, 1)
  , b_category_S_shapes = c(0, 19.9)
  , b_category_S_scales = c(0, 1)
  
  , p_auto_S_alphas = c(.1, 9.9)
  , p_auto_S_gammas = c(.1, 1)
  , p_auto_S_shapes = c(0, 19.9)
  , p_auto_S_scales = c(0, 1)
  
  , p_brand_S_alphas = c(.1, 9.9)
  , p_brand_S_gammas = c(.1, 1)
  , p_brand_S_shapes = c(0, 19.9)
  , p_brand_S_scales = c(0, 1)
  
  , p_category_S_alphas = c(.1, 9.9)
  , p_category_S_gammas = c(.1, 1)
  , p_category_S_shapes = c(0, 19.9)
  , p_category_S_scales = c(0, 1)
  
  , p_competitive_S_alphas = c(.1, 9.9)
  , p_competitive_S_gammas = c(.1, 1)
  , p_competitive_S_shapes = c(0, 19.9)
  , p_competitive_S_scales = c(0, 1)
  
  # , email_S_alphas = c(.1, 9.9)
  # , email_S_gammas = c(.1, 1)
  # , email_S_shapes = c(0, 19.9)
  # , email_S_scales = c(0, 1)
  
  , train_size = c(0.5, 0.8)
)

### 2a-3: Put Hyperparameters into robyn_input()

InputCollect <- robyn_inputs(InputCollect = InputCollect,
                             hyperparameters = hyperparameters)

### 2a-4: Optional model calibration

# 1.  Calibration channels need to be paid_media_spends or organic_vars names.
# 2.  We strongly recommend to use Weibull PDF adstock for more degree of freedom when calibrating Robyn.
# 3.  We strongly recommend to use experimental and causal results that are considered ground truth to calibrate MMM.
#     Usual #experiment types are identity-based (e.g. Facebook conversion lift) or geo-based (e.g. Facebook GeoLift).
#     Due to the nature of #treatment and control groups in an experiment, the result is considered immediate effect.
#     It's rather impossible to hold off #historical carryover effect in an experiment. Therefore, only calibrates the
#     immediate and the future carryover effect. When #calibrating with causal experiments,
#     use calibration_scope = "immediate".
# 4.  It's controversial to use attribution/MTA contribution to calibrate MMM. Attribution is considered biased towards
#     lower-funnel #channels and strongly impacted by signal quality. When calibrating with MTA,
#     use calibration_scope = "immediate".
# 5.  Every MMM is different. It's highly contextual if two MMMs are comparable or not. In case of using other
#     MMM result to #calibrate Robyn, use calibration_scope = "total".
# 6.  Currently, Robyn only accepts point-estimate as calibration input. For example, if 10k\$ spend is tested against
#     a hold-out #for channel A, then input the incremental return as point-estimate as the example below.
# 7.  The point-estimate has to always match the spend in the variable. For example, if channel A usually
#     has \$100K weekly spend #and the experimental holdout is 70%, input the point-estimate for the 30K, not the 70K.
# 8.  If an experiment contains more than one media variable, input "channel_A+channel_B" to indicate combination
#     of channels, case #sensitive.

# calibration_input <- data.frame(
#   # channel name must in paid_media_vars
#   channel = c("display_S", "b_branded_S", "b_category_S",
#                         "p_auto_S", "p_brand_S", "p_category_S", "p_competitive_S"),
# #   # liftStartDate must be within input data range
#   liftStartDate = as.Date(c("2022-09-04", "2022-09-04", "2022-09-04", "2022-09-04", "2022-09-04",
#                             "2022-09-04", "2022-09-04")),
# #   # liftEndDate must be within input data range
#   liftEndDate = as.Date(c("2022-11-06", "2022-11-06", "2022-11-06", "2022-11-06", "2022-11-06",
#                           "2022-11-06", "2022-11-06")),
# #   # Provided value must be tested on same campaign level in model and same metric as dep_var_type
#   liftAbs = c(76754, 76754, 76754, 76754, 76754, 76754, 76754),
# #   # Spend within experiment: should match within a 10% error your spend on date range for each channel from dt_input
#   spend = c(42908, 899, 9002, 1527, 11216, 72120, 2706),
# #   # Confidence: if frequentist experiment, you may use 1 - pvalue
#   confidence = c(0.85, 0.8, 0.99, 0.95, .90, .99, .85),
# #   # KPI measured: must match your dep_var
#   metric = c("revenue", "revenue", "revenue", "revenue", "revenue", "revenue", "revenue"),
# #   # Either "immediate" or "total". For experimental inputs like Facebook Lift, "immediate" is recommended.
#   calibration_scope = c("immediate", "immediate", "immediate", "immediate", "immediate", "immediate", "immediate")
# )
# InputCollect <- robyn_inputs(InputCollect = InputCollect, calibration_input = calibration_input)

# InputCollect <- robyn_inputs(
#   dt_input = df,
#   dt_holidays = dt_prophet_holidays,
#   date_var = "DATE", # date format must be "2020-01-01, and must be in all caps"
#   dep_var = "revenue", # there should be only one dependent variable
#   dep_var_type = "revenue", # "revenue" (ROI) or "conversion" (CPA)
#   prophet_vars = c("trend", "season", "holiday"), # "trend","season", "weekday" & "holiday"
#   prophet_country = "US", # input one country. dt_prophet_holidays includes 59 countries by default
#   context_vars = c("deaths", 'hospital', "positive", 'lockdown', 'inflation', 'gdp'), # e.g. competitors, discount, unemployment 
#   paid_media_spends = c("display_S", "b_branded_S", "b_category_S",
#                         "p_auto_S", "p_brand_S", "p_category_S", "p_competitive_S"), # mandatory input
#   paid_media_vars = c("display_I", "b_branded_I", "b_category_I",
#                         "p_auto_I", "p_brand_I", "p_category_I", "p_competitive_I"), # mandatory.
#   # paid_media_vars must have same order as paid_media_spends. Use media exposure metrics like
#   # impressions, GRP etc. If not applicable, use spend instead.
#   # organic_vars = "newsletter", # marketing activity without media spend
#   # factor_vars = c("events"), # force variables in context_vars or organic_vars to be categorical
#   window_start = "2019-09-15",
#   window_end = "2022-12-25",
#   adstock = "weibull_pdf" # geometric, weibull_cdf or weibull_pdf.
#   ,hyperparameters = hyperparameters # as in 2a-2 above
#   ,calibration_input = calibration_input # as in 2a-4 above
# )

# if (length(InputCollect$exposure_vars) > 0) {
#   lapply(InputCollect$modNLS$plots, plot)
# }

## STEP 3: Build Initial Model
#Run all trials and iterations. Use ?robyn_run to check parameter definition

OutputModels <- robyn_run(
  InputCollect = InputCollect # feed in all model specification
  , cores = NULL # on Windows, it will only use 1, multi-core on Linux
  , iterations = 2500 # Increase if failing to converge
  , trials = 14 # Increase if failing to converge
  , ts_validation = FALSE, # 3-way-split time series for NRMSE validation.
)
print(OutputModels)

## Check time-series validation plot (when ts_validation == TRUE)
# Read more and replicate results: ?ts_validation
if (OutputModels$ts_validation) OutputModels$ts_validation_plot

## Check MOO (multi-objective optimization) convergence plots
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

#Calculate Pareto optimality, cluster and export results and plots. See ?robyn_outputs

OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = "auto", # automatically pick how many pareto-fronts to fill min_candidates (100)
  # min_candidates = 100, # top pareto models for clustering. Default to 100
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  export = create_files, # this will create files locally
  plot_folder = robyn_directory, # path for plots exports and files creation
  plot_pareto = create_files # Set to FALSE to deactivate plotting and saving model one-pagers
)
print(OutputCollect)

# STEP 4: Select and save model of choice

print(OutputCollect)
select_model <- "4_157_7" # Pick one of the models from OutputCollect to proceed

#### Version >=3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model, export = create_files)
print(ExportedModel)

myOnePager <- robyn_onepagers(InputCollect, OutputCollect, select_model, export = FALSE)

# To check each of the one-pager's plots
myOnePager[[select_model]]$patches$plots[[1]]
myOnePager[[select_model]]$patches$plots[[2]]
myOnePager[[select_model]]$patches$plots[[3]]
myOnePager[[select_model]]$patches$plots[[4]]
myOnePager[[select_model]]$patches$plots[[5]]
myOnePager[[select_model]]$patches$plots[[6]]

# STEP 5: Get budget allocation from model above

#Budget allocation result requires further validation. Please use this recommendation with caution. Don't interpret
# budget #allocation result if selected model above doesn't meet business expectation. Look at any and all suggestions
# to see if the #recomendations make sense; On occasions, they suggest dropping spend on the holidays and even no ad
# spend Check media summary for #selected model Run ?robyn_allocator to check parameter definition Run the
# "max_historical_response" scenario: "What's the revenue #lift potential with the same historical spend level
# and what is the spend mix?"

# Scenario "max_response": "What's the max. return given certain spend?"
# Example 1: max_response default setting: maximize response for latest month

InputCollect$paid_media_spends

AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  # date_range = NULL, # Default last month as initial period
  # total_budget = NULL, # When NULL, default is total spend in date_range
  channel_constr_low = 0.7,
  channel_constr_up = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
  # channel_constr_multiplier = 3,
  scenario = "max_response",
  export = create_files
)
# Print & plot allocator's output
print(AllocatorCollect1)
plot(AllocatorCollect1)

#Run the "max_response_expected_spend" scenario: "What's the maximum response for a given total spend based on
# historical #saturation and the spend mix?" "optmSpendShareUnit" is the optimum spend share.

InputCollect$paid_media_spends

# Example 2: maximize response for latest 10 periods with given spend
AllocatorCollect2 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "last_10", # Last 10 periods, same as c("2018-10-22", "2018-12-31")
  total_budget = 5000000, # Total budget for date_range period simulation
  channel_constr_low = .7,
  channel_constr_up = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
  channel_constr_multiplier = 3, # Customise bound extension for wider insights
  scenario = "max_response",
  export = create_files
)
print(AllocatorCollect2)
plot(AllocatorCollect2)

# Scenario "target_efficiency": "How much to spend to hit ROAS or CPA of x?"
# Example 3: Use default ROAS target for revenue or CPA target for conversion
# Check InputCollect$dep_var_type for revenue or conversion type
# Two default ROAS targets: 0.8x of initial ROAS as well as ROAS = 1
# Two default CPA targets: 1.2x and 2.4x of the initial CPA

AllocatorCollect3 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  # date_range = NULL, # Default last month as initial period
  scenario = "target_efficiency",
  # target_value = 2, # Customize target ROAS or CPA value
  export = create_files
)
print(AllocatorCollect3)
plot(AllocatorCollect3)