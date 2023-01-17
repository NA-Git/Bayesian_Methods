---
title: "Robyn Test"
output: github_document
editor_options: 
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## STEP 0: Setup Environment

It's best to install and load in this order; it has yet to fail. The following two commands remove any previously installed H2O packages for R. Then we download packages H20 depends on.

```{r, echo=TRUE}
# pkgs <- c("RCurl","jsonlite")
# for (pkg in pkgs) {
#   if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
```

Downloading and initializing H20 for R.

```{r, echo=TRUE}
# install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-zygmund/4/R")
# install.packages("reticulate") # Install reticulate first if you haven't already
# remotes::install_github("facebookexperimental/Robyn/R")
# install.packages(c('nloptr', 'lares', 'dplyr', 'h2o', 'reticulate', 'tidyr'))
```

Calling the necessary libraries

```{r, echo=TRUE}
library(Robyn)
library(npreg)
library(dplyr)
library(nloptr)
library(lares)
library(tidyr)
library(stringr)
library(reticulate)
library(h2o)
```

Finally, initiate H20 and Nevergrad

```{r, echo=TRUE}
# h2o.init()
# conda_create("r-reticulate")
# conda_install("r-reticulate", "nevergrad", pip = TRUE)
# use_condaenv("r-reticulate")
```

## STEP 1: Data Creation

Ideal requirements:\
\* Two years' worth of data\
\* Consistent information on spend and impressions (FB prefers impressions over clicks)\
\* Several tactics with the associated information necessary.

Procedure: with a dataset such as the one described above, eliminate all but date, revenue, tactic category, tactic spend, and impressions or something similar, then create a pivot table that uses weekly dates as rows and has as columns a tactic category's spend and another for impressions. Revenue should also remain.

Robyn suggests a 10:1 ratio between observations and variables, so you'll likely have more tactics than you need and have to cull them down.

Robyn does not like nulls, nor too many zeroes, and if imputing missing values, you need to be very deliberate on how you do it. Multicollinearity is a concern, but less so with ridge regression, so you have a bit of wiggle room there. Just expect to have many of your models fail to converge before you start finding some that do.

## STEP 2: Load Data

This section is important not only to load the data, but it also tells the program where to place the Robyn object.

```{r, echo=TRUE}
setwd('C:/Users/norri/Desktop/')
Sys.setenv(R_FUTURE_FORK_ENABLE = TRUE) # Force multicore when using RStudio
options(future.fork.enable = TRUE)
df <- read.csv('robyn_final.csv', fileEncoding = 'UTF-8-BOM')

data("dt_prophet_holidays")
head(dt_prophet_holidays)
```

I've found it's best to set the full path to make sure this file is in the right spot and can be overwritten.

```{r, echo=TRUE}
robyn_object <- "C:/Users/norri/Desktop/MyRobyn.RDS"
```

## STEP 2a: Four steps of model specification

### 2a-1: First, specify the input variables.

All sign control are now automatically provided: "positive" for media & organic variables and "default" for all others. Users can still customize signs if necessary. Documentation is available in ?robyn_inputs.

```{r, echo=TRUE}
InputCollect <- robyn_inputs(
  dt_input = df,
  dt_holidays = dt_prophet_holidays,
  date_var = "DATE", # date format must be "2020-01-01, and must be in all caps"
  dep_var = "revenue", # there should be only one dependent variable
  dep_var_type = "revenue", # "revenue" (ROI) or "conversion" (CPA)
  prophet_vars = c("trend", "season", "holiday"), # "trend","season", "weekday" & "holiday"
  prophet_country = "US", # input one country. dt_prophet_holidays includes 59 countries by default
  context_vars = c("cargill", 'inflation'), # e.g. competitors, discount, unemployment etc
  paid_media_spends = c("banner_S", "blog_S", "coupon_S", "email_S"), # mandatory input
  paid_media_vars = c("banner_I", "blog_I", "coupon_I", "email_I"), # mandatory.
  # paid_media_vars must have same order as paid_media_spends. Use media exposure metrics like
  # impressions, GRP etc. If not applicable, use spend instead.
  # organic_vars = "newsletter", # marketing activity without media spend
  # factor_vars = c("events"), # force variables in context_vars or organic_vars to be categorical
  window_start = "2019-10-14",
  window_end = "2021-4-26",
  adstock = "weibull_pdf" # geometric, weibull_cdf or weibull_pdf.
)
print(InputCollect)
```

### 2a-2: Define and add hyperparameters

Robyn's hyperparameters have four components: Adstock parameters (theta or shape/scale). Saturation parameters (alpha/gamma). Regularisation parameter (lambda). No need to specify manually. Time series validation parameter (train_size).

```{r, echo=TRUE}
names = hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
limit = hyper_limits()

print(length(hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)))
alphas = c(.1, 9.9)
gammas = c(.1, 1)
shapes = c(0, 19.9)
scales = c(0, 1)

hyperparameters <-list(
  banner_S_alphas = c(2, 5)
  , banner_S_gammas = c(.2, .8)
  , banner_S_shapes = c(0, 15)
  , banner_S_scales = c(0.2, .7)
  
  , blog_S_alphas = c(2.5, 7.5)
  , blog_S_gammas = c(.25, .75)
  , blog_S_shapes = c(5, 15)
  , blog_S_scales = c(.2, .5)
  
  , coupon_S_alphas = c(2, 7.5)
  , coupon_S_gammas = c(.1, 1)
  , coupon_S_shapes = c(10, 19.9)
  , coupon_S_scales = c(0, 1)
  
  , email_S_alphas = c(2, 7.5)
  , email_S_gammas = c(.1, 1)
  , email_S_shapes = c(10, 19.9)
  , email_S_scales = c(0, 1)
  
  , train_size = c(0.5, 0.8)
)

```

### 2a-3: Put Hyperparameters into robyn_input()

```{r, echo=TRUE}
InputCollect <- robyn_inputs(InputCollect = InputCollect,
                             hyperparameters = hyperparameters)
```

### 2a-4: Optional model calibration

1.  Calibration channels need to be paid_media_spends or organic_vars names.
2.  We strongly recommend to use Weibull PDF adstock for more degree of freedom when calibrating Robyn.
3.  We strongly recommend to use experimental and causal results that are considered ground truth to calibrate MMM. Usual experiment types are identity-based (e.g. Facebook conversion lift) or geo-based (e.g. Facebook GeoLift). Due to the nature of treatment and control groups in an experiment, the result is considered immediate effect. It's rather impossible to hold off historical carryover effect in an experiment. Therefore, only calibrates the immediate and the future carryover effect. When calibrating with causal experiments, use calibration_scope = "immediate".
4.  It's controversial to use attribution/MTA contribution to calibrate MMM. Attribution is considered biased towards lower-funnel channels and strongly impacted by signal quality. When calibrating with MTA, use calibration_scope = "immediate".
5.  Every MMM is different. It's highly contextual if two MMMs are comparable or not. In case of using other MMM result to calibrate Robyn, use calibration_scope = "total".
6.  Currently, Robyn only accepts point-estimate as calibration input. For example, if 10k\$ spend is tested against a hold-out for channel A, then input the incremental return as point-estimate as the example below.
7.  The point-estimate has to always match the spend in the variable. For example, if channel A usually has \$100K weekly spend and the experimental holdout is 70%, input the point-estimate for the 30K, not the 70K.
8.  If an experiment contains more than one media variable, input "channel_A+channel_B" to indicate combination of channels, case sensitive.

```{r, echo=TRUE}
# calibration_input <- data.frame(
#   # channel name must in paid_media_vars
#   channel = c(),
#   # liftStartDate must be within input data range
#   liftStartDate = as.Date(c()),
#   # liftEndDate must be within input data range
#   liftEndDate = as.Date(c()),
#   # Provided value must be tested on same campaign level in model and same metric as dep_var_type
#   liftAbs = c(),
#   # Spend within experiment: should match within a 10% error your spend on date range for each channel from dt_input
#   spend = c(),
#   # Confidence: if frequentist experiment, you may use 1 - pvalue
#   confidence = c(),
#   # KPI measured: must match your dep_var
#   metric = c("revenue", "revenue", "revenue", "revenue"),
#   # Either "immediate" or "total". For experimental inputs like Facebook Lift, "immediate" is recommended.
#   calibration_scope = c("immediate", "immediate", "immediate", "immediate")
# )
# InputCollect <- robyn_inputs(InputCollect = InputCollect, calibration_input = calibration_input)
```

## STEP 3: Build Initial Model

Run all trials and iterations. Use ?robyn_run to check parameter definition

```{r, echo=TRUE}

OutputModels <- robyn_run(
  InputCollect = InputCollect # feed in all model specification
  , cores = NULL # on Windows, it will only use 1, multi-core on Linux
  , iterations = 2500 # Increase if failing to converge
  , trials = 10 # Increase if failing to converge
  # , outputs = FALSE # outputs = FALSE disables direct model output
  # , ts_validation = TRUE, # 3-way-split time series for NRMSE validation.
)
print(OutputModels)
```

```{r}
## Check time-series validation plot (when ts_validation == TRUE)
# Read more and replicate results: ?ts_validation
if (OutputModels$ts_validation) OutputModels$ts_validation_plot

## Check MOO (multi-objective optimization) convergence plots
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot
```

Calculate Pareto optimality, cluster and export results and plots. See ?robyn_outputs

```{r, echo=TRUE}

OutputCollect <- robyn_outputs(
  InputCollect, OutputModels
  , pareto_fronts = 'auto'
  , calibration_constraint = .1 #& default at 0.1
  , csv_out = "pareto" # "pareto" or "all"
  , clusters = TRUE # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  , plot_pareto = TRUE # Set to FALSE to deactivate plotting and saving model one-pagers
  , plot_folder = robyn_object # path for plots export
  , export = TRUE
)
print(OutputCollect)
```