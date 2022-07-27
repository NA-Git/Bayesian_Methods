################################################################
#### Step 0: Setup environment

## Install, load, and check (latest) version
remotes::install_github("facebookexperimental/Robyn/R")
install.packages(c('Amelia','nloptr','lares','Robyn','dplyr','reticulate','tidyr'))
install.packages("reticulate") # Install reticulate first if you haven't already

library('Amelia', 'npreg', 'dplyr', 'nloptr',
        'lares', 'Robyn', 'tidyr')
library('reticulate', 'h2o')

getwd()
setwd("G:/My Drive/To_Do/IN_DS/Robyn")

################################################################
#### Step 1: Create Road Side Assistance
df <- read.csv('tyson_cleaned_7_26_clean_3.csv')
imp <- amelia(df, m=5, idvars = 'Date', max.resample = 1000)

plot(imp, which.vars = (2:8))
compare.density(imp, var = "Digital_Equity")
overimpute(imp, var = "Digital_Equity")
disperse(imp, dims = 1, m = 5)
disperse(imp, dims = 2, m = 5)
missmap(imp)

df <- imp$imputations$imp2
summary(lm(Total.Sales ~ Digital_Equity + Coupons_Apps + Brand_Email + Banners
           + Influencers + E_Commerce + In_Store, data = df))

df <- imp$imputations$imp2
# df_temp <- df

df <- df %>%
  select(c(Date, Digital_Equity, Coupons_Apps, Brand_Email, Banners, Influencers,
           E_Commerce, In_Store, Total.Sales)) %>%
  abs()
# df <- df_temp
setwd("G:/My Drive/To_Do/IN_DS/Robyn")
write.csv(df, file = "df1.csv", row.names = FALSE)
