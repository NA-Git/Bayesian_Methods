################################################################
#### Step 0: Setup environment
install.packages(c('Amelia', 'dplyr', 'tidyr'))

library('Amelia')
library('dplyr')
library('tidyr')

getwd()
setwd("G:/My Drive/IN/Data/Robyn/")

################################################################
#### Step 1: Create file for robyn_forecast.R without crashing
df <- read.csv('robyn_cortex_sparse.csv', fileEncoding = 'UTF-8-BOM')
imp <- amelia(df, m = 4, idvars = 'DATE', max.resample = 500)


imp1 <- imp$imputations$imp1
imp2 <- imp$imputations$imp2
imp3 <- imp$imputations$imp3
imp4 <- imp$imputations$imp4
imp5 <- imp$imputations$imp5

plot(imp, which.vars = (2:8))
compare.density(imp, var = "Digital_Equity")
overimpute(imp, var = "Digital_Equity")
disperse(imp, dims = 1, m = 5)
disperse(imp, dims = 2, m = 5)
missmap(imp)

df <- imp$imputations$imp1
summary(lm(Total.Sales ~ Digital_Equity + Coupons_Apps + Brand_Email + Banners
           + Influencers + E_Commerce + In_Store, data = df))

df <- imp$imputations$imp1
df_temp <- df$Date
df_temp <- as.POSIXct(df_temp,"%Y-%m-%d", tz = "UTC", origin="2019-10-01")
df <- df %>%
  select(c(Digital_Equity, Coupons_Apps, Brand_Email, Banners,
           Influencers, E_Commerce, In_Store, Total.Sales)) %>% abs()
df$Date <- df_temp

setwd("G:/My Drive/IN/Data/Robyn/")
write.csv(df, file = "df.csv", row.names = FALSE)
