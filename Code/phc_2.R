library(ggplot2)
library(plyr)
library(dplyr)
library(tibble)
library(nvmix)
library(readxl)
library(pracma)
library(fit.models)
library(sqldf)
library(writexl)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(readr)
library(data.table)
library(dygraphs)
library(xts)
library(lubridate)
library(plotly)
library(hrbrthemes)
library(forecast)
library(bnlearn)
library(R.utils)
library(RCurl)
library(latticeExtra)
library(ggeasy)
require(reshape2)
library(TSstudio)
library(ggeasy)
library(TTR)

#############################################################################
                        #### CONTAGION ANALYSIS ####

## Italian provinces ##
province_contagion <- read_excel("C:/Users/liry9/Desktop/Personalized healthcare/Datasets/Covid-19.xlsx", sheet = "Contagions by Italian province", col_names = TRUE)
province_contagion <- province_contagion[,-c(1:1)]
province_contagion <- transpose(province_contagion, keep.names = "col", make.names = "Province")
province_contagion[is.na(province_contagion)] <- 0
colnames(province_contagion)[1] <- "Date"

# Plot daily time series #
prov_cont <- melt(province_contagion, id.vars = "Date")

## Italian regions ##
ita_regions_contag <- read_excel("C:/Users/liry9/Desktop/Personalized healthcare/Datasets/Covid-19.xlsx", sheet = "Italian Regions contagions", col_names = TRUE)
ita_regions_contag[is.na(ita_regions_contag)] <- 0
ita_cont <- melt(ita_regions_contag, id.vars = "Date")

qplot(Date, value, data = ita_cont, geom = "line", group = variable) +
  facet_grid(variable ~ ., scales = "free", space = "fixed") +
  theme(strip.text.y = element_text(angle = 0))+
  easy_rotate_x_labels(angle = 45, side = "right")

## Comparison regions ##

comparison_contagion <- read_excel("C:/Users/liry9/Desktop/Personalized healthcare/Datasets/Covid-19.xlsx", sheet = "Comparison cities contagion", col_names = TRUE)
names <- c("Kenton County, Kentycky","Canberra (ACT)","Aichi prefecture","Fukuoka prefecture","Eastern cape (South Africa)","Rio grade do sul")
comparison_contagion[is.na(comparison_contagion)] <- 0

df_comp_cont <- melt(comparison_contagion, id.vars = "Date")
ggplot(df_comp_cont) + geom_line(aes(x = Date, y = value , colour = variable))

# Alternative 1

ts_plot(comparison_contagion,
        title = "Comparison countries contagion",
        Xtitle = "Time",
        Ytitle = names,
        type = "multiple")

# Alternative 2

qplot(Date, value, data = df_comp_cont, geom = "line", group = variable) +
  facet_grid(variable ~ ., scales = "free", space = "fixed") +
  theme(strip.text.y = element_text(angle = 0))+
  easy_rotate_x_labels(angle = 45, side = "right")


## Lombardy's time series ##
# Plot #
inds <- seq(as.Date("2020-01-31"), as.Date("2020-10-05"), by = "day")

lombardy <- ts(ita_regions_contag$Lombardia,
             start=c(01, as.numeric(format(inds[1],"%j"))),
             frequency=365)
lombardy
plot.ts(lombardy)

# Log plot #
log_lombardy <- log(lombardy)
plot.ts(log_lombardy)

# "SMA()" function to smooth time series data -> moving average
lombardy_smoothed <- SMA(lombardy,n=5)
plot.ts(lombardy_smoothed)

# Test if the Time series is stationary with
# the augmented Dickey-Fuller test: if p-value <0.05 then
# it will be stationary
library("tseries")
adf.test(lombardy) # non-stationary

# Making a time series stationary
nsdiffs(lombardy)
lombardy_seasdiff <- diff(lombardy, differences=1)
lombardy_seasdiff

plot(lombardy_seasdiff, type="l", main="Seasonally Differenced")



# Fit and ARIMA model and predict the future year
(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))

# To estimate the trend, seasonal and irregular components of this time seris
# https://robjhyndman.com/hyndsight/tslm-decomposition/
# We can approximate the seasonal pattern using Fourier terms with a few parameters.
lombardy_decompose <- tslm(lombardy ~ trend + fourier(lombardy, 2))
# then we can build the decomposition plot
lombardy_decompose

lombardy_trend <- coef(lombardy_decompose)[1] + coef(lombardy_decompose)['trend']*seq_along(lombardy)
lombardy_components <- cbind(
  data = lombardy,
  trend = lombardy_trend,
  season = lombardy - lombardy_trend - residuals(lombardy_decompose),
  remainder = residuals(lombardy_decompose)
)
autoplot(lombardy_components, facet=TRUE)

# Remove the seasonal component to get the seasonally adjusted data

adjust_lombardy <- lombardy - lombardy_components[,'season']
autoplot(lombardy, series="Data") +
  autolayer(adjust_lombardy, series="Seasonally adjusted")

# Autocorrelation and Partial Autocorrelation

acf_lombardy <- acf(lombardy)
pacf_lombardy <- pacf(lombardy)

# Cross correlation between 

## Comparison regions' time series ##
## Kenton
# Plot
kenton <- ts(comparison_contagion$`Kenton County, Kentucky`,
               start=c(01, as.numeric(format(inds[1],"%j"))),
               frequency=365)
plot.ts(kenton)

# "SMA()" function to smooth time series data -> moving average
kenton_smoothed <- SMA(kenton,n=5)
plot.ts(kenton_smoothed)

# To estimate the trend, seasonal and irregular components of this time seris
# https://robjhyndman.com/hyndsight/tslm-decomposition/
# We can approximate the seasonal pattern using Fourier terms with a few parameters.
kenton_decompose <- tslm(kenton ~ trend + fourier(kenton, 2))
# then we can build the decomposition plot
kenton_decompose

kenton_trend <- coef(kenton_decompose)[1] + coef(kenton_decompose)['trend']*seq_along(kenton)
kenton_components <- cbind(
  data = kenton,
  trend = kenton_trend,
  season = kenton - kenton_trend - residuals(kenton_decompose),
  remainder = residuals(kenton_decompose)
)
autoplot(kenton_components, facet=TRUE)

# Remove the seasonal component to get the seasonally adjusted data

adjust_kenton <- kenton - kenton_components[,'season']
autoplot(kenton, series="Data") +
  autolayer(adjust_kenton, series="Seasonally adjusted")

## Canberra (ACT)
# Plot
act <- ts(comparison_contagion$`Canberra (ACT)`,
             start=c(01, as.numeric(format(inds[1],"%j"))),
             frequency=365)
plot.ts(act)

# "SMA()" function to smooth time series data -> moving average
act_smoothed <- SMA(act,n=5)
plot.ts(act_smoothed)

# To estimate the trend, seasonal and irregular components of this time series
# we can approximate the seasonal pattern using Fourier terms with a few parameters.
act_decompose <- tslm(act ~ trend + fourier(act, 2))
# then we can build the decomposition plot
act_decompose

act_trend <- coef(act_decompose)[1] + coef(act_decompose)['trend']*seq_along(act)
act_components <- cbind(
  data = act,
  trend = act_trend,
  season = act - act_trend - residuals(act_decompose),
  remainder = residuals(act_decompose)
)
autoplot(act_components, facet=TRUE)
adjust_act <- act - act_components[,'season']
autoplot(act, series="Data") +
  autolayer(adjust_act, series="Seasonally adjusted")

# Remove the seasonal component to get the seasonally adjusted data

adjust_act <- act - act_components[,'season']
autoplot(act, series="Data") +
  autolayer(adjust_act, series="Seasonally adjusted")

## Aichi
# Plot
aichi <- ts(comparison_contagion$`Aichi prefecture`,
             start=c(01, as.numeric(format(inds[1],"%j"))),
             frequency=365)
plot.ts(aichi)

# "SMA()" function to smooth time series data -> moving average
aichi_smoothed <- SMA(aichi,n=5)
plot.ts(aichi_smoothed)

# To estimate the trend, seasonal and irregular components of this time seris
# we can approximate the seasonal pattern using Fourier terms with a few parameters.
aichi_decompose <- tslm(aichi ~ trend + fourier(aichi, 2))
# then we can build the decomposition plot
aichi_decompose

aichi_trend <- coef(aichi_decompose)[1] + coef(aichi_decompose)['trend']*seq_along(aichi)
aichi_components <- cbind(
  data = aichi,
  trend = aichi_trend,
  season = aichi - aichi_trend - residuals(aichi_decompose),
  remainder = residuals(aichi_decompose)
)
autoplot(aichi_components, facet=TRUE)

# Remove the seasonal component to get the seasonally adjusted data

adjust_aichi <- aichi - aichi_components[,'season']
autoplot(aichi, series="Data") +
  autolayer(adjust_aichi, series="Seasonally adjusted")

## Fukuoka
# Plot
fukuoka <- ts(comparison_contagion$`Fukuoka prefecture`,
            start=c(01, as.numeric(format(inds[1],"%j"))),
            frequency=365)
plot.ts(fukuoka)

# "SMA()" function to smooth time series data -> moving average
fukuoka_smoothed <- SMA(fukuoka,n=5)
plot.ts(fukuoka_smoothed)

# To estimate the trend, seasonal and irregular components of this time seris
# we can approximate the seasonal pattern using Fourier terms with a few parameters.
fukuoka_decompose <- tslm(fukuoka ~ trend + fourier(fukuoka, 2))
# then we can build the decomposition plot
fukuoka_decompose

fukuoka_trend <- coef(fukuoka_decompose)[1] + coef(fukuoka_decompose)['trend']*seq_along(fukuoka)
fukuoka_components <- cbind(
  data = fukuoka,
  trend = fukuoka_trend,
  season = fukuoka - fukuoka_trend - residuals(fukuoka_decompose),
  remainder = residuals(fukuoka_decompose)
)
autoplot(fukuoka_components, facet=TRUE)

# Remove the seasonal component to get the seasonally adjusted data

adjust_fukuoka <- fukuoka - fukuoka_components[,'season']
autoplot(fukuoka, series="Data") +
  autolayer(adjust_fukuoka, series="Seasonally adjusted")

## Eastern Cape (South Africa)
# Plot
eastern <- ts(comparison_contagion$`Eastern cape (South Africa)`,
              start=c(01, as.numeric(format(inds[1],"%j"))),
              frequency=365)
plot.ts(eastern)

# "SMA()" function to smooth time series data -> moving average
eastern_smoothed <- SMA(eastern,n=5)
plot.ts(eastern_smoothed)

# To estimate the trend, seasonal and irregular components of this time seris
# we can approximate the seasonal pattern using Fourier terms with a few parameters.
eastern_decompose <- tslm(eastern ~ trend + fourier(eastern, 2))
# then we can build the decomposition plot
eastern_decompose

eastern_trend <- coef(eastern_decompose)[1] + coef(eastern_decompose)['trend']*seq_along(eastern)
eastern_components <- cbind(
  data = eastern,
  trend = eastern_trend,
  season = eastern - eastern_trend - residuals(eastern_decompose),
  remainder = residuals(eastern_decompose)
)
autoplot(eastern_components, facet=TRUE)

# Remove the seasonal component to get the seasonally adjusted data

adjust_eastern <- eastern - eastern_components[,'season']
autoplot(eastern, series="Data") +
  autolayer(adjust_eastern, series="Seasonally adjusted")

## Rio Grande do Sul
# Plot
rio <- ts(comparison_contagion$`Rio grade do sul`,
              start=c(01, as.numeric(format(inds[1],"%j"))),
              frequency=365)
plot.ts(rio)

# Log plot
log_rio <- log(rio)
plot.ts(log_rio)

# "SMA()" function to smooth time series data -> moving average
rio_smoothed <- SMA(rio,n=5)
plot.ts(rio_smoothed)

# To estimate the trend, seasonal and irregular components of this time seris
# we can approximate the seasonal pattern using Fourier terms with a few parameters.
rio_decompose <- tslm(rio ~ trend + fourier(rio, 2))
# then we can build the decomposition plot
rio_decompose

rio_trend <- coef(rio_decompose)[1] + coef(rio_decompose)['trend']*seq_along(rio)
rio_components <- cbind(
  data = rio,
  trend = rio_trend,
  season = rio - rio_trend - residuals(rio_decompose),
  remainder = residuals(rio_decompose)
)
autoplot(rio_components, facet=TRUE)

# Remove the seasonal component to get the seasonally adjusted data

adjust_rio <- rio - rio_components[,'season']
autoplot(rio, series="Data") +
  autolayer(adjust_rio, series="Seasonally adjusted")

#############################################################################
                      #### Fatality Rates ####

## Italian regions ##

ita_regions_fat <- read_excel("C:/Users/liry9/Desktop/Personalized healthcare/Datasets/Covid-19.xlsx", sheet = "Italian Regions fatality", col_names = TRUE)
ita_regions_fat[is.na(ita_regions_fat)] <- 0
ita_fat <- melt(ita_regions_fat, id.vars = "Date")

qplot(Date, value, data = ita_fat, geom = "line", group = variable) +
  facet_grid(variable ~ ., scales = "free", space = "fixed") +
  theme(strip.text.y = element_text(angle = 0))+
  easy_rotate_x_labels(angle = 45, side = "right")

## Comparison regions ##

comparison_fat <- read_excel("C:/Users/liry9/Desktop/Personalized healthcare/Datasets/Covid-19.xlsx", sheet = "Comparison regions fatalities", col_names = TRUE)
comparison_fat[is.na(comparison_fat)] <- 0
df_comp_fat <- melt(comparison_fat, id.vars = "Date")

qplot(Date, value, data = df_comp_fat, geom = "line", group = variable) +
  facet_grid(variable ~ ., scales = "free", space = "fixed") +
  theme(strip.text.y = element_text(angle = 0))+
  easy_rotate_x_labels(angle = 45, side = "right")

## Lombardy's time series ##
# Plot #
lombardy_fat <- ts(ita_regions_fat$Lombardia,
               start=c(01, as.numeric(format(inds[1],"%j"))),
               frequency=365)
lombardy_fat
plot.ts(lombardy_fat)

# To estimate the trend, seasonal and irregular components of this time seris
# https://robjhyndman.com/hyndsight/tslm-decomposition/
# We can approximate the seasonal pattern using Fourier terms with a few parameters.
lombardy_fat_decompose <- tslm(lombardy_fat ~ trend + fourier(lombardy_fat, 2))
# then we can build the decomposition plot
lombardy_fat_decompose

lombardy_fat_trend <- coef(lombardy_fat_decompose)[1] + coef(lombardy_fat_decompose)['trend']*seq_along(lombardy_fat)
lombardy_fat_components <- cbind(
  data = lombardy_fat,
  trend = lombardy_fat_trend,
  season = lombardy_fat - lombardy_fat_trend - residuals(lombardy_fat_decompose),
  remainder = residuals(lombardy_fat_decompose)
)
autoplot(lombardy_fat_components, facet=TRUE)

ccf_lombardy <- ccf(lombardy, lombardy_fat, ylab = "cross-correlation") # computes cross correlation between 2 timeseries.
head(ccf_lombardy[[1]])

#### Time series of the italian regions ####

#### Time series of the comparison regions ####

#### Regression ####

