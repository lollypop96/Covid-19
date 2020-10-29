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
library(tseries)
library(fUnitRoots)
library(urca)
library(lmtest)
library(FitAR)
library(forecast)

######################################################################################
######################## CONTAGION ANALYSIS ##########################################
######################################################################################
######################################################################################
############################ Italian provinces #######################################
######################################################################################
province_contagion <- read_excel("C:/Users/liry9/Desktop/Personalized healthcare/Datasets/Covid-19.xlsx", sheet = "Contagions by Italian province", col_names = TRUE)
province_contagion <- province_contagion[,-c(1:1)]
province_contagion <- transpose(province_contagion, keep.names = "col", make.names = "Province")
province_contagion[is.na(province_contagion)] <- 0
colnames(province_contagion)[1] <- "Date"

## Plot daily time series
prov_cont <- melt(province_contagion, id.vars = "Date")

######################################################################################
################################ Italian regions #####################################
######################################################################################
rm(list = ls())
ita_regions_contag <- read_excel("C:/Users/liry9/Desktop/Personalized healthcare/Datasets/Covid-19.xlsx", sheet = "Italian Regions contagions", col_names = TRUE)
ita_regions_contag[is.na(ita_regions_contag)] <- 0
ita_regions_contag <- ita_regions_contag[,-(7:8)] 
ita_cont <- melt(ita_regions_contag, id.vars = "Date")

qplot(Date, value, data = ita_cont, geom = "line", group = variable) +
  facet_grid(variable ~ ., scales = "free", space = "fixed") +
  theme(strip.text.y = element_text(angle = 0))+
  easy_rotate_x_labels(angle = 45, side = "right")

######################################################################################
################################ Comparison regions ##################################
######################################################################################
comparison_contagion <- read_excel("C:/Users/liry9/Desktop/Personalized healthcare/Datasets/Covid-19.xlsx", sheet = "Comparison cities contagion", col_names = TRUE)
names <- c("Kenton County, Kentycky","Canberra (ACT)","Aichi prefecture","Fukuoka prefecture","Eastern cape (South Africa)","Rio grade do sul")
comparison_contagion[is.na(comparison_contagion)] <- 0

df_comp_cont <- melt(comparison_contagion, id.vars = "Date")
ggplot(df_comp_cont) + geom_line(aes(x = Date, y = value , colour = variable))

### Alternative 1 ###

ts_plot(comparison_contagion,
        title = "Comparison countries contagion",
        Xtitle = "Time",
        Ytitle = names,
        type = "multiple")

### Alternative 2 ###

qplot(Date, value, data = df_comp_cont, geom = "line", group = variable) +
  facet_grid(variable ~ ., scales = "free", space = "fixed") +
  theme(strip.text.y = element_text(angle = 0))+
  easy_rotate_x_labels(angle = 45, side = "right")

######################################################################################
######################################################################################
                        ### Lombardy's time series ###
## Plot
inds <- seq(as.Date("2020-01-31"), as.Date("2020-10-05"), by = "day")

lombardy <- ts(ita_regions_contag$Lombardia,
             start=c(01, as.numeric(format(inds[1],"%j"))),
             frequency=365)
lombardy
plot.ts(lombardy)

## Log plot
log_lombardy <- log(lombardy)
plot.ts(log_lombardy)

## "SMA()" function to smooth time series data -> moving average
lombardy_smoothed <- SMA(lombardy,n=5)
plot.ts(lombardy_smoothed)

## Test if the Time series is stationary with
# the augmented Dickey-Fuller test: if p-value <0.05 then
# it will be stationary
adf.test(lombardy) # non-stationary

## Unit root test - This test is used to find out that first difference
# or regression which should be used on the trending data to make it
# stationary. In Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test, small
# p-values suggest differencing is required.
#https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/#:~:text=arima()%20function%20in%20R,chosen%20by%20minimizing%20the%20AICc.
# Courtesy of https://stackoverflow.com/questions/59936691/kpss-test-returns-na
my_urkpssTest <- function (x, type, lags, use.lag, doplot) {
  x <- as.vector(x)
  urca <- urca::ur.kpss(x, type = type[1], lags = lags[1], use.lag = use.lag)
  output = capture.output(urca::summary(urca))[-(1:4)]
  output = output[-length(output)]
  for (i in 1:length(output)) output[i] = paste(" ", output[i])
  ans = list(name = "ur.kpss", test = urca, output = output)
  if (doplot) 
    plot(urca)
  new("fHTEST", call = match.call(), data = list(x = x), 
      test = ans, title = "KPSS Unit Root Test", description = description())
}

my_urkpssTest(lombardy, type = c("tau"), lags = c("short"),
              use.lag = NULL, doplot = TRUE)

## Making a time series stationary
nsdiffs(lombardy)
lombardy_seasdiff <- diff(lombardy, differences=1)
lombardy_seasdiff

plot(lombardy_seasdiff, type="l", main="Seasonally Differenced")
adf.test(lombardy_seasdiff) # now it is stationary since the p_value is smaller

acf_lombardy <- acf(lombardy_seasdiff)
pacf_lombardy <- pacf(lombardy_seasdiff)

## Fit and ARIMA model
class(lombardy_seasdiff)
fit_lombardy1 <- arima(lombardy_seasdiff, c(2, 1, 2),seasonal = list(order = c(0, 1, 1), period = 12))
fit_lombardy1
coeftest(fit_lombardy1) 
confint(fit_lombardy1)

## Choosing the best model
acf(fit_lombardy1$residuals)
boxresultLombardy <- LjungBoxTest(fit_lombardy1$residuals, k=2, StartLag=1)
plot(boxresultLombardy[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fit_lombardy1$residuals)
qqline(fit_lombardy1$residuals)

# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/#:~:text=arima()%20function%3A,functions%3A%20ets()%20and%20auto.&text=arima()%20function%20in%20R,algorithm%20for%20automatic%20ARIMA%20modeling.
lombardy_arima<-auto.arima(lombardy_seasdiff, trace=TRUE)

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

## Remove the seasonal component to get the seasonally adjusted data
adjust_lombardy <- lombardy - lombardy_components[,'season']

autoplot(lombardy, series="Data") + autolayer(adjust_lombardy, series="Seasonally adjusted")

##########################################################################
#################### Comparison regions' time series #####################
                            ### Kenton County ### 
## Plot
kenton <- ts(comparison_contagion$`Kenton County, Kentucky`,
               start=c(01, as.numeric(format(inds[1],"%j"))),
               frequency=365)
plot.ts(kenton)

## "SMA()" function to smooth time series data -> moving average
kenton_smoothed <- SMA(kenton,n=5)
plot.ts(kenton_smoothed)

## Test if the Time series is stationary with
# the augmented Dickey-Fuller test: if p-value <0.05 then
# it will be stationary
adf.test(kenton) # non-stationary

## Unit root test
my_urkpssTest(kenton, type = c("tau"), lags = c("short"),
              use.lag = NULL, doplot = TRUE)

## Making a time series stationary
nsdiffs(kenton)
kenton_seasdiff <- diff(kenton, differences=1)
kenton_seasdiff

plot(kenton_seasdiff, type="l", main="Seasonally Differenced")
adf.test(kenton_seasdiff) # now it is stationary since the p_value is smaller

acf_kenton <- acf(kenton_seasdiff)
pacf_kenton <- pacf(kenton_seasdiff)

## Fit and ARIMA model
class(kenton_seasdiff)
fit_kenton1 <- arima(kenton_seasdiff, c(2, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
fit_kenton1
coeftest(fit_kenton1) 
confint(fit_kenton1)

## Choosing the best model
acf(fit_kenton1$residuals)
boxresultKenton <- LjungBoxTest(fit_kenton1$residuals, k=2, StartLag=1)
plot(boxresultKenton[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fit_kenton1$residuals)
qqline(fit_kenton1$residuals)

# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/#:~:text=arima()%20function%3A,functions%3A%20ets()%20and%20auto.&text=arima()%20function%20in%20R,algorithm%20for%20automatic%20ARIMA%20modeling.
auto.arima(kenton_seasdiff, trace=TRUE)

## To estimate the trend, seasonal and irregular components of this time seris
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

## Remove the seasonal component to get the seasonally adjusted data
adjust_kenton <- kenton - kenton_components[,'season']
autoplot(kenton, series="Data") +
  autolayer(adjust_kenton, series="Seasonally adjusted")

##########################################################################
                          ### Canberra (ACT) ###
## Plot
act <- ts(comparison_contagion$`Canberra (ACT)`,
             start=c(01, as.numeric(format(inds[1],"%j"))),
             frequency=365)
plot.ts(act)

## "SMA()" function to smooth time series data -> moving average
act_smoothed <- SMA(act,n=5)
plot.ts(act_smoothed)

## Test if the Time series is stationary with
# the augmented Dickey-Fuller test: if p-value <0.05 then
# it will be stationary
adf.test(act) # stationary

## Unit root test
my_urkpssTest(act, type = c("tau"), lags = c("short"),
              use.lag = NULL, doplot = TRUE)

## Making a time series stationary
nsdiffs(act)
act_seasdiff <- diff(act, differences=1)
act_seasdiff

plot(act_seasdiff, type="l", main="Seasonally Differenced")
adf.test(act_seasdiff) # now it is stationary since the p_value is smaller

acf_act <- acf(act_seasdiff)
pacf_act <- pacf(act_seasdiff)

## Fit and ARIMA model
class(act_seasdiff)
fit_act1 <- arima(act_seasdiff, c(2, 1, 2),seasonal = list(order = c(0, 1, 1), period = 12))
fit_act1
coeftest(fit_act1) 
confint(fit_act1)

## Choosing the best model
acf(fit_act1$residuals)
boxresultACT <- LjungBoxTest(fit_act1$residuals, k=2, StartLag=1)
plot(boxresultACT[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fit_act1$residuals)
qqline(fit_act1$residuals)

# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/#:~:text=arima()%20function%3A,functions%3A%20ets()%20and%20auto.&text=arima()%20function%20in%20R,algorithm%20for%20automatic%20ARIMA%20modeling.
auto.arima(act, trace=TRUE)

## To estimate the trend, seasonal and irregular components of this time series
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

## Remove the seasonal component to get the seasonally adjusted data
adjust_act <- act - act_components[,'season']
autoplot(act, series="Data") +
  autolayer(adjust_act, series="Seasonally adjusted")

##########################################################################
                        ### Aichi Prefecture ###
## Plot
aichi <- ts(comparison_contagion$`Aichi prefecture`,
             start=c(01, as.numeric(format(inds[1],"%j"))),
             frequency=365)
plot.ts(aichi)

## "SMA()" function to smooth time series data -> moving average
aichi_smoothed <- SMA(aichi,n=5)
plot.ts(aichi_smoothed)

## Test if the Time series is stationary with
# the augmented Dickey-Fuller test: if p-value <0.05 then
# it will be stationary
adf.test(aichi) # non-stationary

## Unit root test
my_urkpssTest(aichi, type = c("tau"), lags = c("short"),
              use.lag = NULL, doplot = TRUE)

## Making a time series stationary
nsdiffs(aichi)
aichi_seasdiff <- diff(aichi, differences=1)
aichi_seasdiff

plot(aichi_seasdiff, type="l", main="Seasonally Differenced")
adf.test(aichi_seasdiff) # now it is stationary since the p_value is smaller

acf_aichi <- acf(aichi_seasdiff)
pacf_aichi <- pacf(aichi_seasdiff)

## Fit and ARIMA model
class(aichi_seasdiff)
fit_aichi1 <- arima(aichi_seasdiff, c(2, 1, 2),seasonal = list(order = c(0, 1, 1), period = 12))
fit_aichi1
coeftest(fit_aichi1) 
confint(fit_aichi1)

## Choosing the best model

acf(fit_aichi1$residuals)
boxresultAichi <- LjungBoxTest(fit_aichi1$residuals, k=2, StartLag=1)
plot(boxresultAichi[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fit_aichi1$residuals)
qqline(fit_aichi1$residuals)

# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/#:~:text=arima()%20function%3A,functions%3A%20ets()%20and%20auto.&text=arima()%20function%20in%20R,algorithm%20for%20automatic%20ARIMA%20modeling.
auto.arima(aichi, trace=TRUE)

## To estimate the trend, seasonal and irregular components of this time seris
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

## Remove the seasonal component to get the seasonally adjusted data

adjust_aichi <- aichi - aichi_components[,'season']
autoplot(aichi, series="Data") +
  autolayer(adjust_aichi, series="Seasonally adjusted")

#########################################################################
                            ### Fukuoka ### 
## Plot
fukuoka <- ts(comparison_contagion$`Fukuoka prefecture`,
            start=c(01, as.numeric(format(inds[1],"%j"))),
            frequency=365)
plot.ts(fukuoka)

## "SMA()" function to smooth time series data -> moving average
fukuoka_smoothed <- SMA(fukuoka,n=5)
plot.ts(fukuoka_smoothed)

## Test if the Time series is stationary with
# the augmented Dickey-Fuller test: if p-value <0.05 then
# it will be stationary
adf.test(fukuoka) # non-stationary

## Unit root test
my_urkpssTest(fukuoka, type = c("tau"), lags = c("short"),
              use.lag = NULL, doplot = TRUE)

## Making a time series stationary
nsdiffs(fukuoka)
fukuoka_seasdiff <- diff(fukuoka, differences=1)
fukuoka_seasdiff

plot(fukuoka_seasdiff, type="l", main="Seasonally Differenced")
adf.test(fukuoka_seasdiff) # now it is stationary since the p_value is smaller

acf_fukuoka <- acf(fukuoka_seasdiff)
pacf_fukuoka <- pacf(fukuoka_seasdiff)

## Fit and ARIMA model
class(fukuoka_seasdiff)
fit_fukuoka1 <- arima(fukuoka_seasdiff, c(2, 1, 2),seasonal = list(order = c(0, 1, 1), period = 12))
fit_fukuoka1
coeftest(fit_fukuoka1) 
confint(fit_fukuoka1)

## Choosing the best model
acf(fit_fukuoka1$residuals)
boxresultFukuoka <- LjungBoxTest(fit_fukuoka1$residuals, k=2, StartLag=1)
plot(boxresultFukuoka[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fit_fukuoka1$residuals)
qqline(fit_fukuoka1$residuals)

# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/#:~:text=arima()%20function%3A,functions%3A%20ets()%20and%20auto.&text=arima()%20function%20in%20R,algorithm%20for%20automatic%20ARIMA%20modeling.
auto.arima(fukuoka_seasdiff, trace=TRUE)

## To estimate the trend, seasonal and irregular components of this time seris
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

## Remove the seasonal component to get the seasonally adjusted data

adjust_fukuoka <- fukuoka - fukuoka_components[,'season']
autoplot(fukuoka, series="Data") +
  autolayer(adjust_fukuoka, series="Seasonally adjusted")

#########################################################################
                    ### Eastern Cape (South Africa) ###
## Plot
eastern <- ts(comparison_contagion$`Eastern cape (South Africa)`,
              start=c(01, as.numeric(format(inds[1],"%j"))),
              frequency=365)
plot.ts(eastern)

## "SMA()" function to smooth time series data -> moving average
eastern_smoothed <- SMA(eastern,n=5)
plot.ts(eastern_smoothed)

## Test if the Time series is stationary with
# the augmented Dickey-Fuller test: if p-value <0.05 then
# it will be stationary
adf.test(eastern) # non-stationary

## Unit root test
my_urkpssTest(eastern, type = c("tau"), lags = c("short"),
              use.lag = NULL, doplot = TRUE)

## Making a time series stationary
nsdiffs(eastern)
eastern_seasdiff <- diff(eastern, differences=1)
eastern_seasdiff

plot(eastern_seasdiff, type="l", main="Seasonally Differenced")
adf.test(eastern_seasdiff) # now it is stationary since the p_value is smaller

acf_eastern <- acf(eastern_seasdiff)
pacf_eastern <- pacf(eastern_seasdiff)

## Fit and ARIMA model
class(eastern_seasdiff)
fit_eastern1 <- arima(eastern_seasdiff, c(2, 1, 2),seasonal = list(order = c(0, 1, 1), period = 12))
fit_eastern1
coeftest(fit_eastern1) 
confint(fit_eastern1)

## Choosing the best model
acf(fit_eastern1$residuals)
boxresultEastern <- LjungBoxTest(fit_eastern1$residuals, k=2, StartLag=1)
plot(boxresultEastern[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fit_eastern1$residuals)
qqline(fit_eastern1$residuals)

# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/#:~:text=arima()%20function%3A,functions%3A%20ets()%20and%20auto.&text=arima()%20function%20in%20R,algorithm%20for%20automatic%20ARIMA%20modeling.
auto.arima(eastern_seasdiff, trace=TRUE)

## To estimate the trend, seasonal and irregular components of this time seris
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

## Remove the seasonal component to get the seasonally adjusted data
adjust_eastern <- eastern - eastern_components[,'season']
autoplot(eastern, series="Data") +
  autolayer(adjust_eastern, series="Seasonally adjusted")

#########################################################################
                      ### Rio Grande do Sul ###
## Plot
rio <- ts(comparison_contagion$`Rio grade do sul`,
              start=c(01, as.numeric(format(inds[1],"%j"))),
              frequency=365)
plot.ts(rio)

## Log plot
log_rio <- log(rio)
plot.ts(log_rio)

## "SMA()" function to smooth time series data -> moving average
rio_smoothed <- SMA(rio,n=5)
plot.ts(rio_smoothed)

## Test if the Time series is stationary with
# the augmented Dickey-Fuller test: if p-value <0.05 then
# it will be stationary
adf.test(rio) # non-stationary

## Unit root test
my_urkpssTest(rio, type = c("tau"), lags = c("short"),
              use.lag = NULL, doplot = TRUE)

## Making a time series stationary
nsdiffs(rio)
rio_seasdiff <- diff(eastern, differences=1)
rio_seasdiff

plot(rio_seasdiff, type="l", main="Seasonally Differenced")
adf.test(rio_seasdiff) # now it is stationary since the p_value is smaller

acf_rio <- acf(rio_seasdiff)
pacf_rio <- pacf(rio_seasdiff)

## Fit and ARIMA model
class(rio_seasdiff)
fit_rio1 <- arima(rio_seasdiff, c(2, 1, 2),seasonal = list(order = c(0, 1, 1), period = 12))
fit_rio1
coeftest(fit_rio1) 
confint(fit_rio1)

## Choosing the best model
acf(fit_rio1$residuals)
boxresultRio <- LjungBoxTest(fit_rio1$residuals, k=2, StartLag=1)
plot(boxresultRio[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fit_rio1$residuals)
qqline(fit_rio1$residuals)

# https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/#:~:text=arima()%20function%3A,functions%3A%20ets()%20and%20auto.&text=arima()%20function%20in%20R,algorithm%20for%20automatic%20ARIMA%20modeling.
auto.arima(rio_seasdiff, trace=TRUE)

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

## Remove the seasonal component to get the seasonally adjusted data
adjust_rio <- rio - rio_components[,'season']
autoplot(rio, series="Data") +
  autolayer(adjust_rio, series="Seasonally adjusted")
