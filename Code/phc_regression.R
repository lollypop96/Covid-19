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

###########################################################################################
########################### Regression Analysis ###########################################
###########################################################################################
data <- read_excel("C:/Users/liry9/Desktop/Personalized healthcare/Datasets/Covid-19.xlsx", sheet = "Area Regions full sum up", col_names = TRUE)

# Remove the column of the state
data <- data[,-2]
data <- data[,-28]

## Remove the rows of Eastern cape and Rio Grande do sul
data1 <- data %>% drop_na()
#data1 <- data_cont[-1]
#row.names(data1) <- data_cont$`Area/Region`
summary(data1)

###########################################################################################
################################# Contagion rate ##########################################
###########################################################################################
# Check for Normality
hist(data1$`Contagion rate`)

# Check correlation between variables that are known to be correlated
cor(data1$`Malignant Neoplasms`, data1$`Tracheal, Bronchus and Lung Cancer`)
cor(data1$`Heart diseases`, data1$Stroke)
cor(data1$`Contagion rate`, data1$`Fatality rate`)
cor(data1$`Chronic Lower Respiratory Disease`, data1$Tubercolosis)
cor(data1$`Influenza and Pneumonia`, data1$Tubercolosis)
cor(data1$`Contagion rate`, data1$`Population (inhabitants)`)

## Regression of Contagion rate on density
lm_cont_density<-lm(data1$`Contagion rate` ~ data1$Density, data = data1)
summary(lm_cont_density) # No significant correlation

## Regression of Contagion rate on CAQI Index
lm_cont_CAQI<-lm(data1$`Contagion rate` ~ data1$`CAQI index`, data = data1)
summary(lm_cont_CAQI)

## Regression of Contagion rate on Unemployment Rate
lm_cont_unemp<-lm(data1$`Contagion rate` ~ data1$`Unemployment rate`, data = data1)
summary(lm_cont_unemp) # No significant correlation

###########################################################################################
############################### Multiple regression #######################################
### Regression on the dataset without:
# PM10, PM25, stroke, Tracheal etc cancer, Chronic Lower respiratory disease,
# Tubercolosis
dat1 <- data1[, -c(17:18)]
dat1 <- dat1[, -20]
dat1 <- dat1[, -21]
dat1 <- dat1[, -c(22:23)]
dat1 <- dat1[, -3]
dat1 <- dat1[, -13]

dat_def <- dat1[-1]
row.names(dat_def) <- dat1$`Area/Region`

lm_cont1<-lm(dat_def$`Contagion rate` ~ ., data = dat_def)
summary(lm_cont1) # No significan correlation on any of the variables

## Regression on the dataset with the Area/Region, Contagion rate, PM10, PM25, Stroke, Tracheal cancer, Chronic disease
keeps <- c("Area/Region", "Contagion rate", "PM10 micrograms/cubic meter", "PM25 micrograms/cubic meter", "Stroke", "Tracheal, Bronchus and Lung Cancer", "Chronic Lower Respiratory Disease")
df<-data1[ , keeps, drop = FALSE]
df_def <- df[-1]
row.names(df_def) <- df$`Area/Region`

lm_df<-lm(df_def$`Contagion rate` ~ ., data = df_def)
summary(lm_df) # Significant correlation with `Tracheal, Bronchus and Lung Cancer`

## Regression on the dataset with the Area/Region, Contagion rate, PM10, PM25, Stroke, Tracheal cancer, Tubercolosis
keeps2 <- c("Area/Region", "Contagion rate", "PM10 micrograms/cubic meter", "PM25 micrograms/cubic meter", "Stroke", "Tracheal, Bronchus and Lung Cancer", "Tubercolosis")
df2<-data1[ , keeps2, drop = FALSE]
df_def2 <- df2[-1]
row.names(df_def2) <- df2$`Area/Region`

lm_df2<-lm(df_def2$`Contagion rate` ~ ., data = df_def2)
summary(lm_df2) # Still significant correlation with `Tracheal, Bronchus and Lung Cancer`

rm(list=c('data', 'dat1', 'dat_def', 'lm_cont1', 'keeps', 'df', 'df_def', 'lm_df', 'keeps2', 'df2', 'df_def2', 'lm_df2', 'lm_cont_CAQI', 'lm_cont_density', 'lm_cont_unemp'))
###########################################################################################
### Regression on the dataset without:
# CAQI, stroke, Tracheal etc cancer, Chronic Lower respiratory disease,
# Tubercolosis
dat2 <- data1[, -19]
dat2 <- dat2[, -21]
dat2 <- dat2[, -22]
dat2 <- dat2[, -c(23:24)]
dat2 <- dat2[, -3]
dat2 <- dat2[, -13]

dat_def2 <- dat2[-1]
row.names(dat_def2) <- dat2$`Area/Region`
rm(dat2)

lm_cont2 <- lm(dat_def2$`Contagion rate` ~ ., data = dat_def2)
summary(lm_cont2) # No significant correlation

###########################################################################################
## Regression on the dataset without: PM10, PM25 stroke, malignant tumor,
# Inluenza, Tubercolosis
dat3 <- data1[, -c(17:18)]
dat3 <- dat3[, -c(21:22)]
dat3 <- dat3[, -22]
dat3 <- dat3[, -22]
dat3 <- dat3[, -3]
dat3 <- dat3[, -13]

dat_def3 <- dat3[-1]
row.names(dat_def3) <- dat3$`Area/Region`
rm(dat3)

lm_cont3<-lm(dat_def3$`Contagion rate` ~ ., data = dat_def3)
summary(lm_cont3) # No significant correlation

###########################################################################################
################################## Fatality rate ##########################################
###########################################################################################
rm(list = ls())

## Import the dataset
data <- read_excel("C:/Users/liry9/Desktop/Personalized healthcare/Datasets/Covid-19.xlsx", sheet = "Area Regions full sum up", col_names = TRUE)

# Remove the column of the state
data <- data[,-2]
data <- data[,-28]

## Remove the rows of Eastern cape and Rio Grande do sul
data1 <- data %>% drop_na()

## Regression Model of Fatality rate on Contagion rate
lm_fat_cont = lm(data1$`Fatality rate`~data1$`Contagion rate`, data = data1)
summary(lm_fat_cont) # Significant correlation

scatter.smooth(x=data1$`Contagion rate`, y=data1$`Fatality rate`, main="Fatality rate ~ Contagion rate")  # scatterplot

## Regression of Fatality rate on density
lm_fat_density<-lm(data1$`Fatality rate` ~ data1$Density, data = data1)
summary(lm_fat_density) # No significant correlation

## Regression of Fatality rate on CAQI Index
lm_fat_CAQI<-lm(data1$`Fatality rate` ~ data1$`CAQI index`, data = data1)
summary(lm_fat_CAQI) # No significant correlation

## Regression of Fatality rate on Hospital beds
lm_fat_hosp<-lm(data1$`Fatality rate` ~ data1$`Hospital beds`, data = data1)
summary(lm_fat_hosp) # No significant correlation

###########################################################################################
############################### Multiple regression #######################################
## Regression analysis on dataset without: PM10, PM25, stroke, Tracheal etc cancer,
# Chronic Lower respiratory disease, Tubercolosis
fat1 <- data1[, -c(17:18)]
fat1 <- fat1[, -20]
fat1 <- fat1[, -21]
fat1 <- fat1[, -c(22:23)]
fat1 <- fat1[, -14]

fat_def <- fat1[-1]
row.names(fat_def) <- fat1$`Area/Region`
rm(fat1)
rm(data)

lm_fat1<-lm(fat_def$`Fatality rate` ~ ., data = fat_def)
summary(lm_fat1)

## Regression on the dataset with the Area/Region, Contagion rate, Fatality rate, Cfb, Density, PM10, PM25, Hospital beds, Stroke, Tracheal cancer, Chronic disease
keeps <- c("Area/Region", "Contagion rate", "Fatality rate", "Cfb", "Density", "PM10 micrograms/cubic meter", "PM25 micrograms/cubic meter", "Hospital beds", "Stroke", "Tracheal, Bronchus and Lung Cancer", "Chronic Lower Respiratory Disease")
df<-data1[ , keeps, drop = FALSE]
df_def <- df[-1]
row.names(df_def) <- df$`Area/Region`

lm_df<-lm(df_def$`Fatality rate` ~ ., data = df_def)
summary(lm_df)

## Regression on the dataset with the Area/Region, Contagion rate, PM25, Hospital beds, Stroke, Tubercolosis
keeps2 <- c("Area/Region", "Contagion rate", "Fatality rate", "Density", "PM25 micrograms/cubic meter", "Hospital beds", "Stroke", "Tubercolosis")
df2<-data1[ , keeps2, drop = FALSE]
df_def2 <- df2[-1]
row.names(df_def2) <- df2$`Area/Region`

lm_df2<-lm(df_def2$`Fatality rate` ~ ., data = df_def2)
summary(lm_df2)

############################################################################################
## Regression analysis on dataset without: PM10, PM25, stroke, Malignant tumor,
# Influenza, Tubercolosis, Unemployment rate, area
fat2 <- data1[, -c(17:18)]
fat2 <- fat2[, -c(20:21)]
fat2 <- fat2[, -21]
fat2 <- fat2[, -22]
fat2 <- fat2[, -13]
fat2 <- fat2[, -14]
fat2 <- fat2[, -13]

fat_def2 <- fat2[-1]
row.names(fat_def2) <- fat2$`Area/Region`
rm(fat2)

lm_fat2<-lm(fat_def2$`Fatality rate` ~ ., data = fat_def2)
summary(lm_fat2)

## Regression on the dataset with the Area/Region, Contagion rate, Cfb, Dfb, Density, Hospital beds
keeps3 <- c("Area/Region", "Contagion rate", "Fatality rate", "Density", "Cfb", "Dfb", "Hospital beds")
df3<-data1[ , keeps3, drop = FALSE]
df_def3 <- df3[-1]
row.names(df_def3) <- df3$`Area/Region`

lm_df3<-lm(df_def3$`Fatality rate` ~ ., data = df_def3)
summary(lm_df3)

############################################################################################
############################################################################################
################################ Alternative dataset #######################################
############################################################################################
rm(list = ls())

# Import the dataset
data <- read_excel("C:/Users/liry9/Desktop/Personalized healthcare/Datasets/Covid-19.xlsx", sheet = "Area Regions full sum up", col_names = TRUE)
data <- data[,-2]
data <- data[,-28]

## Remove the columns for which some of the values are Nan
data2 <- data[,!apply(is.na(data), 2, any)]

###########################################################################################
################################# Contagion rate ##########################################
# Check for Normality
hist(data2$`Contagion rate`)

## Regression of Contagion rate on density
lm_cont_density<-lm(data2$`Contagion rate` ~ data2$Density, data = data2)
summary(lm_cont_density) # No significant correlation

## Regression of Contagion rate on CAQI Index
lm_cont_CAQI<-lm(data2$`Contagion rate` ~ data2$`CAQI index`, data = data2)
summary(lm_cont_CAQI)

## Regression of Contagion rate on Unemployment Rate
lm_cont_unemp<-lm(data2$`Contagion rate` ~ data2$`Unemployment rate`, data = data2)
summary(lm_cont_unemp) # No significant correlation

###########################################################################################
############################### Multiple regression #######################################
### Regression on the dataset without:
# CAQI Index and population
dat1 <- data2[, -14]
dat1 <- dat1[, -18]
dat1 <- dat1[, -3]

dat_def <- dat1[-1]
row.names(dat_def) <- dat1$`Area/Region`

lm_cont1<-lm(dat_def$`Contagion rate` ~ ., data = dat_def)
summary(lm_cont1) # No significan correlation on any of the variables

## Regression on the dataset with Dfc, Density, Area and Hospital beds
keeps <- c("Area/Region", "Contagion rate", "Dfc", "Density", "Area (km2)", "Hospital beds")
df<-data2[ , keeps, drop = FALSE]
df_def <- df[-1]
row.names(df_def) <- df$`Area/Region`

lm_df<-lm(df_def$`Contagion rate` ~ ., data = df_def)
summary(lm_df)

###########################################################################################
################################## Fatality rate ##########################################
###########################################################################################
rm(list = ls())

# Import the dataset
data <- read_excel("C:/Users/liry9/Desktop/Personalized healthcare/Datasets/Covid-19.xlsx", sheet = "Area Regions full sum up", col_names = TRUE)
data <- data[,-2]
data <- data[,-28]

## Remove the columns for which some of the values are Nan
data2 <- data[,!apply(is.na(data), 2, any)]

## Regression Model of Fatality rate on Contagion rate
lm_fat_cont = lm(data2$`Fatality rate`~data2$`Contagion rate`, data = data2)
summary(lm_fat_cont) # Significant correlation

scatter.smooth(x=data2$`Contagion rate`, y=data2$`Fatality rate`, main="Fatality rate ~ Contagion rate")  # scatterplot

## Regression of Fatality rate on density
lm_fat_density<-lm(data2$`Fatality rate` ~ data2$Density, data = data2)
summary(lm_fat_density) # No significant correlation

## Regression of Fatality rate on CAQI Index
lm_fat_CAQI<-lm(data2$`Fatality rate` ~ data2$`CAQI index`, data = data2)
summary(lm_fat_CAQI) # No significant correlation

## Regression of Fatality rate on Hospital beds
lm_fat_hosp<-lm(data2$`Fatality rate` ~ data2$`Hospital beds`, data = data2)
summary(lm_fat_hosp) # No significant correlation

###########################################################################################
############################### Multiple regression #######################################
## Regression analysis without CAQI Index and population
fat1 <- data2[, -14]
fat1 <- fat1[, -18]

fat_def <- fat1[-1]
row.names(fat_def) <- fat1$`Area/Region`
rm(fat1)
rm(data)

lm_fat1<-lm(fat_def$`Fatality rate` ~ ., data = fat_def)
summary(lm_fat1)

## Regression on the dataset with the Area/Region, Contagion rate, Fatality rate, BSk, Area (km2), Density, PM10, Hospital beds
keeps <- c("Area/Region", "Contagion rate", "Fatality rate", "BSk", "Area (km2)", "Density", "PM10 micrograms/cubic meter", "Hospital beds")
df<-data2[ , keeps, drop = FALSE]
df_def <- df[-1]
row.names(df_def) <- df$`Area/Region`

lm_df<-lm(df_def$`Fatality rate` ~ ., data = df_def)
summary(lm_df)
###########################################################################################