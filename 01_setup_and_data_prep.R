library(lattice)
library(caret)
library(psych)
library(ggExtra)
library(tidyverse)
library(corrplot)
library(forcats)
library(lubridate)
library(data.table)
library(ggforce)
library(broom)
library(modelr)
# For ridge regression:
library(glmnet)
library(lmridge)
# For neural network:
library(neuralnet)
library(keras)
# install_keras() 
# If issues with keras try this: https://github.com/rstudio/keras/issues/147#issuecomment-335990686
library(magrittr)
library(mlbench)

##########################################################################
# Predicting Solar PV Outputs from weather data - Group Project ESDA 2018
##########################################################################

# Sunlab - Get Data -------------------------------------------------------

# Create a data directory if none exists
if(!dir.exists('data')){dir.create('data')}

# Download all PV and weather data files
# Use the below script if desired to download files
# WARNING: rename the 2015 meteo datafile to the same pattern as others.

baseurl = "https://opendata.edp.com/explore/dataset/"
url_vars = "/download/?format=csv&timezone=GMT&use_labels_for_header=true"
datasets = c("sunlab-faro-meteo-2017","sunlab-faro-meteo-2016","faro_meteo_2015","sunlab-faro-meteo-2014",
             "sunlab-faro-pv-2017", "sunlab-faro-pv-2016","sunlab-faro-pv-2015","sunlab-faro-pv-2014")

# This checks for files against the original downloaded name, 
# so will redownload meteo_2015 once you have changed the name

# for(name in datasets){
#   # Check if file is already downloaded
#   if(!file.exists(file.path("data", paste(name, ".csv", sep="")))){
#     # If not, download the file
#     print(paste("Downloading file: ", name, ".csv", sep=""))
#     # url <- paste(baseurl, name, url_vars, sep="")
#     # download.file(url, file.path("data", paste(name, ".csv", sep="")))
#   }
# }

# 2018 weather data doesn't want to be downloaded this way: get it manually.
# https://opendata.edp.com/explore/dataset/sunlab-faro-meteo-2018/export/

# Clear out those variables for a tidy environment
rm(list=ls()) 

# Data preparation all years ----------------------------------------------

# Notes
# Care that faro_meteo_2015.csv does not match other environmental data filenames - change it
# Change 2016 meteo column header Direct -> Diffuse to match other years

# Read all PV data files:
df <- data.frame()
for(i in 1:4){
  k<-(i+2013)
  file_name=paste("data/sunlab-faro-pv-",k,".csv",sep="")
  data<-read.csv(file=file_name, stringsAsFactors = FALSE, sep=';')
  df<-rbind(df,data)
}

# Prepare pv generation data
df <- drop_na(df)
df$Datetime <- ymd_hms(df$Datetime)
df$Year <- year(df$Datetime)
df$Month <- month(df$Datetime)
df$YDay <- yday(df$Datetime)
df$Hour <- hour(df$Datetime)
df$Minute <- minute(df$Datetime)
df$Month <- as.numeric(df$Month)

# Select only the 'optimal' angle panels and divide into A and B modules
df2 <- select(df, c("Datetime", "Year", "Month", "YDay", "Hour", "Minute"), contains("Optimal"))
sunlab_A <-  select(df2, c("Datetime", "Year", "Month", "YDay", "Hour", "Minute"), contains("A_"))
sunlab_B <-  select(df2, c("Datetime", "Year", "Month", "YDay", "Hour", "Minute"), contains("B_"))

# Read in all weather data (2014 - 2017 - remember we have first week of 2018 too)
df1 <- data.frame()
for(i in 1:4){
  k<-(i+2013)
  file_name=paste("data/sunlab-faro-meteo-",k,".csv",sep="")
  data<-read.csv(file=file_name, stringsAsFactors = FALSE, sep=';')
  df1<-rbind(df1,data)
}
meteo <- df1
meteo$Datetime <- ymd_hms(meteo$Datetime)
meteo$Year <- year(meteo$Datetime)

# Make weather data names easier to work with
setnames(meteo, old=c("Ambient.Temperature..ºC.","Global.Radiation..W.m2.","Diffuse.Radiation..W.m2.",
                      "Ultraviolet..W.m2.","Wind.Velocity..m.s.","Wind.Direction..º.",
                      "Precipitation..mm.","Atmospheric.pressure..hPa."),
               new=c("ambient_temperature","global_radiation","diffuse_radiation",
                     "ultraviolet","wind_velocity","wind_direction",
                     "precipitation","atmospheric_pressure"))

# Create complete dataset for module A
sunlab_meteo_A <- meteo[,c(1:9)]
sunlab_A <- left_join(sunlab_A ,sunlab_meteo_A,by=c("Datetime"="Datetime"))

# Create a dataset of just PV data just for 2017
sunlab_pv_17 <- filter(df, Year=="2017")

# Create a slice just of 2017
sunlab_all_2017 <- filter(sunlab_A, Year=="2017")
