##########################################################################
# Predicting Solar PV Outputs from weather data - Group Project ESDA 2018
##########################################################################

# Sunamp - Get Data -------------------------------------------------------

# Create a data directory if none exists
if(!dir.exists('data')){dir.create('data')}

# Download the PV and weather station data
baseurl = "https://opendata.edp.com/explore/dataset/"
url_vars = "/download/?format=csv&timezone=GMT&use_labels_for_header=true"
datasets = c("sunlab-faro-meteo-2017","sunlab-faro-meteo-2016","faro_meteo_2015","sunlab-faro-meteo-2014",
            "sunlab-faro-pv-2017", "sunlab-faro-pv-2016","sunlab-faro-pv-2015","sunlab-faro-pv-2014")

for(name in datasets){
  # Check if file is already downloaded
  if(!file.exists(file.path("data", paste(name, ".csv", sep="")))){
    # If not, download the file
    print(paste("Downloading file: ", name, ".csv", sep=""))
    url <- paste(baseurl, name, url_vars, sep="")
    download.file(url, file.path("data", paste(name, ".csv", sep="")))
  }
}

# For some reason the 2018 weather data doesn't want to be downloaded this way: get it manually.
# https://opendata.edp.com/explore/dataset/sunlab-faro-meteo-2018/export/

# Clear out those variables for a tidy environment
rm(list=ls())

# Sunamp data - initial data cleaning ---------------------------------------
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)

# We have columns for PV panels oriented horiztonally, vertically and 'optimally'
# We only want to examine 'Optimal' configuration
# I'm pretty sure we only want power as well for this task.

# Read all PV data files:
pv_files <- list.files(path="data", pattern="sunlab-faro-pv", full.names = TRUE)
df <- rbindlist(lapply(pv_files,fread))

# Select the timestamp and the 'Optimal' readings, then just Power
df <- select(df, "Datetime", contains("Optimal"))
df <- select(df, "Datetime", contains("Power"))

# What times are we working with?
# Convert to lubridate datetime objects
df$Datetime <- ymd_hms(df$Datetime)
# Min and max:
range(df$Datetime)
# "2014-01-01 07:41:00 UTC" "2017-12-31 17:18:00 UTC"

# Let's examine power output over the year - daily and monthly
df$Date <- as.Date(df$Datetime)
df$Month <- month(df$Datetime)
df$Hour <- hour(df$Datetime)
time(df$Datetime)


# weather data

weather <- read.csv('data/sunlab-faro-meteo-2014.csv', sep = ";")













