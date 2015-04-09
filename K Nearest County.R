## K Nearest County
## Written by: Mike Silva

## Description:
## Using R county map, this function will find the k closest counties to a 
## given lattitude and longitude and return the name, fips, and distance.

## Example:
## To get the 3 nearest counties for Rochester, NY
## df <- data.frame(k=3, lat=43.1656, lon=-77.6114)
## kcounty(df)

library(ggplot2)
library(dplyr)
library(fields)

data <- read.csv('counties.csv', colClass=rep('character',3)) %>%
  merge(., map_data('county')) %>%
  mutate(lat=as.numeric(lat)) %>%
  mutate(lon=as.numeric(long)) %>%
  rename(fips=county.fips) %>%
  select(county.name,fips,lat,lon)

## Input is data frame with
## lon: longitude
## lat: latitude
## k: number of counties nearest neighbors
kcounty <- function(df){
  data$distance <- as.vector(rdist.earth(data[,c('lat','lon')], df[, c('lat', 'lon')]))
  data <-aggregate(data[ , c('distance')]  ,list(data$fips, data$county.name) , min)
  names(data) <- c('fips', 'name','distance')
  data <- data[order(data$distance),]
  data[1:df$k,]
}

## Deploy function on yhat

install.packages('yhatr')
library(yhatr)

model.require <- function() {
  library(fields)
}

model.transform <- function(df) {
  df
}

model.predict <- function(df) {
  kcounty(df)
}

## Username/API key go here!
yhat.config  <- c(
  username='YOUR_USERNAME',
  apikey='YOUR_API_KEY',
  env='http://cloud.yhathq.com/'
)

yhat.deploy('kcounty')