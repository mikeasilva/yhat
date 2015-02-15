## K Nearest County
## Written by: Mike Silva

## Description:
## Using the 2010 Geographic Identifiers downloaded from American FactFinder, 
## this function will find the closest counties to a given lattitude and 
## longitude.

## Example:
## To get the 3 nearest counties for Rochester, NY
## df <- data.frame(k=3, lat=43.1656, lon=77.6114)
## kncounty(df)

library(dplyr)
library(fields)

csv <- read.csv('DEC_10_DP_G001_with_ann.csv') %>%
  select(GEO.display.label, GEO.id2, VD074, VD075)
names(csv) <- c('county.name', 'fips', 'lat', 'lon')
csv <- csv[2:nrow(csv),] # remove the meta information

counties <- csv %>%
  mutate(lat = as.numeric(as.character(lat))) %>%
  mutate(lon = - as.numeric(as.character(lon))) %>%
  select(lat, lon)

fips <- csv %>%
  select(fips)

county.names <- csv %>%
  select(county.name)

rm(csv)

## Input is data frame with
## lon: longitude
## lat: latitude
## k: number of counties nearest neighbors
kncounty <- function(df){
  coords <- df[, c('lat', 'lon')]
  distance <- as.data.frame(rdist.earth(counties, coords))
  return.df <- data.frame(fips = fips, name = county.names, distance = distance)
  names(return.df) <- c('fips', 'name','distance')
  return.df <- return.df[order(return.df$distance),]
  return.df[1:df$k,]
}