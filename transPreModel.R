library(data.table)
library(lubridate)

source('~/Projects/transFlux/transLoadData.R')
source('~/Projects/transFlux/transAuxFuncs.R')

transData.Orig <- transLoadMergeData()
sites <- read.csv('data/siteData.csv')

transData <- merge(transData.Orig$merged, sites, by='Site')
transData[,dEVI:=c(NA, diff(EVI)), Site]
transData[,dNDVI:=c(NA, diff(NDVI)), Site]

sol <- calcSolar(DOY = transData$DOY + (transData$Hour)/24, 
          Lat = transData$Latitude, Lon = transData$Longitude, 
          SLon = -75)

transData$DayLength <- sol$Daylength
transData$Sdiropen <- sol$Sdiropen
transData$Sdifopen <- sol$Sdifopen
transData[,RgOpen:=Sdifopen+Sdiropen]

transData$ET <- NA
transData$ET <- as.numeric(transData$ET)
# str(transData$ET)
transData[Site=='PK',]$ET <- transData.Orig$pk$ET
transData[ET<=0,ET:=NA]

transData[,YearCon:=Year+DOY/(365+(Year%%4==0))]
