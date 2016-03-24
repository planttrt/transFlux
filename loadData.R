#--------------------------------------------------------
#
# this file contains necessary codes to load all the data including
#       sap flux, meteorology, eddy flux and remote sensing data
#       (MODIS LST, LAI and VI)
#       for both Blackwood and Parker sites
#
#--------------------------------------------------------



source('auxFunction.R')
source('../R.Repository/calcSolar.R')

#----------------------------
# begin loading the Blackwood data
#----------------------------

# begin sap flux --  blackwood
bwSap <- read.csv('data/sap/sapFlux.BW.csv')
bwSap$ydh <- bwSap$y*100000+bwSap$d*100+bwSap$h
bwSap$yd <- bwSap$y*1000+bwSap$d

bwSap[bwSap==0] <- NA

bwProbs <- read.csv('data/sap/bw.sap/SapTrees.csv')
bwSapTot <- bwSap[,c('pita152', 'pita165', 'litu656', 'litu658', 'acru736', 'acru774', 'qual747', 'qual765', 'acru904', 'acru915', 'pita3969', 'pita2493')]
for(i in 1:ncol(bwSapTot))  bwSapTot[,i] <- bwSapTot[,i]*bwProbs$Sapwood[which(colnames(bwSapTot)[i]==bwProbs$label)]
bwSapTot <- rowSums(bwSapTot[,], na.rm= T)/sum(bwProbs$Sapwood)
bwSapTot[bwSapTot==0] <- NA
bwSapTot <- bwSapTot/3600*1000
# plot(bwSapTot, type='l')
bwSap$sapTot <- bwSapTot
#end sap flux -- blackwood


# begin meteorological -- blackwood
bwRAWS <- read.csv(file = 'data/raws/NDUK.HOURLY.2000.2014.EDITED.csv')
bwRAWS$DOY <- dateToDOY(bwRAWS$Year, bwRAWS$Month, bwRAWS$Day)
bwRAWS$VPD <- 0.6108 * exp(17.27 * bwRAWS$Ta / (bwRAWS$Ta + 237.3))*(1-bwRAWS$RH/100)
bwRAWS$ydh <- bwRAWS$Year*100000 + bwRAWS$DOY*100 + bwRAWS$Hour
bwRAWS$yd <- bwRAWS$Year*1000 + bwRAWS$DOY

# end meteorological -- blackwood


# begin modis reflectance bands, LST and VIs -- blackwood
bwMODIS.SR <- read.table( file = 'data/modis/modisB.BW.csv',  sep = ',', header = T)
bwMODIS.SR$yd <- bwMODIS.SR$y*1000 + bwMODIS.SR$d
  
bwMODIS.LAI <- read.table( file = 'data/modis/modisLAI.BW.csv',  sep = ',', header = T)
bwMODIS.LAI$yd <- bwMODIS.LAI$y*1000 + bwMODIS.LAI$d

bwMODIS.LST <- read.table( file = 'data/modis/modisT.BW.csv',  sep = ',', header = T)
bwMODIS.LST$yd <- bwMODIS.LST$y*1000 + bwMODIS.LST$d

bwMODIS.LST$VTd[is.na(bwMODIS.LST$STd)] <- NA
bwMODIS.LST$VTn[is.na(bwMODIS.LST$STn)] <- NA
bwMODIS.LST$VTd <- bwMODIS.LST$VTd*.1
bwMODIS.LST$VTn <- bwMODIS.LST$VTn*.1
# begin modis reflectance bands, LST and VIs -- blackwood

#----------------------------
# end loading the Blackwood data
#----------------------------



#----------------------------
# begin loading the Parker data
#----------------------------

# begin sap flux --  parker
pkSap <- read.csv('data/sap/sapFlux.Parker.csv')
pkSap$ydh <- pkSap$y*100000+pkSap$d*100+pkSap$h
pkSap$yd <- pkSap$y*1000+pkSap$d
pkSap[pkSap==0] <- NA
# plot(pkSap$sap.mm.day, type='l')
#end sap flux -- parker




# begin meteorological/ameriflux -- parker
 # pkAmeriFluxList <- readAmeriFlux('data/ameriflux/North_Carolina_Loblolly_Pine/with_gaps')
 # write.table(pkAmeriFluxList$units, file = 'data/ameriflux/pkAmeriFluxList_units.csv', sep = ',')
 # write.table(pkAmeriFluxList$data, file = 'data/ameriflux/pkAmeriFluxList_data.csv', sep = ',')
 # write.table(pkAmeriFluxList$coverage, file = 'data/ameriflux/pkAmeriFluxList_coverage.csv', sep = ',')

pkAmeriFluxList <- list()
pkAmeriFluxList$units <- read.table(file = 'data/ameriflux/pkAmeriFluxList_units.csv', sep = ',')
pkAmeriFluxList$coverage <- read.table(file = 'data/ameriflux/pkAmeriFluxList_coverage.csv', sep = ',')
pkAmeriFluxList$data <- read.table(file = 'data/ameriflux/pkAmeriFluxList_data.csv', sep = ',')

pkAmeriFlux <- pkAmeriFluxList$data
pkAmeriFlux$VPD2 <- 0.6108 * exp(17.27 * pkAmeriFlux$TA / (pkAmeriFlux$TA + 237.3))*(1-pkAmeriFlux$RH/100)
pkAmeriFlux$HR <- floor(pkAmeriFlux$HRMIN/100)+1
pkAmeriFlux$ydh <- pkAmeriFlux$YEAR*100000 + pkAmeriFlux$DOY*100 + pkAmeriFlux$HR
pkAmeriFlux$yd <- pkAmeriFlux$YEAR*1000 + pkAmeriFlux$DOY
# begin meteorological/ameriflux -- parker




# begin modis reflectance bands, LST and VIs -- parker
pkMODIS.SR <- read.table( file = 'data/modis/modisB.NC2.csv',  sep = ',', header = T)
pkMODIS.SR$yd <- pkMODIS.SR$y*1000 + pkMODIS.SR$d

pkMODIS.LAI <- read.table( file = 'data/modis/modisLAI.NC2.csv',  sep = ',', header = T)
pkMODIS.LAI$yd <- pkMODIS.LAI$y*1000 + pkMODIS.LAI$d

pkMODIS.LST <- read.table( file = 'data/modis/modisT.NC2.csv',  sep = ',', header = T)
pkMODIS.LST$yd <- pkMODIS.LST$y*1000 + pkMODIS.LST$d

pkMODIS.LST$VTd[is.na(pkMODIS.LST$STd)] <- NA
pkMODIS.LST$VTn[is.na(pkMODIS.LST$STn)] <- NA
pkMODIS.LST$VTd <- pkMODIS.LST$VTd*.1
pkMODIS.LST$VTn <- pkMODIS.LST$VTn*.1
# begin modis reflectance bands, LST and VIs -- parker

#----------------------------
# end loading the Parker data
#----------------------------




#----------------------------
# begin geolocation data for bw and pk
#----------------------------
bw <- list(Latitude = 35.9736, Longitude = -79.1004, Altitude = 168)
pk <- list(Latitude = 35.8031, Longitude = -76.6679, Altitude = 12)
#----------------------------
# end geolocation data for bw and pk
#----------------------------



#----------------------------
# begin corrections for bw and pk
#----------------------------
# pkAmeriFlux$Rg[pkAmeriFlux$Rg<0] <- 0
# bwRAWS$Rg[bwRAWS$Rg<0] <- 0
bwRAWS$Rg <- bwRAWS$Solar.Radiation..KW.hr.m_.*1000
bwRAWS$WS <- bwRAWS$WS..m.s.
bwRAWS$TA <- bwRAWS$Ta
bwSap$TR <- bwSap$sapTot
pkSap$TR <- pkSap$sap.mm.day

pkAmeriFlux$ET <- pkAmeriFlux$LE/(2.502*10^3-2.308*pkAmeriFlux$TA)/1000*3600*24
#----------------------------
# end corrections for bw and pk
#----------------------------



#----------------------------
# begin solar calculations for bw and pk
#----------------------------
# bwSolar <- calcSolar(DOY = data.bw$d + data.bw$dvt/24 , Lat = 35.9736, Elevation = 168, Lon = -79.1004, SLon =  -79.1004) 
# pkSolar <- calcSolar(DOY = data.nc$d + data.nc$dvt/24, Lat = 35.8031, Elevation = 12, Lon=-76.6679, SLon = -76.6679) 
#----------------------------
# end solar calculations for bw and pk
#----------------------------

 

