source('auxFunction.R')
source('../R.Repository/calcSolar.R')

data <- list()


# loading the Blackwood data
#----------------------------

# begin sap flux --  bw
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
plot(bwSapTot, type='l')
bwSap$sapTot <- bwSapTot
#end sap flux -- bw



# begin meteorological -- bw
bwRAWS <- read.csv(file = 'data/raws/NDUK.HOURLY.2000.2014.EDITED.csv')
bwRAWS$DOY <- dateToDOY(bwRAWS$Year, bwRAWS$Month, bwRAWS$Day)
bwRAWS$VPD <- 0.6108 * exp(17.27 * bwRAWS$Ta / (bwRAWS$Ta + 237.3))*(1-bwRAWS$RH/100)
bwRAWS$ydh <- bwRAWS$y*100000 + bwRAWS$d*100 + bwRAWS$h
bwRAWS$yd <- bwRAWS$y*1000 + bwRAWS$d
# end meteorological -- bw





# begin modis reflectance bands, LST and VIs -- bw
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
# begin modis reflectance bands, LST and VIs -- bw



sapfluxParker <- read.csv('data/sapFlux.Parker.csv')

ameriParker <- readAmeriFlux('data/other data/North_Carolina_Loblolly_Pine/with_gaps/')

# -------------Load MODIS 

######
