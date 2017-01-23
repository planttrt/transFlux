library(raster)
library(maps)

source('~/Projects/procVisData/colorSet.R')
# eco <- shapefile('~/Projects/traitsModel/data/maps/ecoregions/eco_us_latlon_provMerged.shp')
physio <- shapefile('~/Projects/traitsModel/data/maps/physioProvinceLatLon/physioProvinceLatLon.shp')

dt.Anomaly.Monthly <- list()
f <- dir('data/spatialdata/anomaly/dt/', pattern = '.tif$', full.names = T )
for(i in 1:12) dt.Anomaly.Monthly[[i]] <- raster(f[i])


rList <- dt.Anomaly.Monthly[6:10]
f <- function(x){x0 <- x;x0[x<0] <- 0;x0}
r <- sapply(rList, f)
cdt <- Reduce(r, f = mean)
# quantile(cdt, seq(0,1,.05))
cdt[cdt>1] <- 1

# plot(eco, add=T, border='#cccccc', lwd=1)

fs <- dir('data/spatialdata/PRISM/ppt/ppt/', pattern = 'bil$', recursive = T, full.names = T)
ppty <- sapply(fs, raster)
ppttot <- Reduce(ppty[5:9],f =  mean)

fs <- dir('data/spatialdata/PRISM/ppt/normal/PRISM_ppt_30yr_normal_4kmM2_all_bil//', pattern = 'bil$', recursive = T, full.names = T)
pptn <- sapply(fs, raster)
pptnorm <- Reduce(pptn[5:9],f =  mean)



dppt <- pptnorm -ppttot
dppt[dppt<0] <- 0
dppt[dppt>40] <- 40



ylim <- c(32, 37)
xlim <- c(-84, -76)
xlim=c(-90,-70)
ylim=c(25,40)

par(mfrow=c(2,1))
plot(cdt, zlim=c(0,1), 
     xlim=xlim, ylim = ylim, 
     col=colorRampPalette(colList.purpleOrange)(100))
plot(physio, add=T, border='#cccccc', lwd=1)
map('usa', add = T, lwd=2)

plot(dppt,
     xlim=xlim, ylim = ylim, 
     col=colorRampPalette(colList.purpleOrange)(100))
plot(physio, add=T, border='#cccccc', lwd=1)
map('usa', add = T, lwd=2)
