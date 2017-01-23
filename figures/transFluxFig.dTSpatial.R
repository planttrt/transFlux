library(raster)
library(maps)
source('~/Projects/procVisData/colorSet.R')
source('transAuxFuncs.R')
physio <- shapefile('~/Projects/traitsModel/data/maps/physioProvinceLatLon/physioProvinceLatLon.shp')

# forest <- raster('data/spatialdata/forest.tif')
# 
# ta <- lst <- ls <- list()
# for(i in 1:12){
#   lst[[i]] <- raster(paste0('data/spatialdata/2016/lst',i,'.tif'))
#   ta[[i]] <- raster(paste0('data/spatialdata/2016/ta',i,'.tif'))
# }

ta <- sapply(  dir('data/spatialdata/PRISM/2015/monthly/', pattern = 'bil$', full.names = T, recursive = T)  , raster)
lst <- sapply(  dir('data/spatialdata/MODIS/', pattern = 'MOD11C3.A2015', full.names = T, recursive = T)  , raster)

dt <- list()
for(i in 1:12)  dt[[i]] <- resample(lst[[i]],ta[[i]])-273.15-ta[[i]]

# lc <- raster('data/spatialdata/lc1.tif')
# lc <- resample(lc, ta[[1]], method='ngb' )

# dt <- list()
# for(i in 1:12)
#   dt[[i]] <- ls[[i]]-ta[[i]]-273.15
rng <- c(-15, 30)

png('figures/transFluxFig.dTSpatial.USA.Monthly.png', width = 10, height = 7, res = 150,  units = 'in')
plotMonthlySpatial(dt, colList.purpleOrange, rng, cexLegend = 1.5)
dev.off()

r <- rMean(dt)
png('figures/transFluxFig.dTSpatial.USA.Annual.png', width = 9, height = 5, res = 300,  units = 'in')
par(mar=c(3,3,4,1))
plot(r, 
     # xlim=c(-90.5,-74.5), ylim=c(25,40), 
     # zlim=c(-3, 3), #breaks=bks,
     col=colorRampPalette(colList.purpleOrange)(100))
map('usa', add = T)
plot(physio, add=T)
mtext(expression(paste('Thermal stress (',Delta,'T) across USA, 2015')), font=2, line = 2, cex=1.5)
mtext(expression(paste(Delta,'T=T'[surface],'-T'[air])), cex=1.5, font=2, line=.5)
dev.off()
