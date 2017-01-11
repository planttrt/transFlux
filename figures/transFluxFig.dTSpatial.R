library(raster)
library(maps)
source('~/Projects/procVisData/colorSet.R')
source('transAuxFuncs.R')
forest <- raster('data/spatialdata/forest.tif')

ta <- lst <- ls <- list()
for(i in 1:12){
  lst[[i]] <- raster(paste0('data/spatialdata/2016/lst',i,'.tif'))
  ta[[i]] <- raster(paste0('data/spatialdata/2016/ta',i,'.tif'))
}
for(i in 1:12)  ls[[i]] <- resample(lst[[i]],ta[[i]])

lc <- raster('data/spatialdata/lc1.tif')
lc <- resample(lc, ta[[1]], method='ngb' )

dt <- list()
for(i in 1:12)
  dt[[i]] <- ls[[i]]*.02-ta[[i]]-273.15
rng <- c(-15, 30)

png('figures/transFluxFig.dTSpatial.png', width = 10, height = 7, res = 150,  units = 'in')
plotMonthlySpatial(dt, colList.purpleOrange, rng, cexLegend = 1.5)
dev.off()
