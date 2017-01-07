library(raster)
library(maps)
source('~/Projects/procVisData/colorSet.R')

ta <- lst <- ls <- list()
for(i in 1:12){
  lst[[i]] <- raster(paste0('data/spatialdata/2016/lst',i,'.tif'))
  ta[[i]] <- raster(paste0('data/spatialdata/2016/ta',i,'.tif'))
}
for(i in 1:12)  ls[[i]] <- resample(lst[[i]],ta[[i]])

lc <- raster('data/spatialdata/lc1.tif')
lc <- resample(lc, ta[[1]], method='ngb' )

png('figures/transFluxFig.dTSaptial.png', width = 12, height = 9, res = 300,  units = 'in')
par(mfrow=c(3,4))
for(i in 1:12){
  dt <- ls[[i]]*.02-ta[[i]]-273.15
  plot(dt)
  map('usa', add=T)
}
dev.off()