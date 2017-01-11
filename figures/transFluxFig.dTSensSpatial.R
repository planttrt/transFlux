library(data.table)
library(raster)
library(fields)
library(maps)
source('~/Projects/procVisData/colorSet.R')
  source('transAuxFuncs.R')

ta <- lst <- ls <- list()
for(i in 1:12){
  lst[[i]] <- raster(paste0('data/spatialdata/2016/lst',i,'.tif'))
  ta[[i]] <- raster(paste0('data/spatialdata/2016/ta',i,'.tif'))
}
for(i in 1:12)  ls[[i]] <- resample(lst[[i]],ta[[i]])

lc <- raster('data/spatialdata/lc1.tif')
lc <- resample(lc, ta[[1]], method='ngb' )

bg <- bothSites$pk$out$beta
bgMean <- apply(bg, 1, mean)
colnames(bothSites$pk$x)

names(bgMean) <- colnames(bothSites$pk$x)

source('figures/transFluxFig.CalcSolarWithCloud.R')


sensdT0 <- sensdT <- sensdT2 <-  sensdT20 <- sensLST <- list()
for(i in 1:12){
  # sensLST[[i]] <- bgMean['dT'] + bgMean['LST4']*4*(ls[[i]])^3+ bgMean['TA.dT']*ta[[i]]
  # sensLST[[i]][lc>5] <- NA

  sensdT0[[i]] <- bgMean['dT'] + bgMean['TA.dT']*ta[[i]] 
  sensdT[[i]] <- sensdT0[[i]] 
  sensdT[[i]][lc>5] <- NA
  
  sensdT20[[i]] <- bgMean['dT'] + bgMean['TA.dT']*ta[[i]] + bgMean['Solar.dT']*solcloudraster[[i]]
  sensdT2[[i]] <- sensdT20[[i]] 
  sensdT2[[i]][lc>5] <- NA
}


png('figures/transFluxFig.dTSensSpatial.WithoutSolarEffect.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(sensdT, colList.Contad, 
                   nlevelsContour = 10,
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()


png('figures/transFluxFig.dTSensSpatial.WithSolarEffect.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(sensdT2, colList.Contad, 
                   nlevelsContour = 10, 
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()



eco <- shapefile('~/Projects/traitsModel/data/maps/ecoregions/eco_us_latlon_provMerged.shp')
physio <- shapefile('~/Projects/traitsModel/data/maps/physioProvinceLatLon/physioProvinceLatLon.shp')


r <- rMean(sensdT[7:9])
png('figures/transFluxFig.dTSensSpatial.WithoutSolarEffect.Summer.png', width = 6, height = 6, res = 300,  units = 'in')
plot(r,xlim=c(-90,-75), ylim=c(25,40),
     col=colorRampPalette(colList.purpleOrange)(100))
map('usa', add = T)
plot(physio, add=T)
dev.off()



r <- rMean(sensdT2[7:9])
png('figures/transFluxFig.dTSensSpatial.WithSolarEffect.Summer.png', width = 6, height = 6, res = 300,  units = 'in')
plot(r, xlim=c(-90,-75), ylim=c(25,40),
     col=colorRampPalette(colList.purpleOrange)(100))
map('usa', add = T)
plot(physio, add=T)
dev.off()

