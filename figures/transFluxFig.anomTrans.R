library(maps)
source('transAuxFuncs.R')
source('~/Projects/procVisData/colorSet.R')

library(raster)
source('figures/transFluxFig.dTAnomaly.LoadPRISM.R')
source('figures/transFluxFig.dTAnomaly.LoadMODIS.R')
# eco <- shapefile('~/Projects/traitsModel/data/maps/ecoregions/eco_us_latlon_provMerged.shp')
physio <- shapefile('~/Projects/traitsModel/data/maps/physioProvinceLatLon/physioProvinceLatLon.shp')
# forest <- raster('data/spatialdata/forest.tif')


# lst.anomaly <- sapply(dir('data/spatialdata/anomaly/lst/', pattern = '.tif$', full.names = T ), raster)
# ta.anomaly <- sapply(dir('data/spatialdata/anomaly/ta/', pattern = '.tif$', full.names = T ), raster)
dt.Anomaly.Monthly <- sapply(dir('data/spatialdata/anomaly/dt/', pattern = '.tif$', full.names = T ), raster)[c(1,5:12,2:4)]


# tLST4.2015.Monthly <- sapply(dir('data/spatialdata/lst4/2015/', pattern = '.tif$', full.names = T ), raster)
# tLST4.Mean.Monthly <- sapply(dir('data/spatialdata/lst4/mean/', pattern = '.tif$', full.names = T ), raster)
# tLST4.Anom2015.Monthly <- sapply(dir('data/spatialdata/lst4/anom2015/', pattern = '.tif$', full.names = T ), raster)
tLST4.Anom2015.Monthly.Resampled <- sapply(dir('data/spatialdata/lst4/anom2015.resam//', pattern = '.tif$', full.names = T ), raster)[c(1,5:12,2:4)]

plot(tLST4.Anom2015.Monthly.Resampled[[1]])

betas <- apply(bothSites$pk$out$beta, MARGIN = 1,  quantile, probs=c(.5))
names(betas) <- colnames(bothSites$pk$x)

anomTran2015 <- list()
for(i in 1:12)
  anomTran2015[[i]] <- betas['dT']* dt.Anomaly.Monthly[[i]]

png('figures/transFluxFig.TrAnomaly.Southeast.Monthly.png', width = 6.5, height = 9, res = 150,  units = 'in')
plotMonthlySpatial(anomTran2015, #rng=c(0,6),
                   colList.orangePurple, 
                   xlim=c(-90.5,-74.5), ylim=c(25,40),
                   cexLegend = 1.5, nlevelsContour = 3)
dev.off()

png('figures/transFluxFig.TrAnomaly.USA.Monthly.png', width = 10, height = 7, res = 150,  units = 'in')
par(mar=c(3,3,4,1))
plotMonthlySpatial(anomTran2015, #rng=c(0,6),
                   colList.orangePurple, 
                   # xlim=c(-90.5,-74.5), ylim=c(25,40),
                   cexLegend = 1.5, nlevelsContour = 3)
mtext('Anomaly in transpiration across USA, 2015', font=2, line = 2, cex=1.5, outer = T)
dev.off()

# plot(rMean(anomTran2015[6:8]) )

