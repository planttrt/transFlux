source('~/Projects/procVisData/colorSet.R')
load('data/usaRaster.RData')
library(maps)
library(raster)
physio <- shapefile('~/Projects/traitsModel/data/maps/physioProvinceLatLon/physioProvinceLatLon.shp')

ndvi <- raster('data/ndvi/MOD13A2_M_NDVI_2015-07-01_rgb_3600x1800.FLOAT.TIFF')
prism.sampl <- raster('data/spatialdata/PRISM/2015/PRISM_tmean_stable_4kmM2_2015_bil/PRISM_tmean_stable_4kmM2_2015_bil.bil')
rsmp <- raster(nrow=nrow(prism.sampl), ncol=ncol(prism.sampl))
rsmp@extent <- prism.sampl@extent
ndvi <- resample(ndvi, rsmp)
ndvi[is.na(resample(usaRaster, ndvi, method='ngb'))] <- NA


png('figures/transFluxFig.VI.Map.png', width = 8, height = 5, res = 300,  units = 'in')
par(mar=c(0,0,0,0), bty='n')
plot(ndvi, zlim=c(0,1), axes=F,
     col=colorRampPalette(colList.brownGreen)(1000))
map('usa',add=T)    
plot(physio, add=T)
mtext('NDVI across USA, July 2015', cex=2, font=2, line = -2)
dev.off()
