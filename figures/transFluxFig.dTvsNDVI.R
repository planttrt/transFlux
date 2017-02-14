library(data.table)
library(raster)
library(ggplot2)
source('transAuxFuncs.R')
ndvi <- raster('data/ndvi/MOD13A2_M_NDVI_2015-07-01_rgb_3600x1800.FLOAT.TIFF')

lc <- raster('data/spatialdata/lc.tif')

r <- sapply(dir('~/Box Sync/Home Folder/Private/DT/4K/NORM//', recursive = T, full.names = T), raster)
r <- rMean(r[6:9])
ndvi <- resample(ndvi, r)
lc <- resample(lc, r, method='ngb')

ndvi[ndvi>1] <- NA
DT <- data.table(ΔT=as.vector(r), 
                  NDVI=as.vector(ndvi),
                  LCid= as.vector(lc))
DT <- na.omit(DT)
DT[,LC:='']
DT
read.csv('~/Projects/phenoLeaf/data/LandCoverTable.csv')

DT[LCid==0, LC:=as.factor('Water')]
DT[LCid%in%(1:5), LC:=as.factor('Forest')]
DT[LCid%in%(6:7), LC:=as.factor('Shrublands')]
DT[LCid%in%(8:9), LC:=as.factor('Savannas')]
DT[LCid%in%c(10), LC:=as.factor('Grassland')]
DT[LCid%in%c(12,14), LC:=as.factor('Croplands')]
DT[LCid%in%c(11), LC:=as.factor('Wetlands')]
DT[LCid%in%c(13), LC:=as.factor('Urban')]
DT[!LCid%in%c(0:14), LC:=as.factor('Others')]

#g <- ggplot(DT[LC%in%c('Forest'),], aes(ΔT, NDVI)) 

#g + geom_point(aes(col=LC)) + geom_smooth()
# g + geom_smooth(method = 'lm', aes(col=LC)) + geom_point(aes(col=LC))
# g +  geom_point(aes(col=LC)) +geom_smooth(method = 'lm', aes()) + ggsave('figures/transFluxFig.dTvsNDVI.png', width = 6, height = 6, units = 'in', dpi = 150)
# g +  geom_point(aes(col=LC)) +geom_smooth(method = 'lm', aes(lty=LC)) + ggsave('figures/transFluxFig.dTvsNDVI2.png', width = 6, height = 6, units = 'in', dpi = 150)

ggplot(DT[!LC%in%c('Others','Water','Wetlands'),], aes(ΔT, NDVI)) +
  geom_point(alpha = 0.05, size=.1) +
  geom_smooth(method = 'lm', aes()) +
  facet_grid(LC~.) + 
  xlim(0,25) + 
  ylim(0,1) +
  ggsave('figures/transFluxFig.dTvsNDVI.png', width = 3,  height = 9, units = 'in', dpi = 150)

# ggplot(DT, aes(LC)) + geom_histogram()
# ggplot(DT[LC%in%c('Forest'),], aes(ΔT, NDVI))  + geom_hex(bins=20)
