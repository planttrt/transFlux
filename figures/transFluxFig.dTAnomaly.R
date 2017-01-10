library(maps)
source('transAuxFuncs.R')
source('~/Projects/procVisData/colorSet.R')

library(raster)
source('figures/transFluxFig.dTAnomaly.LoadPRISM.R')
source('figures/transFluxFig.dTAnomaly.LoadMODIS.R')

# plot(tAir.Mean.Annual- tAir.2015.Annual )
# plot(tLST.Mean.Annual- tLST.2015.Annual )

# dt.anom = (dt)2015 - (dt)mean
#         = (lst-ta)2015 - (lst-ta)mean
#         = (lst2015 - lstmean) -(ta2015-tamean)
x <- tAir.Mean.Annual*0
dt.Anomaly.Monthly <- list()
lst.anomaly <- list()
ta.anomaly <- list()
for(i in 1:12){
  lst.anomaly[[i]] <- tLST.2015.Monthly[[i]] - tLST.Mean.Monthly[[i]]
  lst.anomaly[[i]] <- resample(lst.anomaly[[i]], x)
  ta.anomaly[[i]] <- tAir.2015.Monthly[[i]] - tAir.Mean.Monthly[[i]]
  dt.Anomaly.Monthly[[i]] <- lst.anomaly[[i]]-ta.anomaly[[i]]
}

lst.anomaly.Annaul <- tLST.2015.Annual - tLST.Mean.Annual
lst.anomaly.Annaul <- resample(lst.anomaly.Annaul, x)
ta.anomaly.Annaul <- tAir.2015.Annual - tAir.Mean.Annual
dt.Anomaly.Annaul <- lst.anomaly.Annaul-ta.anomaly.Annaul

# plotMonthlySpatial(dt.Anomaly.Monthly, colList = colList.brownGreyGreen)


png('figures/transFluxFig.tLST.2015.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(tLST.2015.Monthly, colList.orangePurple, 
                   nlevelsContour = 10, 
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()

png('figures/transFluxFig.tAir.2015.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(tAir.2015.Monthly, colList.purpleOrange, 
                   nlevelsContour = 10, 
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()


png('figures/transFluxFig.tLST.Mean.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(tLST.Mean.Monthly, colList.purpleOrange, 
                   nlevelsContour = 10, 
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()

png('figures/transFluxFig.tAir.Mean.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(tAir.Mean.Monthly, colList.purpleOrange, 
                   nlevelsContour = 10, 
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()



png('figures/transFluxFig.tLST.Anomaly.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(lst.anomaly, colList = colList.purpleOrange, 
                   nlevelsContour = 10, rng = c(-5,5),
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()

png('figures/transFluxFig.tAir.Anomaly.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(ta.anomaly, colList = colList.purpleOrange, 
                   nlevelsContour = 10, rng = c(-5,5),
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()

png('figures/transFluxFig.dTAnomaly.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(dt.Anomaly.Monthly, colList =  colList.purpleOrange, 
                   nlevelsContour = 10, rng=c(-10,10),
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()



lc <- raster('data/spatialdata/lc1.tif')
lc
r <- resample(dt.Anomaly.Annaul, lc)
tmp <- r
lr <- raster(ncol=floor(ncol(tmp)/2),
             nrow=floor(nrow(tmp)/2))
extent(lr) <- extent(tmp)
rlr  <- resample(r, lr)
rng <- quantile(r, probs=c(.01,.99))
xlim=c(-90,-75)
ylim=c(25,40)
r[!lc%in%c(1:5)] <- NA

eco <- shapefile('~/Projects/traitsModel/data/maps/ecoregions/eco_us_latlon_provMerged.shp')
physio <- shapefile('~/Projects/traitsModel/data/maps/physioProvinceLatLon/physioProvinceLatLon.shp')
png('figures/transFluxFig.dTAnomaly.Annual.png', width = 6, height = 6, res = 300,  units = 'in')
plot(r, xlim=xlim, ylim=ylim,col=colorRampPalette(colList.purpleOrange)(100))
# contour(rlr, nlevels = 5, add=T, lwd=2)
map('usa', add = T)
# plot(eco, add=T)
plot(physio, add=T)
dev.off()






png('figures/transFluxFig.dTAnomaly.USA.png', width = 10, height = 7, res = 150,  units = 'in')
plotMonthlySpatial(dt.Anomaly.Monthly, rng=c(0,6),
                   colList.purpleOrange, 
                   cexLegend = 1.5, nlevelsContour = 3)
dev.off()

