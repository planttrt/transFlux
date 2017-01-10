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
for(i in 1:12){
  lst.anomaly <- tLST.2015.Monthly[[i]] - tLST.Mean.Monthly[[i]]
  lst.anomaly <- resample(lst.anomaly, x)
  ta.anomaly <- tAir.2015.Monthly[[i]] - tAir.Mean.Monthly[[i]]
  dt.Anomaly.Monthly[[i]] <- lst.anomaly-ta.anomaly
}

lst.anomaly <- tLST.2015.Annual - tLST.Mean.Annual
lst.anomaly <- resample(lst.anomaly, x)
ta.anomaly <- tAir.2015.Annual - tAir.Mean.Annual
dt.Anomaly.Annaul <- lst.anomaly-ta.anomaly

plotMonthlySpatial(dt.Anomaly.Monthly, colList = colList.brownGreyGreen)


png('figures/transFluxFig.tLST.2015.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(tLST.2015.Monthly, colList.orangePurple, 
                   nlevelsContour = 10, 
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()

png('figures/transFluxFig.tAir.2015.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(tAir.2015.Monthly, colList.orangePurple, 
                   nlevelsContour = 10, 
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()

png('figures/transFluxFig.dTAnomaly.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(dt.Anomaly.Monthly, colList.orangePurple, 
                   nlevelsContour = 10, 
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

png('figures/transFluxFig.dTAnomaly.Annual.png', width = 6, height = 6, res = 300,  units = 'in')
plot(r, xlim=xlim, ylim=ylim,breaks=seq(-2,1, length.out = 100),
     col=colorRampPalette(colList.orangePurple)(100), legend=F)
# contour(rlr, nlevels = 5, add=T, lwd=2)
map('usa', add = T)
dev.off()

