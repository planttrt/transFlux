library(maps)
source('transAuxFuncs.R')
source('~/Projects/procVisData/colorSet.R')

library(raster)

source('figures/transFluxFig.dTAnomaly.LoadPRISM.R')
source('figures/transFluxFig.dTAnomaly.LoadMODIS.R')

eco <- shapefile('~/Projects/traitsModel/data/maps/ecoregions/eco_us_latlon_provMerged.shp')
physio <- shapefile('~/Projects/traitsModel/data/maps/physioProvinceLatLon/physioProvinceLatLon.shp')
forest <- raster('data/spatialdata/forest.tif')

# plot(tAir.Mean.Annual- tAir.2015.Annual )
# plot(tLST.Mean.Annual- tLST.2015.Annual )

# dt.anom = (dt)2015 - (dt)mean
#         = (lst-ta)2015 - (lst-ta)mean
#         = (lst2015 - lstmean) -(ta2015-tamean)
# x <- tAir.Mean.Annual*0
# dt.Anomaly.Monthly <- list()
# lst.anomaly <- list()
# ta.anomaly <- list()
# for(i in 1:12){
#   lst.anomaly[[i]] <- tLST.2015.Monthly[[i]] - tLST.Mean.Monthly[[i]]
#   lst.anomaly[[i]] <- resample(lst.anomaly[[i]], x)
#   ta.anomaly[[i]] <- tAir.2015.Monthly[[i]] - tAir.Mean.Monthly[[i]]
#   dt.Anomaly.Monthly[[i]] <- lst.anomaly[[i]]-ta.anomaly[[i]]
# }

# for(i in 1:12){
#   writeRaster(lst.anomaly[[i]], paste0('data/spatialdata/anomaly/lst/lst.anomaly',i,'.tif'), format='GTiff')
#   writeRaster(ta.anomaly[[i]], paste0('data/spatialdata/anomaly/ta/ta.anomaly',i,'.tif'), format='GTiff')
#   writeRaster(dt.Anomaly.Monthly[[i]], paste0('data/spatialdata/anomaly/dt/dt.Anomaly.Monthly',i,'.tif'), format='GTiff')
# }

lst.anomaly <- sapply(dir('data/spatialdata/anomaly/lst/', pattern = '.tif$', full.names = T ), raster)[c(1,5:12,2:4)]
ta.anomaly <- sapply(dir('data/spatialdata/anomaly/ta/', pattern = '.tif$', full.names = T ), raster)[c(1,5:12,2:4)]
dt.Anomaly.Monthly <- sapply(dir('data/spatialdata/anomaly/dt/', pattern = '.tif$', full.names = T ), raster)[c(1,5:12,2:4)]


ta.anomaly.Annual <- tAir.2015.Annual - tAir.Mean.Annual
lst.anomaly.Annual <- tLST.2015.Annual - tLST.Mean.Annual
lst.anomaly.Annual <- resample(lst.anomaly.Annual, ta.anomaly.Annual)
dt.Anomaly.Annual <- lst.anomaly.Annual-ta.anomaly.Annual

# plotMonthlySpatial(dt.Anomaly.Monthly, colList = colList.brownGreyGreen)



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
                   nlevelsContour = 10, rng=c(-3,3),
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()



# lc <- raster('data/spatialdata/lc1.tif')
# r <- resample(dt.Anomaly.Annual, lc)
# tmp <- r
# lr <- raster(ncol=floor(ncol(tmp)/2),
#              nrow=floor(nrow(tmp)/2))
# extent(lr) <- extent(tmp)
# rlr  <- resample(r, lr)
r <- dt.Anomaly.Annual
rng <- quantile(r, probs=c(.01,.99))
xlim=c(-90,-75)
ylim=c(25,40)
png('figures/transFluxFig.dTAnomaly.Annual.png', width = 6, height = 6, res = 300,  units = 'in')
par(mar=c(3,3,4,1))
plot(r, xlim=xlim, ylim=ylim,col=colorRampPalette(colList.purpleOrange)(100))
# contour(rlr, nlevels = 5, add=T, lwd=2)
map('usa', add = T)
# plot(eco, add=T)
plot(physio, add=T)
mtext(expression(paste('Anomaly of thermal stress (',Delta,'T), 2015')), font=2, line = 2, cex=1.5)
mtext(expression(paste(Delta,'T=T'[surface],'-T'[air])), cex=1.5, font=2, line=.5)
dev.off()

# r[!lc%in%c(1:5)] <- NA
# 
# png('figures/transFluxFig.dTAnomaly.Annual.Clipped.png', width = 6, height = 6, res = 300,  units = 'in')
# par(mar=c(3,3,4,1))
# plot(r, xlim=xlim, ylim=ylim,col=colorRampPalette(colList.purpleOrange)(100))
# # contour(rlr, nlevels = 5, add=T, lwd=2)
# map('usa', add = T)
# # plot(eco, add=T)
# plot(physio, add=T)
# mtext(expression(paste('Anomaly of thermal stress (',Delta,'T), 2015')), font=2, line = 2, cex=1.5)
# mtext(expression(paste(Delta,'T=T'[surface],'-T'[air])), cex=1.5, font=2, line=.5)
# dev.off()



r <- rMean(dt.Anomaly.Monthly[6:9])
rng <- c(-2,2)
r [r< rng[1]] <- rng[1]
r [r> rng[2]] <- rng[2]
# rp <- as.data.table(rasterToPoints(r))
# n <- 100
# bks <- rp[x > -90 & x < -70 & y > 25 & y < 40, quantile(layer, probs=seq(0,1, length.out = n))]
png('figures/transFluxFig.dTAnomaly.USA.Summer.png', width = 9, height = 5, res = 300,  units = 'in')
par(mar=c(3,3,4,1))
plot(r, 
     # xlim=c(-90.5,-74.5), ylim=c(25,40), 
     zlim=rng, #breaks=bks,
     col=colorRampPalette(colList.purpleOrange)(100))
map('usa', add = T)
plot(physio, add=T)
mtext(expression(paste('Anomaly of thermal stress (',Delta,'T), summer 2015')), font=2, line = 2, cex=1.5)
mtext(expression(paste(Delta,'T=T'[surface],'-T'[air])), cex=1.5, font=2, line=.5)
dev.off()

# rp <- as.data.table(rasterToPoints(r))
# n <- 100
# bks <- rp[x > -90 & x < -70 & y > 25 & y < 40, quantile(layer, probs=seq(0,1, length.out = n))]
png('figures/transFluxFig.dTAnomaly.Summer.png', width = 6, height = 6, res = 300,  units = 'in')
par(mar=c(3,3,4,1))
plot(r, xlim=c(-90.5,-74.5), ylim=c(25,40), zlim=rng, #breaks=bks,
     col=colorRampPalette(colList.purpleOrange)(100))
map('usa', add = T)
plot(physio, add=T)
mtext(expression(paste('Anomaly of thermal stress (',Delta,'T), summer 2015')), font=2, line = 2, cex=1.5)
mtext(expression(paste(Delta,'T=T'[surface],'-T'[air])), cex=1.5, font=2, line=.5)
dev.off()

r[is.na(forest)] <- NA
png('figures/transFluxFig.dTAnomaly.Summer.Clipped.png', width = 6, height = 6, res = 300,  units = 'in')
par(mar=c(3,3,4,1))
plot(r, xlim=c(-90.5,-74.5), ylim=c(25,40), zlim=c(-3, 3), #breaks=bks,
     col=colorRampPalette(colList.purpleOrange)(100))
map('usa', add = T)
plot(physio, add=T)
mtext(expression(paste('Anomaly of thermal stress (',Delta,'T), summer 2015')), font=2, line = 2, cex=1.5)
mtext(expression(paste(Delta,'T=T'[surface],'-T'[air])), cex=1.5, font=2, line=.5)
dev.off()
# r <- rMean(dt.Anomaly.Monthly[6:8])
# tmp <- resample(lc, r, method='ngb')
# r[tmp>5] <- NA




png('figures/transFluxFig.dTAnomaly.USA.png', width = 10, height = 7, res = 150,  units = 'in')
plotMonthlySpatial(dt.Anomaly.Monthly, #rng=c(0,6),
                   colList.purpleOrange, 
                   cexLegend = 1.5, nlevelsContour = 3)
dev.off()

