library(data.table)
library(raster)
library(fields)
library(maps)
source('~/Projects/procVisData/colorSet.R')
source('transAuxFuncs.R')

ta <- sapply(dir('data/spatialdata/PRISM/2015/monthly/', pattern = '.bil$', full.names = T, recursive = T),
             raster)


bg <- bothSites$pk$out$beta
bgMean <- apply(bg, 1, mean)
colnames(bothSites$pk$x)

names(bgMean) <- colnames(bothSites$pk$x)

source('figures/transFluxFig.CalcSolarWithCloud.R')

# lc <- resample(raster('data/spatialdata/lc1.tif'), ta[[1]], method='ngb')
sensdT0 <- sensdT <- sensdT2 <-  sensdT20 <- sensLST <- list()
for(i in 1:12){
  # sensLST[[i]] <- bgMean['dT'] + bgMean['LST4']*4*(ls[[i]])^3+ bgMean['TA.dT']*ta[[i]]
  # sensLST[[i]][lc>5] <- NA

  # sensdT0[[i]] <- bgMean['dT'] + bgMean['TA.dT']*ta[[i]] 
  # sensdT[[i]] <- sensdT0[[i]] 
  # sensdT[[i]][lc>5] <- NA
  
  sensdT20[[i]] <- bgMean['dT'] + bgMean['TA.dT']*ta[[i]] + bgMean['Solar.dT']*solcloudraster[[i]]
  sensdT2[[i]] <- sensdT20[[i]] 
  # sensdT2[[i]][lc>5] <- NA
}


# png('figures/transFluxFig.dTSensSpatial.WithoutSolarEffect.png',
#     width = 5, height = 7, res = 300,  units = 'in')
# par(mar=c(0,0,1,0), oma=c(0,0,0,0))
# rng <- c(-.40, -.35)
# plotMonthlySpatial(sensdT0[5:10], colList.Contad, arrangeMat = c(3,2),
#                    nlevelsContour = 10,rng = rng, 
#                    cexLegend = 1.5,lwdContour = 2,
#                    legendPos= c(.6, .7, .04, .5),
#                    xlim=c(-90,-75), ylim=c(25,40),
#                    lables = month.name[5:10])
# dev.off()
# 



png('figures/transFluxFig.dTSensSpatial.WithSolarEffect.png',
    width = 5, height = 7, res = 300,  units = 'in')
par(mar=c(0,0,1,0), oma=c(0,0,0,0))
rng <- c(-.35, -.15)
plotMonthlySpatial(sensdT20[5:10], colList.Contad, arrangeMat = c(3,2),
                   nlevelsContour = 10,rng = rng, 
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .04, .5),
                   xlim=c(-90,-75), ylim=c(25,40),
                   lables = month.name[5:10])
dev.off()





eco <- shapefile('~/Projects/traitsModel/data/maps/ecoregions/eco_us_latlon_provMerged.shp')
physio <- shapefile('~/Projects/traitsModel/data/maps/physioProvinceLatLon/physioProvinceLatLon.shp')

# 
# r <- rMean(sensdT0[7:9])
# rng <- c(-.40, -.35)
# r[r<rng[1]] <- rng[1]
# r[r>rng[2]] <- rng[2]
# png('figures/transFluxFig.dTSensSpatial.WithoutSolarEffect.NotClipped.Summer.png', width = 6, height = 6, res = 300,  units = 'in')
# par(mar=c(3,3,4,1))
# plot(r,xlim=c(-90,-75), ylim=c(25,40),zlim=rng,
#      col=colorRampPalette((colList.Contad))(100))
# map('usa', add = T)
# plot(physio, add=T)
# mtext('Sensitivity to ∆T, Jul-Aug-Sept', font=2, line = 2, cex=1.5)
# mtext(expression('(b) without solar radiation effects'), font=2, line = .1, cex=1.5)
# dev.off()


r <- rMean(sensdT20[7:9])
rng <- c(-.29, -.26)
r[r<rng[1]] <- rng[1]
r[r>rng[2]] <- rng[2]
png('figures/transFluxFig.dTSensSpatial.WithSolarEffect.Summer.NotClipped.png', width = 6, height = 6, res = 300,  units = 'in')
par(mar=c(3,3,4,1))
plot(r, xlim=c(-90,-75), ylim=c(25,40),zlim=rng,
     col=colorRampPalette((colList.Contad))(100))
map('usa', add = T)
plot(physio, add=T)
mtext('Sensitivity to ∆T, Jul-Aug-Sept', font=2, line = 2, cex=1.5)
mtext('mm/day/°C', font=1, line = -4, cex=1, adj=1.25)
# mtext(expression('(a) with solar radiation effects'), font=2, line = .1, cex=1.5)
dev.off()



r <- rMean(sensdT20[7:9])
rng <- c(-.29, -.26)
r[r<rng[1]] <- rng[1]
r[r>rng[2]] <- rng[2]
png('figures/transFluxFig.dTSensSpatial.WithSolarEffect.Summer.NotClipped.USA.png', width = 6, height = 6, res = 300,  units = 'in')
par(mar=c(3,3,4,1))
plot(r, xlim=c(-95,-65), ylim=c(25,50),zlim=rng,
     col=colorRampPalette((colList.Contad))(100))
map('usa', add = T)
plot(physio, add=T)
mtext('Sensitivity to ∆T, Jul-Aug-Sept', font=2, line = 2, cex=1.5)
# mtext(expression('(a) with solar radiation effects'), font=2, line = .1, cex=1.5)
dev.off()

