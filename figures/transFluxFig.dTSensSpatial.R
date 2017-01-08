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

# solrasters <- list()
# for(i in 1:12){
# pts <- as.data.table(rasterToPoints(lc))
# colnames(pts) <- c('lon','lat','lc')
# setcolorder(pts, neworder = c('lat','lon','lc'))
# pts[, sol:=calcSolar(lat, DOY = 0.5+(i-.5)*30)$Sdiropen]
# coordinates(pts) <- ~ lon+lat
# rast <- raster(nrow=nrow(lc), ncol=ncol(lc))
# extent(rast) <- extent(lc) 
# solrasters[[i]] <- rasterize(pts, rast, pts$sol, fun = mean)
# 
# }
# save(solrasters, "solrasters.RData")

load('solrasters.RData')

sensdT0 <- sensdT <- sensdT2 <-  sensdT20 <- sensLST <- list()
for(i in 1:12){
  sensLST[[i]] <- bgMean['dT'] + bgMean['LST4']*4*(ls[[i]])^3+ bgMean['TA.dT']*ta[[i]]
  sensLST[[i]][lc>5] <- NA

  sensdT0[[i]] <- bgMean['dT'] + bgMean['TA.dT']*ta[[i]] 
  sensdT[[i]] <- sensdT0[[i]] 
  sensdT[[i]][lc>5] <- NA
  
  sensdT20[[i]] <- bgMean['dT'] + bgMean['TA.dT']*ta[[i]] + bgMean['Solar.dT']*solrasters[[i]]*.5
  sensdT2[[i]] <- sensdT20[[i]] 
  sensdT2[[i]][lc>5] <- NA
}


png('figures/transFluxFig.dTSensSpatial.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(sensdT, colList.Contad, 
                   nlevelsContour = 10,
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()


png('figures/transFluxFig.dTSensSpatial.WithSolarEffect.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(sensdT2, colList.orangePurple, 
                   nlevelsContour = 10, 
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()

