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



col <- colorRampPalette(colList.Contad)(100)
png('figures/transFluxFig.dTSensSpatial.png', width = 8, height = 6, res = 300,  units = 'in')
rng <- range(sapply(sensdT, function(x)(range(x@data@values, na.rm = T))))
par(mfrow=c(3,4), mar=c(0,0,1.5,0))
for(i in 1:12){
  plot(sensdT[[i]], breaks=seq(rng[1], rng[2],length.out = 100),
       xlim=c(-90,-70), ylim=c(25,43),
       axes=FALSE,legend=FALSE,  col=col)
  # contour(sensdT0[[i]], levels = pretty(rng, 15), add=T)
  contour(sensdT0[[i]], nlevels = 10, add=T)
  map('usa', add=T)
  mtext(month.name[i], line = 0, font = 2)
  # if (i==12)colorbar.plot(-72, 30,strip = 1:100, col=col,
  #                       horizontal = F, strip.width = .12, strip.length = .35)
  if (i==12) image.plot(legend.only=TRUE, zlim= rng,
             smallplot=c(.6, .66, .05, .4),
             legend.args = list(text= '', side=3, adj=0, cex=1, line=.3, font=2), 
             col = col, horizontal = F, yaxt='s') 
}
dev.off()




col <- colorRampPalette(colList.FunAndTropical)(100)
png('figures/transFluxFig.dTSensSpatial2.png', width = 8, height = 6, res = 300,  units = 'in')
rng <- range(sapply(sensdT2, function(x)(range(x@data@values, na.rm = T))))
par(mfrow=c(3,4), mar=c(0,0,1.5,0))
for(i in 1:12){
  plot(sensdT2[[i]], breaks=seq(rng[1], rng[2],length.out = 100),
       xlim=c(-90,-70), ylim=c(25,43),
       axes=FALSE,legend=FALSE,  col=col)
  # contour(sensdT2[[i]], levels = pretty(rng, 15), add=T)
  contour(sensdT20[[i]], nlevels = 10, add=T)
  
  map('usa', add=T)
  mtext(month.name[i], line = 0, font = 2)
  # if (i==12)colorbar.plot(-72, 30,strip = 1:100, col=col,
  #                       horizontal = F, strip.width = .12, strip.length = .35)
  if (i==12) image.plot(legend.only=TRUE, zlim= rng,
                        smallplot=c(.6, .66, .05, .4),
                        legend.args = list(text= '', side=3, adj=0, cex=1, line=.3, font=2), 
                        col = col, horizontal = F, yaxt='s') 
}
dev.off()


# 
# col <- colorRampPalette(colList.FunAndTropical)(100)
# png('figures/transFluxFig.LSTSensSpatial.png', width = 8, height = 6, res = 300,  units = 'in')
# rng <- range(sapply(sensLST, function(x)(range(x@data@values, na.rm = T))))
# par(mfrow=c(3,4), mar=c(0,0,1.5,0))
# for(i in 1:12){
#   plot(sensLST[[i]], breaks=seq(rng[1], rng[2],length.out = 100),
#        xlim=c(-90,-70), ylim=c(25,43),
#        axes=FALSE,legend=FALSE,  col=col)
#   map('usa', add=T)
#   mtext(month.name[i], line = 0, font = 2)
#   # if (i==12)colorbar.plot(-72, 30,strip = 1:100, col=col,
#   #                       horizontal = F, strip.width = .12, strip.length = .35)
#   if (i==12) image.plot(legend.only=TRUE, zlim= rng,
#                         smallplot=c(.6, .66, .05, .4),
#                         legend.args = list(text= '', side=3, adj=0, cex=1, line=.3, font=2), 
#                         col = col, horizontal = F, yaxt='s') 
# }
# 
# dev.off()

