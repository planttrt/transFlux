library(raster)

# tmp <- ta[[1]]
# 
# f <- dir('~/Downloads/monthly/', full.names = T, pattern = '.tif$')
# fs <- dir('~/Downloads/monthly/', full.names = F, pattern = '.tif$')
# for(i in 1:12){
#   r <- raster(f[i])
#   r <- resample(r, tmp)
#   writeRaster(r, paste0('data/spatialdata/cloud/', fs[i]), format='GTiff')
# }
# 
# 

# solrasters <- list()
# for(i in 1:12){
# pts <- as.data.table(rasterToPoints(lc))
# colnames(pts) <- c('lon','lat','lc')
# setcolorder(pts, neworder = c('lat','lon','lc'))
# pts[, sol:=calcSolar(lat, DOY = 0.5+(i-.5)*30)$Stotopen]
# coordinates(pts) <- ~ lon+lat
# rast <- raster(nrow=nrow(lc), ncol=ncol(lc))
# extent(rast) <- extent(lc)
# solrasters[[i]] <- rasterize(pts, rast, pts$sol, fun = mean)
# 
# }
#  save(solrasters,file = "solrasterstot.RData")
# load('solrasterstot.RData')

# for(i in 1:12) 
#   writeRaster(solrasters[[i]], paste0('data/spatialdata/solarTot/Stotopen_', i, '.tif'), format='GTiff')
# 
# solrasters <- list()
# cloudasters <- list()
# fsol <- dir('data/spatialdata/solarTot/', full.names = T, pattern = '.tif$')
# fcld <- dir('data/spatialdata/cloud/', full.names = T, pattern = '.tif$')
# for(i in 1:12){
#   solrasters[[i]] <- raster(fsol[i])
#   cloudasters[[i]] <- raster(fcld[i])
# 
# }
# 
# solcloudraster <- list()
# for(i in 1:12){
#   solcloudraster[[i]] <- solrasters[[i]]*(1 - 0.75*(cloudasters[[i]]/10000)^3)
#   writeRaster(solcloudraster[[i]], paste0('data/spatialdata/solCloud/Solcloud_', i, '.tif'), format='GTiff')
# }


solcloudraster <- list() 
fsc <- dir('data/spatialdata/solCloud/', full.names = T, pattern = '.tif$')
for(i in 1:12)  solcloudraster[[i]] <- raster(fsc[i])
  

