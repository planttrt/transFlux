library(data.table)
source('~/Projects/modisProcess/modisProcess.R')
source('transAuxFuncs.R')

# infolder <- '~/Downloads/MOD11C3//'
# outfolder <- 'data/spatialdata/MODIS/'
# 
# convertHDFEOS2GeoTIFF(infolder, outfolder)

tiffPaths <- dir(path = 'data/spatialdata/MODIS/', pattern = '.tif$', full.names = T)
tiffFiles <- dir(path = 'data/spatialdata/MODIS/', pattern = '.tif$', full.names = F)
tmp <- matrix(unlist(strsplit(tiffFiles, split = '\\.')), nrow = length(tiffFiles), byrow = T)

modTable <- data.table(tiffPaths = tiffPaths,
                       tiffFiles=tiffFiles,
                       adate = as.integer(gsub(tmp[,2], pattern = 'A', replacement = '')))
modTable[, Year:= floor(adate/1000)]
modTable[, DOY:= adate%%1000]
modTable[, Month:=ceiling((DOY+1)/30)]
modTable


# for(i in 1:12) {
#   rList <- sapply(modTable[Month==i, tiffPaths], raster)
#   tmp <- rMean(rList)
#   fname <- paste0('data/spatialdata/toTIFF/tLST.Mean.Monthly.', i, '.tif')
#   writeRaster(tmp, fname, format='GTiff')
# }

tLST.Mean.Monthly <- list()
for(i in 1:12) tLST.Mean.Monthly[[i]] <- raster(paste0('data/spatialdata/toTIFF/tLST.Mean.Monthly.', i, '.tif'))

# tmp <- rMean(tLST.Mean.Monthly)
# writeRaster(tmp, 'data/spatialdata/toTIFF/tLST.Mean.Annual.tif', format='GTiff')

tLST.Mean.Annual <-raster('data/spatialdata/toTIFF/tLST.Mean.Annual.tif')

tLST.2015.Monthly <- sapply(modTable[Year==2015, tiffPaths], raster)

# tmp <- rMean(tLST.2015.Monthly)
# writeRaster(tmp, 'data/spatialdata/toTIFF/tLST.2015.Annual.tif', format='GTiff')

tLST.2015.Annual <-raster('data/spatialdata/toTIFF/tLST.2015.Annual.tif')


