library(raster)

# load annual data
tAir.2016.Annual <- raster('data/spatialdata/PRISM/2016/PRISM_tmean_provisional_4kmM2_2016_bil/PRISM_tmean_provisional_4kmM2_2016_bil.bil')
tAir.2015.Annual <- raster('data/spatialdata/PRISM/2015/PRISM_tmean_stable_4kmM2_2015_bil/PRISM_tmean_stable_4kmM2_2015_bil.bil')
tAir.Mean.Annual <- raster('data/spatialdata/PRISM/PRISM_tmean_30yr_normal_4kmM2_annual_bil/PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil')


# load monthly data
tAir.2016.MonthlyFiles <- dir(path = 'data/spatialdata/PRISM/2016/monthly/',
                              pattern = '.bil$', full.names = T, recursive = T)[c(7:12,1:6)]
# tAir.2016.MonthlyFiles
tAir.2016.Monthly <- list()
for(i in 1:12) tAir.2016.Monthly[[i]] <- raster(tAir.2016.MonthlyFiles[i])

tAir.2015.MonthlyFiles <- dir(path = 'data/spatialdata/PRISM/2016/monthly/',
                              pattern = '.bil$', full.names = T, recursive = T)
# tAir.2015.MonthlyFiles
tAir.2015.Monthly <- list()
for(i in 1:12) tAir.2015.Monthly[[i]] <- raster(tAir.2015.MonthlyFiles[i])


tAir.Mean.MonthlyFiles <- dir(path = 'data/spatialdata/PRISM/PRISM_tmean_30yr_normal_4kmM2_all_bil/',
                              pattern = '.bil$', full.names = T, recursive = T)
# tAir.Mean.MonthlyFiles
tAir.Mean.Monthly <- list()
for(i in 1:12) tAir.Mean.Monthly[[i]] <- raster(tAir.Mean.MonthlyFiles[i])
