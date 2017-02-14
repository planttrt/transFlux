source('~/Projects/procVisData/colorSet.R')

library(raster)
physio <- shapefile('~/Projects/traitsModel/data/maps/physioProvinceLatLon/physioProvinceLatLon.shp')
# eco <- shapefile('~/Projects/traitsModel/data/maps/ecoregions/eco_us_latlon_provMerged.shp')
# provs <- eco
provs <- physio

n <- length(provs$PROVINCE)
labs <- tools::toTitleCase(tolower(provs$PROVINCE))
colList <- c('NA',rainbow(n-1))


png('figures/transFluxFig.PhysioRegionsMaps.png', width = 8, height = 5.5, res = 300,  units = 'in')
par(mar=c(4,0,2,0))
plot(provs, col=colList)

legend(-110, 25.5,legend = labs[2:7] , xpd=T,xjust = 1,
       fill = colList[2:7], bty='n', cex=.8)
legend(-97, 25.5,legend = labs[8:13] , xpd=T,xjust = 1,
       fill = colList[8:13], bty='n', cex=.8)
legend(-80, 25.5,legend = labs[14:19] , xpd=T,xjust = 1,
       fill = colList[14:19], bty='n', cex=.8)
legend(-65, 25.5,legend = labs[20:25] , xpd=T,xjust = 1,
       fill = colList[20:25], bty='n', cex=.8)
mtext('United States physiographic region', cex=2, font=2, line = 0)

dev.off()



