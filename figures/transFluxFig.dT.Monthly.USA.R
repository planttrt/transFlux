source('transAuxFuncs.R')
r <- sapply(dir('~/Box Sync/Home Folder/Private/DT/4K/NORM//', recursive = T, full.names = T), raster)
rng <- c(5,20)
# colList <- rev(colList.FunAndTropical)
# colList <- colList.Ideas4Homes
# colList <- colList.WarmAndRustic[c(2,4,1)]
colList <- c('#4897D8', '#ffffff','#C60000')

png('figures/transFluxFig.dT.Monthly.USA.png', width = 8, height = 7, res = 150,  units = 'in')
par(mar=c(0,0,1,0), oma=c(0,0,0,0))
plotMonthlySpatial(r[5:10], colList = colList, showContour = F,rng = rng,
                   cexLegend = 1.5, arrangeMat = c(3,2), sameRange = T,
                   legendPos= c(.78, .82, .05, .6),
                   lables = month.name[5:10])
dev.off()


r <- rMean(r[6:9])
r [r< rng[1]] <- rng[1]
r [r> rng[2]] <- rng[2]
png('figures/transFluxFig.dT.Summer.USA.png', width = 8, height = 5, res = 300,  units = 'in')
par(mar=c(3,3,3,1))
plot(r, 
     # xlim=c(-90.5,-74.5), ylim=c(25,40), 
     zlim=rng, #breaks=bks,
     col=colorRampPalette(colList)(100))
map('usa', add = T)
plot(physio, add=T)
mtext(expression(paste('Thermal stress (∆T) across USA')), font=2, line = 1, cex=1.5)
dev.off()