source('transAuxFuncs.R')
r <- sapply(dir('~/Box Sync/Home Folder/Private/DT/4K/ANOMALY/2015/', recursive = T, full.names = T), raster)

png('figures/transFluxFig.dTAnomaly.Monthly.USA.png', width = 8, height = 7, res = 150,  units = 'in')
par(mar=c(0,0,1,0), oma=c(0,0,0,0))
plotMonthlySpatial(r[5:10], colList = colList.purpleOrange, showContour = F,
                    rng = c(-2,2), cexLegend = 1.5, arrangeMat = c(3,2), 
                   legendPos= c(.78, .82, .05, .6),
                   
                   lables = month.name[5:10])
dev.off()


r <- rMean(r[6:9])
rng <- c(-2,2)
r [r< rng[1]] <- rng[1]
r [r> rng[2]] <- rng[2]
png('figures/transFluxFig.dTAnomaly.USA.Summer.png', width = 8, height = 5, res = 300,  units = 'in')
par(mar=c(3,3,3,1))
plot(r, 
     # xlim=c(-90.5,-74.5), ylim=c(25,40), 
     zlim=rng, #breaks=bks,
     col=colorRampPalette(colList.purpleOrange)(100))
map('usa', add = T)
plot(physio, add=T)
mtext(expression(paste('Anomaly of thermal stress (∆T), across USA in summer 2015')), font=2, line = 1, cex=1.5)
# mtext(expression(paste(Delta,'T=T'[surface],'-T'[air])), cex=1.5, font=2, line=.5)
dev.off()
