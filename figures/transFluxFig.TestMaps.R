
png('figures/transFluxFig.tLST.2015.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(tLST.2015.Monthly, colList.orangePurple, 
                   nlevelsContour = 10, 
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()

png('figures/transFluxFig.tAir.2015.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(tAir.2015.Monthly, colList.purpleOrange, 
                   nlevelsContour = 10, 
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()


png('figures/transFluxFig.tLST.Mean.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(tLST.Mean.Monthly, colList.purpleOrange, 
                   nlevelsContour = 10, 
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()

png('figures/transFluxFig.tAir.Mean.Monthly.png', width = 6.5, height = 9, res = 300,  units = 'in')
plotMonthlySpatial(tAir.Mean.Monthly, colList.purpleOrange, 
                   nlevelsContour = 10, 
                   cexLegend = 1.5,lwdContour = 2,
                   legendPos= c(.6, .7, .05, .4),
                   xlim=c(-90,-75), ylim=c(25,40))
dev.off()

