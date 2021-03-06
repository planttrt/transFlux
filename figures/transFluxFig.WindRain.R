source('~/Projects/procVisData/dataViz.R')
source('~/Projects/procVisData/colorSet.R')

colList <- colList.brownGreyGreen


bg <- bothSites$pk$betaStd

wind <- bg[,"windy.dT"]+bg[,"dT"]
noWind <- -bg[,"windy.dT"]+bg[,"dT"]
rain <- -bg[,"rainy.dT"]+bg[,"dT"]
noRain <- bg[,"rainy.dT"]+bg[,"dT"]
windRain <- cbind(wind, noWind, rain, noRain)

png('figures/transFluxFig.WindRain.png', res=150, width = 8, height = 4, units = 'in')
par(mar=c(3,4,3,1))
par(mfrow=c(1,2))
par(cex.axis=1.8, font=2)

boxplot(windRain[,1:2], outline=F, ylim=c(-.6,-.1), names=c('windy', 'not windy'), col=colList[c(1,3)])
mtext('(c)', line = -2, adj = 0.04, cex = 2)
boxplot(windRain[,3:4], outline=F, ylim=c(-.6,-.1), names=c('rainy', 'not rainy'), col=colList[c(1,3)])
mtext('(d)', line = -2, adj = 0.04, cex = 2)
mtext(text = 'Sensitivity to ΔT', outer = T, line = -2, font=2, cex=1.3)
dev.off()