source('~/Projects/procVisData/dataViz.R')

bg <- bothSites$pk$betaStd

wind <- bg[,"windy.dT"]+bg[,"dT"]
noWind <- -bg[,"windy.dT"]+bg[,"dT"]
rain <- -bg[,"rainy.dT"]+bg[,"dT"]
noRain <- bg[,"rainy.dT"]+bg[,"dT"]
windRain <- cbind(wind, noWind, rain, noRain)

png('figures/transFluxFig.WindRain.png', res=150, width = 8, height = 5, units = 'in')
par(mar=c(4,4,2,1))
par(mfrow=c(1,2))
boxplot(windRain[,1:2], outline=F, ylim=c(-.6,-.1), names=c('windy', 'not windy'))
boxplot(windRain[,3:4], outline=F, ylim=c(-.6,-.1), names=c('rainy', 'not rainy'))
dev.off()