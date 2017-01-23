source('~/Projects/procVisData/dataViz.R')
source('~/Projects/procVisData/colorSet.R')
library(data.table)

colList <- colList.greenGreyBrown

bw.betaStd <- bothSites$bw$betaStd
pk.betaStd <- bothSites$pk$betaStd

colnames(bw.betaStd) <- paste0('BW-',colnames(bw.betaStd))
colnames(pk.betaStd) <- paste0('PK-',colnames(pk.betaStd))

both.betaStd <- cbind(bw.betaStd, pk.betaStd)

colnames(both.betaStd)

png('figures/transFluxFig.SiteCompare.png', res=150, width = 8, height = 5, units = 'in')
par(mar=c(4,4,3,1))
par(mfrow=c(1,2))
boxplot(both.betaStd[, c("PK-Solar", "BW-Solar")], outline=F, ylim=c(.2,.5), names=c('BW', 'PK'), col=colList[c(1,3)])
mtext(text = '(a) Solar Radiation', line = 1, font=2, cex=1.3)
boxplot(both.betaStd[, c("BW-LST4", "PK-LST4")], outline=F, ylim=c(.2,.5), names=c('BW', 'PK'), col=colList[c(1,3)])
mtext(text = '(b) Thermal Radiation', line = 1, font=2, cex=1.3)
# boxplot(both.betaStd[, c("BW-dT", "PK-dT")], outline=F, ylim=c(-.5,.5), names=c('Piedmont', 'Coast'))
dev.off()


