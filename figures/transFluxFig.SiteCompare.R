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

png('figures/transFluxFig.SiteCompare.png', res=150, width = 8, height = 4, units = 'in')
par(mar=c(3,4,3,1))
par(mfrow=c(1,2))
par(cex.axis=1.5, font=2)
boxplot(both.betaStd[, c("PK-Solar", "BW-Solar")], outline=F, ylim=c(.2,.5), names=c('Piedmont', 'Coast'), col=colList[c(1,3)])
mtext(text = 'Sensitivity to solar radiation', line = 1, font=2, cex=1.3)
mtext('(a)', line = -2, adj = 0.04, cex = 2)

boxplot(both.betaStd[, c("BW-LST4", "PK-LST4")], outline=F, ylim=c(.2,.5), names=c('Piedmont', 'Coast'), col=colList[c(1,3)])
mtext(text = 'Sensitivity to thermal radiation', line = 1, font=2, cex=1.3)
mtext('(b)', line = -2, adj = 0.04, cex = 2)
# boxplot(both.betaStd[, c("BW-dT", "PK-dT")], outline=F, ylim=c(-.5,.5), names=c('Piedmont', 'Coast'))
dev.off()


