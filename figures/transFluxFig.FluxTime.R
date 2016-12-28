library(data.table)
source('~/Projects/procVisData/colorSet.R')
colList <- colList.brownGreen 

png(filename = 'figures/transFluxFig.FluxTime.png', width = 7, height = 7, units = 'in',res = 150)

par(mfrow=c(2,1), mar=c(4,4,1,1))
transData[Site=='BW', plot(YearCon, TR, 
                           type='l', col=colList[1], lwd=2,
                           xlim = c(2010,2015), ylim=c(0,10),
                           xlab='Time (Year)', ylab='Flux (mm/day)')]
legend('topright', legend = c('Transpiraiton', 'ET'),
       col = c( col=colList[c(1,5)]), bty = 'n',
       lty=1, lwd=4)
transData[Site=='PK', plot(YearCon, TR,
                           type='l',  col=colList[1], lwd=2,
                           xlim = c(2007,2010), ylim=c(0,20),
                           xlab='Time (Year)', ylab='Flux (mm/day)')]
transData[Site=='PK', lines(YearCon, ET, type='l', col=colList[5], lwd=2)]
dev.off()

