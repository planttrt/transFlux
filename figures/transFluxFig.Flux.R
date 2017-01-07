library(ggplot2)
library(data.table)
library(lubridate)
source('~/Projects/procVisData/colorSet.R')
colList <- colList.brownGreen 
colList <- c('#1240AB','#FF7400','#FFD300')
colList <- c('#3E13AF','#8B6ED7','black')
colList <- c('#375E97','#FB6542','black')

colnames(transData.Orig$pkField)
colnames(transData.Orig$bwField)

tmp <- transData.Orig$pkField[, .(SM=mean(SWC1, na.rm = T), 
                                  PR=sum(PREC, na.rm = T),
                                  ET=mean(ET, na.rm = T),
                                  TR=mean(TR, na.rm = T)), .(YEAR, DOY)]



png('figures/transFluxFig.Flux.png', width = 10, height = 5, units = 'in', res = 150)
par(mar=c(3,8,2,4))
plot(NA, xlim=c(0,365), ylim=c(0,1), xlab='',ylab='',xaxt='n', yaxt='n', bty='n')



par(new=T)
tmp[YEAR==2008, barplot(PR, border=colList[1], ylim =c(300,0), 
                        xaxt='n', yaxt='n', bty='n')]
axis(side = 4, at = c(0,50), 
     labels = c(0,50), col = colList[1])
mtext(text = 'Precipitation (mm/day)', col = colList[1], font = 2,
      side = 4, cex = 1.2, line = 2.5)



par(new=T)
tmp[YEAR==2008, plot(DOY, SM, type='l', col=colList[3], lty=2,
                     xlab='',ylab='', lwd=2, ylim=c(0,35), 
                     xaxt='n', yaxt='n', bty='n')]
axis(side = 2, line = 0, col = colList[3], col.ticks = colList[3])
mtext(text = 'Soil moisture (%)', col = colList[3], font = 2,
      side = 2, cex = 1.2, line = 2.5)



par(new=T)
tmp[YEAR==2008, plot(DOY, TR, type='l', col=paste0(colList[2],'bb'), 
                     xlab='',ylab='', lwd=2,ylim=c(0,5),
                     xaxt='n', yaxt='n', bty='n')]
axis(side = 2, line = 5, col = colList[2], col.ticks = colList[2])

mtext(text = 'Transpiration (mm/day)', side = 2, cex = 1.2, font = 2,
      line = 7, col = colList[2])

legend(270, 4.5, legend = c('Transpiration','Precipitation','Soil Moisture'),
       lty = c(1,1,2), col=colList[c(2,1,3)], bty='n', cex=1.1, lwd=3)

axis(side = 1, line = -1, at = seq(0,350, 50))
mtext('Day of year', side = 1, line = 1.5, cex = 1.4, font=2)

dev.off()

