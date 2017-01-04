source('~/Projects/procVisData/dataViz.R')
library(data.table)

png('figures/transFluxFig.FluxDiurnal.png', res=150, width = 8, height = 6, units = 'in')
layout(matrix(c(1,1,2,2, 1,1,2,2, 3,3,3,3), 4,3))
par(mar=c(1,1,1,3), oma=c(4,5,1,1))
# par(mfcol=c(2,2))
transData.Orig$bwField[Year==2011&DOY%in%(225:240), 
                       plot(DOY+-(Hour-1)/24, TR, type = 'l',cex.axis=1.7,
                            ylim = c(0,10), xlim=c(225,240),
                            col=1, xlab='', ylab = '', xaxt='n')]
# mtext('Transpiration (mm/day)', side = 2, font=2, line = 3, cex=1.2)
mtext(text = 'BW, 2011', font = 2, cex=2.5, adj = .05, col='#777777', line = -3)
polygon(c(228,229,229,228), c(0,0,7.5,7.5), border = '#ff5e00', lwd=3, lty=2)

transData.Orig$pkField[YEAR==2008&DOY%in%(225:245), 
                       plot(DOY+(HR-1)/24, TR, type = 'l',cex.axis=1.7,
                            ylim = c(0,10), xlim=c(225,240),
                            col=1, xlab='', ylab = '')]
# mtext('Transpiration (mm/day)', side = 2, font=2, line = 3, cex=1.2)
mtext('Day of year', side = 1, font=2, line = 3, cex=1.7)
mtext(text = 'PK, 2008', font = 2, cex=2.5, adj = .05, col='#777777', line = -3)
polygon(c(228,229,229,228), c(0,0,7.5,7.5), border = '#ff5e00', lwd=3, lty=2)

transData.Orig$bwField[Year==2011&DOY%in%(228), 
                       plot(Hour, TR, type = 'l',lwd=5,
                            ylim = c(0,10), xlim=c(5,23),cex.axis=1.7,
                            col='grey', xlab='', ylab = '', xaxt='s', yaxt='s')]
# mtext(text = 'August 15th', font = 2, cex=2, adj = .05, col='#777777', line = -2.5)
transData.Orig$pkField[YEAR==2008&DOY%in%(228), 
                       lines(HR, TR, type = 'l', lwd=5, lty=2,
                            ylim = c(0,10), xlim=c(5,23), 
                            col='black', xlab='', ylab = '', yaxt='n')]

mtext('Transpiration (mm/day)', side = 2, font=2, line = 2.5, cex=1.5, outer =T)
mtext('Hours', side = 1, font=2, line = 3, cex=1.7)
mtext(text = 'Day 228', font = 2, cex=2, adj = .5, col='#777777', line = -2.5)
legend(y=9, x=14,legend = c('BW','PK'), lty=c(1,2), lwd=5, col=c('grey','black'), 
       bty='n', cex=2.5, xjust = .5, yjust = .7, text.font = 2)
dev.off()
