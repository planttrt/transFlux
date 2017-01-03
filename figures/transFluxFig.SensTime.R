source('~/Projects/procVisData/dataViz.R')

beta <- t(apply(bothSites$pk$out$beta, c(1,2), mean))/sd(bothSites$pk$y)
colnames(beta) <- colnames(bothSites$pk$x)
          
# str(beta)

# sensXVec <- transData[Year==2008&Site=='PK', .(YearCon, Solar=Rg, TA)]
sensXVec <- transData[Site=='PK', .(Solar=mean(Rg, na.rm = T),TA=mean(TA, na.rm = T)), DOY][order(DOY)]
sensXVec <- na.omit(sensXVec)


sensSolar <- as.matrix(sensXVec[, .(Solar)])%*%matrix(beta[,"Solar.dT"], nrow = 1)
sensTA <- as.matrix(sensXVec[, .(TA)])%*%matrix(beta[,"TA.dT"], nrow = 1)
sensFix <- t(replicate(beta[,"dT"], n = nrow(sensTA)))

sensTime <- sensTA + sensSolar + sensFix

sensSolar.quant <- t(apply(sensSolar, 1, quantile, probs=c(.5,.025,.975,.25,.75), na.rm=T))
sensTA.quant <- t(apply(sensTA, 1, quantile, c(.5,.025,.975,.25,.75), na.rm=T))
sensTime.quant <- t(apply(sensTime, 1, quantile, c(.5,.025,.975,.25,.75), na.rm=T))

colnames(sensSolar.quant ) <- paste0('sensSolar', colnames(sensSolar.quant))
colnames(sensTA.quant ) <- paste0('sensTA', colnames(sensTA.quant))
colnames(sensTime.quant ) <- paste0('sensTime', colnames(sensTime.quant))

sensMat <- cbind(sensXVec, sensSolar.quant, sensTA.quant, sensTime.quant)





png('figures/transFluxFig.SensTime.png', width = 4, height = 7, units = 'in',res = 150)
par(mfrow=c(3,1), mar=c(.5,0,0,0), oma=c(4,5,1,1))
# wNA <- which(is.na(rowMeans(sensTime)))
ytime <- sensMat$DOY
quant <- sensTime.quant

plot(ytime, quant[,1], type='l', lty=1, lwd=2,
     ylim=c(-.14,-.04),xaxt='n',
     xlab='', ylab = '')
polygon(x = c(ytime, rev(ytime)),
        y = c(quant[,2], rev(quant[,3])),
        # y = c(quant[,4], rev(quant[,5])),
        border = NA, col = '#8888aa88')
abline(h=0, lwd=2, lty=2, col='#0066cc')
mtext('Total effect', side = 3, line = -1.5, font = 2, adj = 0.05, col='#444444')

quant <- sensSolar.quant
plot(ytime, quant[,1], type='l', lty=1, lwd=2,
     ylim=c(-.01,.11),xaxt='n',
     xlab='', ylab = '')
polygon(x = c(ytime, rev(ytime)),
        y = c(quant[,2], rev(quant[,3])),
        # y = c(quant[,4], rev(quant[,5])),
        border = NA, col = '#8888aa88')
abline(h=0, lwd=2, lty=2, col='#0066cc')
mtext('Solar radiation effect', side = 3, line = -1.5, font = 2, adj = 0.05, col='#444444')

quant <- sensTA.quant
plot(ytime, quant[,1], type='l', lty=1, lwd=2,
     ylim=c(-.10,.02),
     xlab='', ylab = '')
polygon(x = c(ytime, rev(ytime)),
        y = c(quant[,2], rev(quant[,3])),
        # y = c(quant[,4], rev(quant[,5])),
        border = NA, col = '#8888aa88')
abline(h=0, lwd=2, lty=2, col='#0066cc')
mtext('Air temperature effect', side = 3, line = -1.5, font = 2, adj = 0.05, col='#444444')

mtext('Day of year', side = 1, line = 2.5, font=2, outer = T, cex=1.3)
mtext('Sensitivity to dT', side = 2, line = 3, font=2, outer = T, cex=1.3)

dev.off()



