source('~/Projects/procVisData/dataViz.R')
source('~/Projects/procVisData/colorSet.R')
source('~/Projects/procVisData/colorProc.R')



y <- bothSites$pk$y
yp <- bothSites$pk$yp
t <- list(ydh=bothSites$pk$ydh)
t$y <- floor(t$ydh/100000)
t$d <- floor((t$ydh%%100000)/100)
t$h <- t$ydh%%100
ytime <- t$y+ t$d/(365+(t$y%%4==0)*1) + t$h/(365+(t$y%%4==0)*1)/24

set.seed(1001)
w <- sample(1:length(y), size = 20)

png('figures/transFluxFig.Predict.png', width = 4, height = 8, units = 'in',res = 150)
par(mfrow=c(2,1), mar=c(4,4,1,1))
plotObsPred(y, rowMeans(yp), xlim = c(0,8), ylim=c(0,8), 
            xlab = '',
            ylab = 'Predicted TR (mm/day)')

# colList <- paste0(colList.brownGreen,'99')
# col <- rep(colList[1], length(y))
# col[w] <- colList[5]
# pch <- rep(17, length(y))
# pch[w] <- 19
# points(y, rowMeans(yp), col=col, pch=pch, cex=.8)

abline(0,1, col='#2255aa', lwd=2, lty=2)
title('in-sample')

plot(y[w], rowMeans(yp)[w], xlim = c(0,8), ylim=c(0,8), pch=15, 
     xlab = 'Observed TR (mm/day)',
     ylab = 'Predicted TR (mm/day)')
abline(0,1, col='#2255aa', lwd=2, lty=2)
title('out-of-sample')
dev.off()



png('figures/transFluxFig.PredictTime.png', width = 8, height = 4, units = 'in',res = 150)
par( mar=c(5,4,2,1))
yp.quant <- apply(yp, 1, quantile, c(0.5, .025, .975, .25,.75))

plot(ytime, y, type='l', lty=2,
     xlim = c(2007.5, 2008.8),
     xlab='', ylab = '')
mtext('Time (year)',side = 1, font=2, line = 3)
win <- par('usr')
text(label='Transpiration (mm/day)', xpd=T, font=2,
     x=mean(win[1])-.12, y=mean(win[3:4]), srt=270)

# lines(ytime, yp.quant[2,], col='#888888')
# lines(ytime, yp.quant[3,], col='888888')
polygon(x = c(ytime, rev(ytime)),
        y = c(yp.quant[2,], rev(yp.quant[3,])),
        border = NA,
        col = '#8888aa88')

# polygon(x = c(ytime, rev(ytime)), 
#         y = c(yp.quant[4,], rev(yp.quant[5,])),
#         border = NA,
#         col = '#8888aa88')
lines(ytime, yp.quant[1,], col='#ff000088')
legend('topright', legend = c('Predicted','Observed'), 
       col = c('red', 'black'), lty=c(1,2), bty='n', lwd=3)

dev.off()


RMSE <- rmse(y, yp.quant[1,])

error <- mean(abs(yp.quant[1,]-y))
