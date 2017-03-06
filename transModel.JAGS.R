source('~/Projects/procVisData/dataViz.R')
library(data.table)

if(!exists('transDataLoaded'))source('~/Projects/transFlux/transPreModel.R')

xydata <- transData[,
                    .(Intercept=1,
                      Site, 
                      ydh,
                      BW=(Site=='BW')*1,
                      dT=1*(LST-TA), 
                      TA,
                      LST,
                      Solar=Rg, 
                      Thermal= -0.96*(LST+273)^4 + 0.75*(TA+273)^4,
                      LST4 = (LST+273)^4,
                      TA4 = (TA+273)^4,
                      VPD,
                      windy = (!is.na(WS)&WS > 2)*1,
                      rainy = (!is.na(PR)&PR > 0)*1,
                      cloud = log(Rg/RgOpen),
                      TR)]
xydata[,Year:=floor(ydh/100000)]
xydata[,DOY:=floor(ydh/100)%%1000]
xydata[,Hour:=ydh%%100]
xydata
xydata[Year==2011,plot(DOY,Thermal)]
par(new=T)
xydata[Year==2011,plot(DOY,TR, col=2)]

# xydata <- na.omit((xydata[Site=='PK', ]))

modelET4Site <- function(site='PK'){
  xydata <- na.omit(xydata[Site==site])
  x <- as.matrix(xydata[,.(Intercept, 
                           dT, 
                           Solar, 
                           # TA4,
                           LST4,
                           # Thermal,
                           TA.dT = dT*TA,
                           Solar.dT = dT*Solar,
                           windy.dT=-dT*windy, 
                           # cloud.dT=dT*cloud, 
                           rainy.dT=dT*rainy
  )])
  y <- as.numeric(unlist(as.vector(xydata[,.(TR)])))
  
  out <- lmJAGS(x,y)
  
  yp <- apply(out$ypred, 1:2, mean)
  
  betaStd <- t(apply(out$beta, c(1,2), mean)*apply(x, 2, sd)/sd(y))
  colnames(betaStd) <- colnames(x)
  sigmaStd <-  t(apply(out$sigma, c(1,2), mean)/sd(y))
  
  list(out=out, x=x, y=y, yp=yp, ydh= xydata$ydh, betaStd=betaStd, sigmaStd=sigmaStd)
}


pkSite <- modelET4Site('PK')
bwSite <- modelET4Site('BW')

plotGibbsBoxplots(pkSite$betaStd, textAdj = 0, sigPlot = F, sort = F)

bothSites <- list(bw=bwSite, pk=pkSite)

 # save(list = c('bothSites','xydata','transData', 'transData.Orig'), file = 'bothSites2.Rdata')

# plotObsPred(y, rowMeans(yp), xlim = c(0,10), ylim=c(0,10))
# abline(0,1, col='#55ccaa', lwd=2, lty=2)
# 
# colList <- colList.brownGreen
# col <- rep(colList.brownGreen[1], length(y))
# col[x[,2]==1] <- colList.brownGreen[5]
# pch <- rep(17, length(y))
# pch[x[,2]==1] <- 19
# points(y, rowMeans(yp), col=col, pch=pch, cex=.5 )
# 
# summary(out2$betaStd)
# summary(out2$simaStd)
colnames(bothSites$pk$x)

bg <- t(apply(bothSites$pk$out$beta, c(1,2), mean))
colnames(bg) <- c('Intercept','ΔT','Solar','Thermal','Tair:ΔT','Solar:ΔT','windy:ΔT','rainy:ΔT')
bg.bw <- cbind(rowMeans(bothSites$pk$out$beta[1,,]-bothSites$bw$out$beta[1,,]),
               rowMeans(bothSites$bw$out$beta[3,,]-bothSites$pk$out$beta[3,,]),
               rowMeans(bothSites$pk$out$beta[4,,]-bothSites$bw$out$beta[4,,]))
colnames(bg.bw) <- c('BW','Solar:BW','Thermal:BW')
bg <- cbind(bg, bg.bw)

# betasum0 <- data.frame(beta = c('Intercept','ΔT','Solar','Thermal','Tair:ΔT','Solar:ΔT','windy:ΔT','rainy:ΔT'), 
#                        mean = apply(bothSites$pk$out$beta, 1, mean),
#                        std = apply(bothSites$pk$out$beta, 1, sd))
# betasum1 <- data.frame(beta = c('BW'), 
#                        mean = mean(bothSites$pk$out$beta[1,,]-bothSites$bw$out$beta[1,,]),
#                        std = sd(bothSites$pk$out$beta[1,,]-bothSites$bw$out$beta[1,,]))
# 
# betasum2 <- data.frame(beta = c('Solar:BW'), 
#                        mean = mean(bothSites$bw$out$beta[3,,]-bothSites$pk$out$beta[3,,]),
#                        std = sd(bothSites$bw$out$beta[3,,]-bothSites$pk$out$beta[3,,]))
# betasum3 <- data.frame(beta = c('Thermal:BW'), 
#                        mean = mean(bothSites$pk$out$beta[4,,]-bothSites$bw$out$beta[4,,]),
#                        std = sd(bothSites$pk$out$beta[4,,]-bothSites$bw$out$beta[4,,]))
# betasum <- rbind(betasum0, betasum1, betasum2, betasum3)
betasum <- rbind(apply(bg, 2, mean),
                 apply(bg, 2, quantile, c(.5,.025,.0975)),
                 apply(bg, 2, sd))
rownames(betasum) <- c('mean','median','2.5%','97.5%','std')
betasum <- t(betasum)
write.csv(signif(betasum, 2), 'betasum.csv')
