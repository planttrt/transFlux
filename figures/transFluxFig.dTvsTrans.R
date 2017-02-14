library(ggplot2)
library(data.table)

X <- bothSites$pk$x
Tr <- bothSites$pk$y
beta <- apply(bothSites$pk$out$beta, 1, mean)
names(beta) <- colnames(X)

source('~/Projects/procVisData/dataViz.R')
xbreaks = c(-10,-5,-2,2,5,10)
statsParam = c(.05,.25,.5,.75,.95)

xx <- X[,"dT"]
yy1 <- Tr
yy2 <- Tr -
  X[,"Intercept"]*beta["Intercept"] -
  X[,"Solar"]*beta["Solar"] -
  X[,"LST4"]*beta["LST4"] -
  X[,"TA.dT"]*beta["TA.dT"] -
  X[,"Solar.dT"]*beta["Solar.dT"] -
  X[,"windy.dT"]*beta["windy.dT"] -
  X[,"rainy.dT"]*beta["rainy.dT"]

DT <- data.table(ΔT=xx, Tr = yy1, Tr.Effect=yy2, 
                  Solar=X[,"Solar"], Thermal = X[,"LST4"])
g <-  ggplot(DT, aes(ΔT, Tr)) 

# par(mar=c(3,3,1,1))
# par(mfrow=c(2,1))
# plotObsPred(xx, yy1, nbin = 6)
# plotObsPred(xx, yy2+4, nbin = 6, ylim = c(0,9))
# bp <- xyBinPlot(xx, yy2, statsParam = statsParam, xbreaks = xbreaks, showOutliers = F)

png('figures/transFluxFig.dTvsTrans.png', width = 6, height = 6, res = 300, units = 'in')
par(mar=c(4,4,2,2))
bp <- xyBinPlot(xx, yy1, statsParam = statsParam, xbreaks = xbreaks, showOutliers = F)
mtext('ΔT (°C)', side = 1, line = 2.5, cex = 1.5, font = 2)
mtext('Transpiration (mm/day)', side = 2, line = 2.5, cex = 1.5, font = 2)
dev.off()

